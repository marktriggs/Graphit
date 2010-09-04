(ns graphit
  (:import (org.jfree.chart ChartPanel JFreeChart ChartFactory)
           (org.jfree.data.xy XYSeries XYSeriesCollection)
           (org.jfree.chart.plot PlotOrientation)
           (org.jfree.chart.axis NumberAxis)
           (org.jfree.chart.labels XYToolTipGenerator)
           (org.jfree.ui ApplicationFrame)
           (java.io BufferedReader PrintWriter File)
           (java.text NumberFormat DecimalFormat SimpleDateFormat)
           (java.util Date)
           (javax.swing JFrame JPanel JTextField BoxLayout Box
                        JLabel JMenuItem JOptionPane JButton SwingUtilities
                        SwingConstants JCheckBox BorderFactory JSeparator)
           (java.net ServerSocket Socket)
           (java.awt.event ActionListener ItemListener ItemEvent WindowAdapter)
           (java.awt BasicStroke Dimension Color BorderLayout FlowLayout))
  (:use clojure.contrib.str-utils
        clojure.contrib.duck-streams
        clojure.contrib.command-line)
  (:require tabpane)

  (:gen-class :name GraphIt
              :main true))


;; Thread-shared vars
(def *redraw-delay-ms* (atom 10000))

(def *data-gatherer* (agent []))
(def *expire-threshold* (atom (Integer/MAX_VALUE)))
(def *max-readings* (atom 120))
(def *graphs-per-page* (atom 4))

(def *tab-cycle-active* (atom false))
(def *tab-cycle-delay* (atom 30))

(def *graphs* (atom {}))
(def *hide-legend* (atom nil))


;; Objects used for interruptible sleeps
(def *plot-alarm* (Object.))
(def *tab-cycle-alarm* (Object.))


;;; Misc utilities

(defn interruptible-sleep [ms alarm]
  (locking alarm
    (.wait alarm ms)))

(defn interrupt-sleep [alarm]
  (locking alarm
    (.notify alarm)))


;;; Ah, swing...

(def *window*
     {:frame (JFrame.)
      :panel (tabpane/tabpane *graphs-per-page*)})


(defn set-rate
  "Set the value of `atom' to `n' and interrupt `alarm' to wake up its owner."
  [atom n alarm]

  (reset! atom n)
  (interrupt-sleep alarm))


(defn parse-time [time timefmt]
  (if (string? time)
    (if timefmt
      (.. (SimpleDateFormat. timefmt)
          (parse time)
          getTime)
      (BigDecimal. time))
    time))


(defn parse-datapoint [#^String s]
  (let [[graph & bits] (.split s "\t")
        [time label num timefmt] (if (= (count bits) 2)
                                   (concat [(System/currentTimeMillis)] bits)
                                   bits)]
    {:graph graph
     :time (parse-time time timefmt)
     :line label
     :value (if (= num "delete")
              num
              (.parse (NumberFormat/getInstance) num))}))


(defn add-datapoint [point]
  (send *data-gatherer* conj point))


(defmacro print-exceptions [& body]
  `(try ~@body
        (catch Exception e# (.printStackTrace e#))))


(def *number-formatter* (proxy [DecimalFormat] []
                          (format [a b c]
                            (let [formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
                              (if (>= a (.getTime (.parse formatter "1990-1-1 00:00:00")))
                                (.append b (.format formatter (Date. (long a))))
                                (proxy-super format a b c))))))


(defn dump-dataset [wrtr dataset graph-name]
  (doseq [i (range 0 (.getSeriesCount dataset))]
    (let [key (.getSeriesKey dataset i)
          series (.getSeries dataset key)]
      (doseq [data-item (.getItems series)]
        (.println wrtr
                  (format "%s\t%f\t%s\t%f"
                          graph-name
                          (double (.getX data-item))
                          key
                          (double (.getY data-item))))))))


(defn dump-state [filename]
  (with-open [fh (writer filename)]
    (doseq [[name graph] @*graphs*]
      (dump-dataset fh (:dataset graph) name))
    (.flush fh)))


(def *tooltip-generator*
     (proxy [XYToolTipGenerator] []
       (generateToolTip [dataset series item]
         (format "%s, %s, %s"
                 (.getSeriesKey dataset series)
                 (.format *number-formatter*
                          (.getXValue dataset series item))
                 (String/valueOf (.getYValue dataset series item))))))


(defn make-chart [title y-axis dataset]
  (let [linechart (ChartFactory/createXYLineChart title
                                                  ""
                                                  y-axis
                                                  dataset
                                                  PlotOrientation/VERTICAL
                                                  true true false)]
    (when @*hide-legend*
      (.setVisible (.getLegend linechart)
                   false))
    (.setAntiAlias linechart true)

    ;; Initialisation gumpf
    (let [xyplot (.getPlot linechart)]
      (doto xyplot
        (.setDomainPannable true)
        (.setRangePannable true)
        (.setDomainZeroBaselineVisible true)
        (.setRangeZeroBaselineVisible true)
        (.setBackgroundPaint (Color. 239 239 239)))

      (doto (.getRenderer xyplot)
        (.setBaseToolTipGenerator *tooltip-generator*)
        (.setStroke (BasicStroke. 2.0))
        (.setBaseShapesVisible true)
        (.setBaseShapesFilled true)
        (.setOutlineStroke (BasicStroke. 3.0))
        (.setDrawOutlines true))

      (doto (.getRangeAxis xyplot)
        (.setAutoRangeIncludesZero false)
        (.setStandardTickUnits (NumberAxis/createIntegerTickUnits)))

      (doto (.getDomainAxis xyplot)
        (.setNumberFormatOverride *number-formatter*)))

    (doto (ChartPanel. linechart)
      (.setInitialDelay 50)
      (.setMouseWheelEnabled true))))


(defn hide-graph
  ([graphname] (hide-graph graphname false))
  ([graphname force]
     (when (or force (> (tabpane/child-count (:panel *window*)) 1))
       (let [chart (:chart (@*graphs* graphname))]
         (tabpane/remove-panel (:panel *window*) chart)))))


(defn hide-all-graphs
  ([] (hide-all-graphs false))
  ([force]
     (doseq [graph (keys @*graphs*)]
       (hide-graph graph force))))


(defn show-graph [graphname]
  (hide-graph graphname true)
  (let [chart (:chart (@*graphs* graphname))]
    (tabpane/add-panel (:panel *window*) chart graphname)))


(defn show-all-graphs []
  (doseq [graph (keys @*graphs*)]
    (show-graph graph)))


(defn remove-graph
  "Removes 'graphname' from the display."
  [graphname]
  (when (= (tabpane/child-count (:panel *window*)) 1)
    ;; show any other available graphs...
    (show-all-graphs))
  (hide-graph graphname true)
  (swap! *graphs* dissoc graphname))


(defn item-listener [fn]
  (proxy [ItemListener] []
    (itemStateChanged [e] (fn e))))

(defn action-listener [fn]
  (proxy [ActionListener] []
    (actionPerformed [e] (try (fn e)
                          (catch Exception _)))))


(defn menu-item [label f]
  (doto (JMenuItem. label)
    (.setActionCommand "PROPERTIES")
    (.addActionListener (action-listener f))))


;; A terrible hack ;o)
(defn instrument-popup-menu [name]
  (let [chart (:chart (@*graphs* name))
        field (.getDeclaredField (class chart) "popup")]
    (.setAccessible field true)
    (doto (.get field chart)
      (.addSeparator)
      (.add (menu-item "Show only this graph"
                       (fn [_]
                         (hide-all-graphs true)
                         (show-graph name))))
      (.add (menu-item "Hide this graph"
                       (fn [_] (hide-graph name))))
      (.add (menu-item "Show all graphs"
                       (fn [_] (show-all-graphs))))
      (.addSeparator)
      (.add (menu-item "Delete this graph"
                       (fn [_]
                         (when (= (JOptionPane/showConfirmDialog
                                   (:frame *window*)
                                   "Really delete?")
                                  JOptionPane/YES_OPTION)
                           (remove-graph name))))))))


(defn series-seq [xyseries]
  (map #(.getDataItem xyseries %)
       (range (.getItemCount xyseries))))


(defn delete-line [graph-name label]
  (let [graph (@*graphs* graph-name)
        line (get-in graph [:lines label])]
    (.removeSeries (:dataset graph) line)
    (swap! *graphs* update-in
           [graph-name :lines]
           dissoc label)))


(defn do-plot [values]
  (SwingUtilities/invokeLater
   (fn []
     (print-exceptions

      (doseq [{:keys [graph time line value]} values]

        (when-not (@*graphs* graph)
          (let [dataset (XYSeriesCollection.)
                chart (make-chart graph "" dataset)]
            (tabpane/add-panel (:panel *window*) chart graph)
            (swap! *graphs* assoc graph
                   {:chart chart
                    :dataset dataset
                    :lines {}})
            (instrument-popup-menu graph)))

        (when-not (get-in @*graphs* [graph :lines line])
          (let [new-line (doto (XYSeries. line)
                           (.setMaximumItemCount @*max-readings*))]
            (swap! *graphs* update-in
                   [graph :lines]
                   assoc line new-line)

            (.addSeries (get-in @*graphs* [graph :dataset])
                        new-line)))

        (if (= value "delete")
          (delete-line graph line)
          (.add #^XYSeries (get-in @*graphs* [graph :lines line])
                #^Number time #^Number value false)))

      (doseq [[graph-name graph] @*graphs*]
        (doseq [[label line] (:lines graph)
                :let [last-reading (.getMaxX line)]]
          ;; Experimental: if any line has had *expire-threshold*
          ;; data points since this line's most recent datapoint, "expire" this
          ;; line.
          (when (some (fn [line]
                        (> (count (filter #(> (.getXValue %)
                                              last-reading)
                                          (series-seq line)))
                           @*expire-threshold*))
                      (vals (:lines graph)))
            (delete-line graph-name label))))

      (doseq [graph (vals @*graphs*)]
        (.fireSeriesChanged (first (-> graph :lines vals))))

      (send-off *data-gatherer* do-plot))))
  (interruptible-sleep @*redraw-delay-ms* *plot-alarm*)
  [])


(defn handle-client [#^Socket client]
  (print-exceptions
   (with-open [#^BufferedReader in (reader (.getInputStream client))
               #^PrintWriter out (writer (.getOutputStream client))]
     (doseq [s (take-while #(not= % "done") (line-seq in))]
       (if (= s "help")
         (do (.println out "Syntax: graph name:[xval]:line name:(yval|\"delete\")")
             (.println out "Exit with 'done'")
             (.flush out))
         (add-datapoint (parse-datapoint s))))))
  (.close client))


(defn handle-inputs
  "Open a socket and read lines of input."
  [port]
  (with-open [server (ServerSocket. port)]
    (println "Listening on" port)
    (while true
      (print-exceptions
       (let [client (.accept server)]
         (future (handle-client client)))))))


(defn save-state []
  (dump-state (File. (System/getenv "HOME") ".graphit.state")))


(defn separator []
  (doto (JPanel.)
    (.add (Box/createHorizontalStrut 5))
    (.add (doto (JSeparator. SwingConstants/VERTICAL)
            (.setPreferredSize (Dimension. 1 10))))
    (.add (Box/createHorizontalStrut 5))))


(defn make-control-panel []
  (doto (JPanel.)
    ;; Redraw rate adjustment
    (.add (JLabel. "Redraw rate:"))
    (.add (doto (JTextField.)

            (.addActionListener (action-listener
                                 #(set-rate
                                   *redraw-delay-ms*
                                   (Integer. (.getActionCommand %))
                                   *plot-alarm*)))
            (.setColumns 6)
            (.setText (str @*redraw-delay-ms*))))
    (.add (JLabel. "ms"))

    (.add (separator))

    ;; Tab cycling adjustment
    (.add (doto (JCheckBox.)
            (.addItemListener
             (item-listener
              #(try
                (reset! *tab-cycle-active*
                        (= (.getStateChange %) ItemEvent/SELECTED))
                (catch Exception _))))))
    (.add (JLabel. "Cycle tabs every "))
    (.add (doto (JTextField. nil (str @*tab-cycle-delay*) 3)
            (.addActionListener
             (action-listener
              #(set-rate *tab-cycle-delay*
                         (Integer. (.getActionCommand %))
                         *tab-cycle-alarm*)))))
    (.add (JLabel. " secs"))

    (.add (separator))

    ;; Graphs per page
    (.add (JLabel. "Show "))
    (.add (doto (JTextField. nil (str @*graphs-per-page*) 3)
            (.addActionListener
             (action-listener
              #(do
                 (reset! *graphs-per-page*
                         (Integer. (.getActionCommand %)))
                 (tabpane/rebalance (:panel *window*)))))))
    (.add (JLabel. " graphs/page"))
    (.setLayout (doto (FlowLayout.)
                  (.setAlignment FlowLayout/LEFT)))))


(defn make-status-bar []
  (doto (JPanel.)
    (.setPreferredSize (Dimension. 15 33))
    (.setBorder (BorderFactory/createEmptyBorder 5 5 5 5))
    (.setLayout (BorderLayout.))
    (.add (make-control-panel) BorderLayout/WEST)
    (.add (doto (JButton. "Dump points")
            (.addActionListener (action-listener
                                 (fn [_]
                                   (try (save-state)
                                        (catch Exception _))))))
          BorderLayout/EAST)))


(defn run-ui []
  (SwingUtilities/invokeLater
   (fn []
     (.setLayout (.getContentPane (:frame *window*))
                 (BorderLayout.))
     (doto (:frame *window*)
       (.addWindowListener
        (proxy [WindowAdapter] []
          (windowClosed [e] (save-state))))
       (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
       (.add (make-status-bar) BorderLayout/NORTH)
       (.add (tabpane/get-panel (:panel *window*)))
       (.setSize (Dimension. 1280 1024))
       (.setVisible true)))))


(defn -main [& args]
  (with-command-line args
    "A handy graphing thingy"
    [[max-to-keep "Maximum points to keep per line" "120"]
     [expire-threshold
      "The number of points a line can fall behind other lines before being expired."
      nil]
     [hide-legend "Don't display the graph's legend"]
     [graphs-per-page "Number of graphs per tabbed page" "4"]
     [port "Listen port" "6666"]
     [redraw "Redraw ms" "2000"]]

    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. #(save-state)))

    (set-rate *redraw-delay-ms* (Integer. redraw) *plot-alarm*)
    (when expire-threshold
      (reset! *expire-threshold* (Integer. expire-threshold)))
    (reset! *max-readings* (Integer. max-to-keep))
    (reset! *hide-legend* hide-legend)
    (reset! *graphs-per-page* (Integer. graphs-per-page))
    (future (handle-inputs (Integer. port)))

    (send-off *data-gatherer* do-plot)
    (future
     (while true
       (when @*tab-cycle-active*
         (tabpane/cycle-tab (:panel *window*)))
       (interruptible-sleep (* @*tab-cycle-delay* 1000) *tab-cycle-alarm*)))
    (run-ui)))
