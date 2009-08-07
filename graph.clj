(ns graph
  (:import (org.jfree.chart ChartPanel JFreeChart ChartFactory)
           (org.jfree.data.xy XYSeries XYSeriesCollection)
           (org.jfree.chart.plot PlotOrientation)
           (org.jfree.chart.axis NumberAxis)
           (org.jfree.chart.labels XYToolTipGenerator)
           (org.jfree.ui ApplicationFrame)
           (java.io BufferedReader PrintWriter File)
           (java.text NumberFormat DecimalFormat SimpleDateFormat)
           (java.util Date)
           (javax.swing JFrame JPanel JTextField BoxLayout
                        JLabel JMenuItem JOptionPane)
           (java.net ServerSocket Socket)
           (java.awt.event ActionListener WindowAdapter)
           (java.awt BasicStroke Dimension Color BorderLayout FlowLayout))
  (:use clojure.contrib.str-utils
        clojure.contrib.duck-streams
        clojure.contrib.command-line
        swank)
  (:gen-class :name GraphIt
              :main true))


;; Config bits
(def *redraw-delay-ms* nil)


;; Thread-shared vars
(def *server-port* nil)
(def *data-gatherer* (agent []))
(def *max-readings* nil)
(def *graphs* (atom {}))
(def *frame* (JFrame.))
(def *status-bar* (JPanel.))
(def *refresh-rate* (JTextField.))
(def *panel* (JPanel.))


(defn set-refresh-rate [n]
  (alter-var-root #'*redraw-delay-ms* (fn [_] n)))


(defn parse-datapoint [#^String s]
  (let [[graph & bits] (.split s ":")]
    (let [[time label num] (if (= (count bits) 2)
                             (concat [(System/currentTimeMillis)] bits)
                             (cons (BigDecimal. #^String (first bits))
                                   (rest bits)))]
      {:graph graph
       :time time
       :line label
       :value (.parse (NumberFormat/getInstance) num)})))


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
                  (format "%s:%f:%s:%f"
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
      (.setInitialDelay 200)
      (.setMouseWheelEnabled true)
      (.setPreferredSize (Dimension. 800 600)))))


(defn hide-graph
  ([graphname] (hide-graph graphname false))
  ([graphname force]
     (when (or force (> (.countComponents *panel*) 1))
       (let [chart (:chart (@*graphs* graphname))]
         (.remove *panel* chart)
         (.revalidate *panel*)))))


(defn hide-all-graphs
  ([] (hide-all-graphs false))
  ([force]
     (doseq [graph (keys @*graphs*)]
       (hide-graph graph force))))


(defn show-graph [graphname]
  (hide-graph graphname true)
  (let [chart (:chart (@*graphs* graphname))]
    (.add *panel* chart)
    (.revalidate *panel*)))


(defn show-all-graphs []
  (doseq [graph (keys @*graphs*)]
    (show-graph graph)))


(defn remove-graph [graphname]
  "Removes 'graphname' from the display."
  (when (= (.countComponents *panel*) 1)
    ;; show any other available graphs...
    (show-all-graphs))
  (hide-graph graphname)
  (swap! *graphs* dissoc graphname))


(defn action-listener [fn]
  (proxy [ActionListener] []
    (actionPerformed [e] (fn e))))


;; A terrible hack ;o)
(defn instrument-popup-menu [name]
  (let [chart (:chart (@*graphs* name))
        field (.getDeclaredField (class chart) "popup")]
    (.setAccessible field true)
    (doto (.get field chart)
      (.addSeparator)
      (.add (doto (JMenuItem. "Show only this graph")
              (.setActionCommand "PROPERTIES")
              (.addActionListener (action-listener (fn [_]
                                                     (hide-all-graphs true)
                                                     (show-graph name))))))
      (.add (doto (JMenuItem. "Hide this graph")
              (.setActionCommand "PROPERTIES")
              (.addActionListener (action-listener (fn [_]
                                                     (hide-graph name))))))
      (.add (doto (JMenuItem. "Show all graphs")
              (.setActionCommand "PROPERTIES")
              (.addActionListener (action-listener (fn [_]
                                                     (show-all-graphs))))))
      (.addSeparator)
      (.add (doto (JMenuItem. "Delete this graph")
              (.setActionCommand "PROPERTIES")
              (.addActionListener (action-listener
                                   (fn [_]
                                     (when (= (JOptionPane/showConfirmDialog
                                               *frame*
                                               "Really delete?")
                                              JOptionPane/YES_OPTION)
                                       (remove-graph name))))))))))


(defn do-plot [values]
  (print-exceptions

   (doseq [{:keys [graph time line value]} values]

     (when-not (@*graphs* graph)
       (let [dataset (XYSeriesCollection.)
             chart (make-chart graph "" dataset)]
         (.add *panel* chart)
         (.revalidate *panel*)
         (swap! *graphs* assoc graph
                {:chart chart
                 :dataset dataset
                 :lines {}})
         (instrument-popup-menu graph)))

     (when-not (get-in @*graphs* [graph :lines line])
       (let [new-line (doto (XYSeries. line)
                        (.setMaximumItemCount *max-readings*))]
         (swap! *graphs* update-in
                [graph :lines]
                assoc line new-line)

         (.addSeries (get-in @*graphs* [graph :dataset])
                     new-line)))

     (.add #^XYSeries (get-in @*graphs* [graph :lines line])
           #^Number time #^Number value false))

   (doseq [#^XYSeries line (for [graph (vals @*graphs*)
                                 line (-> graph :lines vals)]
                             line)]
     (.fireSeriesChanged line))

   (send-off *agent* do-plot)
   (Thread/sleep *redraw-delay-ms*))
  [])


(defn handle-client [#^Socket client]
  (print-exceptions
   (with-open [#^BufferedReader in (reader (.getInputStream client))
               #^PrintWriter out (writer (.getOutputStream client))]
     (.println out "Syntax: graph name:xval:line name:yval")
     (.println out "Exit with 'done'")
     (.flush out)
     (doseq [s (take-while #(not= % "done") (line-seq in))]
       (add-datapoint (parse-datapoint s)))))
  (.close client))



(defn handle-inputs []
  "Open a socket and read lines of input."
  (with-open [server (ServerSocket. *server-port*)]
    (println "Listening on" *server-port*)
    (while true
      (print-exceptions
       (let [client (.accept server)]
         (.start (Thread. #(handle-client client))))))))


(defn save-state []
  (dump-state (File. (System/getenv "HOME") ".graphit.state")))

(defn run-plotter []
  (doto *refresh-rate*
    (.addActionListener (action-listener
                         #(try (set-refresh-rate
                                (Integer. (.getActionCommand %)))
                               (catch Exception _))))
    (.setColumns 6)
    (.setText (str *redraw-delay-ms*)))

  (doto *status-bar*
    (.setPreferredSize (Dimension. 10 23))
    (.setLayout (doto (FlowLayout.)
                  (.setAlignment FlowLayout/LEFT)))
    (.add (JLabel. "Redraw rate:"))
    (.add *refresh-rate*)
    (.add (JLabel. "ms")))

  (doto *panel*
    (.setLayout (BoxLayout. *panel* BoxLayout/PAGE_AXIS)))

  (.setLayout (.getContentPane *frame*)
              (BorderLayout.))

  (doto *frame*
    (.addWindowListener
     (proxy [WindowAdapter] []
       (windowClosed [e] (save-state))))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.add *status-bar* BorderLayout/NORTH)
    (.add *panel*)
    (.setSize (Dimension. 1280 1024))
    (.setVisible true)))


(defn run [max-to-keep port swank-port redraw-ms]
  (let [data-handler (agent nil)]

    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. #(save-state)))

    (alter-var-root #'*redraw-delay-ms* (fn [_] (Integer. redraw-ms)))
    (alter-var-root #'*server-port* (fn [_] (Integer. port)))
    (alter-var-root #'*max-readings* (fn [_] (Integer. max-to-keep)))

    (.start (Thread. handle-inputs))
    (send-off *data-gatherer* do-plot)
    (when swank-port
      (binding [*3 nil *2 nil *1 nil *e nil]
        (swank/start-server "/dev/null" :port (Integer. swank-port))))
    (run-plotter)))


(defn -main [& args]
  (with-command-line args
      "A handy graphing thingy"
      [[max-to-keep "Maximum points to keep per line" "120"]
       [port "Listen port" "6666"]
       [swank-port "Swank listen port"]
       [redraw "Redraw ms" "2000"]]
    (run max-to-keep port swank-port redraw)))
