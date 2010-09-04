(ns tabpane
  (:import (javax.swing JPanel JTabbedPane BoxLayout SwingUtilities)
           (java.awt Dimension))
  (:use clojure.contrib.seq-utils))

(defn tabpane [page-size]
  (let [panel (JTabbedPane.)]
    {:panel panel
     :page-size page-size
     :subpanels (atom #{})}))


(defn get-panel [tabpane]
  (:panel tabpane))


(defn child-count [tabpane]
  (count @(:subpanels tabpane)))


(defn rebalance [tabpane]
  (.removeAll (get-panel tabpane))
  (dorun
   (map (fn [idx panels]
          (let [tab (format "Group %d" idx)
                container (JPanel.)]
            (doto container
                    (.setLayout
                     (BoxLayout. container BoxLayout/PAGE_AXIS)))

            (doseq [[p name] panels]
              (.add container p))

            (.add (get-panel tabpane)
                  tab
                  container)))
        (range 1 (inc (count @(:subpanels tabpane))))
        (partition-all @(:page-size tabpane)
                       (sort-by second @(:subpanels tabpane)))))
  (.revalidate (get-panel tabpane)))


(defn add-panel [tabpane panel name]
  (SwingUtilities/invokeLater
   (fn []
     (swap! (:subpanels tabpane) conj [panel name])
     (rebalance tabpane))))


(defn remove-panel [tabpane panel]
  (SwingUtilities/invokeLater
   (fn []
     (swap! (:subpanels tabpane)
            (fn [subpanels]
              (remove #(= (first %) panel)
                      subpanels)))
     (rebalance tabpane))))


(defn cycle-tab [tabpane]
  (SwingUtilities/invokeLater
   (fn []
     (when (> (.getTabCount (get-panel tabpane))
              1)
       (.setSelectedIndex (get-panel tabpane)
                          (mod (inc (.getSelectedIndex (get-panel tabpane)))
                               (.getTabCount (get-panel tabpane))))))))
