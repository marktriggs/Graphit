(ns multi-frame
  (:use clojure.contrib.str-utils)
  (:import (org.jdesktop.swingx MultiSplitPane MultiSplitLayout)
           (javax.swing JFrame JButton)
           (java.awt Dimension)))


(defn multi-frame []
  (agent {:pane (MultiSplitPane.)
          :stuff []}))


(defn component-count [mf]
  (count (:stuff @mf)))

(defn- layout-string [mf]
  (let [cnt (Math/max (count (:stuff mf)) 2)
        weight (/ 1.0 cnt)]
    (format "(COLUMN %s)"
            (str-join " " (map #(format "(LEAF name=elt%d weight=%.2f)"
                                        % weight)
                               (range cnt))))))


(defn- rejig [mf]
  (.removeAll (:pane mf))
  (.setModel (.getMultiSplitLayout (:pane mf))
             (MultiSplitLayout/parseModel (layout-string mf)))
  (doseq [[i thing] (map vector (iterate inc 0) (:stuff mf))]
    (.add (:pane mf) thing (format "elt%d" i)))
  (.revalidate (:pane mf))
  mf)


(defn add-to-frame [mf thing]
  (rejig (update-in mf [:stuff] conj thing)))


(defn remove-from-frame [mf thing]
  (rejig (update-in mf [:stuff] (fn [components]
                                  (remove #(= % thing) components)))))
