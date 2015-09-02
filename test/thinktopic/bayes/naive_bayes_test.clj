(ns thinktopic.bayes.naive-bayes-test
  (:require [clojure.core.matrix :as mat]
            [thinktopic.bayes.naive-classifier :refer [gaussian-naive-classifier predict-all 
                                                       prediction-accuracy normalized-class-probabilities
                                                       class-probabilities]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def TRAIN-TEST-SPLIT 0.67)

(defn diabetes-data
  "
  https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes"
  []
  (with-open [in-file (io/reader "resources/pima-indians-diabetes.csv")]
    (let [lines (csv/read-csv in-file)
          num-lines (map #(into [] (map read-string %)) lines)]
      (into [] num-lines))))

(defn split-dataset
  "Split the data into [train, test] sets with train having ratio % of the data."
  [data ratio]
  (split-at (int (* (count data) ratio))
            data
            ;(shuffle data)
            ))

(defn diabetes-class-data
  "Split into classes and remove the last field which is the binary class value."
  [data]
  (reduce (fn [mem sample]
            (let [k (last sample)
                  sample (drop-last sample)
                  cur (get mem k [])]
            (assoc mem k (conj cur sample))))
          {}
          data))

(defn train-classifier
  [data]
  (let [[data test-data] (split-dataset data TRAIN-TEST-SPLIT)
        by-class (diabetes-class-data data)
        classifier (gaussian-naive-classifier by-class)
        predictions (predict-all classifier (map drop-last test-data))]
    (println "prediction accuracy: ")
    (println (prediction-accuracy test-data predictions))

    (println "class probabilities:")
    (println (normalized-class-probabilities (class-probabilities classifier (first test-data))))))
