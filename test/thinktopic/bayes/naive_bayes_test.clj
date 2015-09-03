(ns thinktopic.bayes.naive-bayes-test
  (:require [clojure.core.matrix :as mat]
            [thinktopic.bayes.naive-classifier :refer [gaussian-naive-classifier predict-all
                                                       prediction-accuracy normalized-class-probabilities
                                                       class-probabilities tokenize-str train-word-model
                                                       predict-word-class sample-class-probabilities ]]
            [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def TRAIN-TEST-SPLIT 0.67)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gaussian Naive Bayes for Continous Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multinomial Naive Bayes for Text Classification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-test-docs
  []
  [["this is a test" :a]
   ["another test it is" :a]
   ["This is a wonderful day for skiing today." :b]
   ["Wouldn't it be great to go skiing after work today." :b]
   ["I wonder what the skiing is like today?" :b]
   ["Do you think we could go skiing today?" :b]])

(defn test-word-model
  []
  (let [docs (load-test-docs)
        tokenized (map (fn [[words clazz]] [(tokenize-str words) clazz]) docs)
        model (train-word-model (drop-last tokenized))]
    (println "model: " model)
    (println "class probs: "(sample-class-probabilities model (last tokenized)))
    (println "prediction: " (predict-word-class model (last tokenized)))))

(defn class-line-data
  [path]
  (with-open [in-file (io/reader path)]
    (let [lines (line-seq in-file)
          split-lines (map #(re-find #"(\S+)\s+(.*)" %) lines)
          docs (map (fn [[_ clazz txt]]
                      [txt (keyword clazz)])
                    split-lines)]
      (into [] docs))))

(defn sms-data
  []
  (class-line-data "resources/sms_spam_collection.txt"))

(defn newsgroup-data
  []
  [(class-line-data "resources/20ng-train-stemmed.txt")
   (class-line-data "resources/20ng-test-stemmed.txt")])

(defn test-sms-spam-model
  []
  (let [docs (sms-data)
        [orig-docs orig-test-docs] (split-dataset docs TRAIN-TEST-SPLIT)
        tokenized (map (fn [[words clazz]] [(tokenize-str words) clazz]) docs)
        [docs test-docs] (split-dataset tokenized TRAIN-TEST-SPLIT)
        model (train-word-model docs)
        predictions (map (partial predict-word-class model) (map first test-docs))
        accuracy (prediction-accuracy test-docs predictions)]
    ;(doall
    ;  (map #(println %1 (first %2)) predictions orig-test-docs))
    (println "accuracy: " accuracy)))

(defn test-newsgroup-model
  []
  (let [[train-docs test-docs] (newsgroup-data)
        tokenized (map (fn [[words clazz]] [(tokenize-str words) clazz]) train-docs)
        tokenized-test (map (fn [[words clazz]] [(tokenize-str words) clazz]) test-docs)
        model (train-word-model tokenized)
        predictions (map (partial predict-word-class model) (map first tokenized-test))
        accuracy (prediction-accuracy test-docs predictions)]
    ;(doall
    ;  (map #(println %1 (first %2)) predictions test-docs))
    (println "accuracy: " accuracy)))
