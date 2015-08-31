(ns thinktopic.bayes.naive-classifier
  (:require
    [clojure.core.matrix :as mat]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

; A naive Bayes classifier takes multivariate categorical input and
; classifies it to be one of a set of possible classes.  It is called "naive"
; because it is a model that assumes that each component is independent.
; This conditional independence assumption is wrong, but lets us at least
; find an approximate solution.

; For document classification find the class that is most likely (argmax)
; given the document:

; p(class | doc) = p(doc | class) * p(class)
;                   -----------------------
;                          p(doc)

; Since p(doc) is constant, we can just use
;   p(doc|class)*p(class) or likelihood * prior

; p(class) is just globally, how often does a class occur?  The relative
; frequency versus other classes.

; p(class_j) = (docs in class_j) / (num docs)

; What is p(doc | class)?  Given a bag of words representation, it's the
; probability of a set of term coefficients given a class.
; Given the independence assumption, we can compute the joint probability
; conditioned on the class as the product of a bunch of indpendent
; probabilities:
; p(x_1, x_2,..., x_n | c) = p(x_1 | c) * p(x_2 | c)... p(x_n | c)

; p(x_i | class_j) = count(x_i, class_j) / sum(count(w, class_j))

; The number of times word x_i appears in docs of class j, normalized by the
; total number of words in documents of class j.  (In other words, the fraction
; of times word x_i appears amongst all words in docs of class j.)

; If a term never shows up in the training set for some classes it would get
; zero probabilities for them, screwing up the likelihood calculation since
; all probabilities are multiplied together.  To solve this we use "add 1
; smoothing", by just adding 1 to the numerator and denominator.
;
; p(x_i | class_j) = (count(x_i, class_j) + 1) / (sum(count(w, class_j) + 1))

; Since 1 is added for each word in the sum, it results in:

; p(x_i | class_j) = (count(x_i, class_j) + 1) / sum(count(w, class_j) + size_of_vocab


; Learning:
; * extract vocabulary from full training set
; * calculate p(class_j) for each class (prior class probabilities)
; * concat all docs of class_j into one megadoc
; * count # of occurrences of each term in the megadoc, n_i
; * p(word x_i | c_j) = (n_i + alpha) / (n + alpha * |vocab|)
;  - n = total # of tokens in class_j
; * to handle unknown words, add an unknown word term to the vocab, with a count
; of zero to each class count.  (Gets 1 or alpha smoothed.)

; class = argmax p(class_j) \pi_{i \in positions} p(x_i | c_j)
(def TRAIN-TEST-SPLIT 0.67)

(defn load-data
  []
  (with-open [in-file (io/reader "resources/pima-indians-diabetes.csv")]
    (into [] (csv/read-csv in-file))))

(def data (memoize load-data))

(def split-dataset
  "Split the data into [train, test] sets with train having ratio % of the data."
  [data ratio]
  (split-at (shuffle data) (int (* (count data) ratio))))

(defn mean
  [vs]
  (double
    (/ (apply + vs)
       (count vs))))

(defn variance
  [vs]
  (let [avg (mean vs)]
    (/ (apply + (map #(Math/pow (- % avg) 2) vs))
       (dec (count vs)))))

(defn stdev
  [vs]
  (Math/sqrt (variance vs)))

(defn summarize
  [data]
  (apply map (fn [& terms] [(mean terms) (stdev terms)]) data))

(defn train-classifier
  [data]
  (let [[train-data test-data] (split-dataset data TRAIN-TEST-SPLIT)
        by-class (group-by train-data last)]
    ))



