(ns thinktopic.bayes.naive-classifier
  (:require
    [clojure.string :as string]
    [clojure.core.matrix :as mat]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gaussian classifier for samples with one or more continuous values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mean
  "Returns the average value of a sequence."
  [vs]
  (double (/ (apply + vs)
             (count vs))))

(defn variance
  "Returns the expected value of the squared difference from the mean."
  [vs]
  (let [avg (mean vs)]
    (/ (apply + (map #(Math/pow (- % avg) 2) vs))
       (dec (count vs)))))

(defn stdev
  "Returns the standard deviation of the samples."
  [vs]
  (Math/sqrt (variance vs)))

(defn summarize
  "Computes the mean and stdev for each term in a dataset.
  (e.g. avg of first param, then second, etc.)"
  [data]
  (apply map (fn [& terms] [(mean terms) (stdev terms)]) data))

(defn probability-of
  "Compute the probability of X given the Gaussian distribution described by mean and stdev."
  [x mean stdev]
  (let [exp (Math/exp (- (/ (Math/pow (- x mean) 2)
                            (* 2 (Math/pow stdev 2)))))]
    (* exp (/ 1
              (* (Math/sqrt (* 2 Math/PI))
                 stdev)))))

(defn class-probability
  "Calculate the probability of one class given its summary stats and an input sample."
  [summary sample]
  (reduce + (map (fn [[mean stdev] x]
                   (Math/log (probability-of x mean stdev)))
                 summary sample)))

(defn class-probabilities
  "Calculate the probability for each class given the training summaries and an input sample."
  [summaries sample]
  (reduce-kv (fn [mem clazz summary]
               (assoc mem clazz (class-probability summary sample)))
             {}
             summaries))

(defn normalized-class-probabilities
  "Normalize class probabilities so they sum to one."
  [class-probs]
  (let [total (reduce + (vals class-probs))]
    (zipmap (keys class-probs)
            (map #(/ % total) (vals class-probs)))))

(defn predict
  "Predict the class based on summary of training data and an input."
  [summaries sample]
  (let [probs (class-probabilities summaries sample)]
    (first (last (sort-by second probs)))))

(defn predict-all
  "Predict classes for a sequence of samples."
  [summaries samples]
  (map (partial predict summaries) samples))

(defn prediction-accuracy
  "Given labeled test data and predictions return the percent correct."
  [test-data predictions]
  (* (/ (count (filter #(= true %) (map #(= (last %1) %2) test-data predictions)))
        (count test-data))
     100.0))

(defn gaussian-naive-classifier
  "Takes a map of {<class-label> [<class samples>] ...} and returns a summary table
  of statistics that can be used to predict the class of new samples."
  [data-by-class]
  (reduce-kv (fn [mem clazz samples]
               (assoc mem clazz (summarize samples)))
             {} data-by-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multinomial classifier for text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A multinomial naive Bayes classifier takes multivariate categorical input and
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

; p(class) is the relative frequency versus other classes:
;    p(class_j) = (docs in class_j) / (num docs)

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

(defn word-class-probabilities
  "Takes a sequence of document tuples with [<sample> <class>] and returns the
  global class probabilities.  p(class_j) = (docs-in class_j) / (count docs)"
  [docs]
  (let [{:keys [by-class n]} (reduce (fn [{:keys [by-class n]} [sample clazz]]
                                       {:by-class (assoc by-class clazz (inc (get by-class clazz 0)))
                                        :n (inc n)})
                                     {:by-class {}
                                      :n 0}
                                     docs)]
    (reduce-kv (fn [mem clazz clazz-count]
                 (assoc mem clazz (/ clazz-count n)))
               {}
               by-class)))

(defn sanitize-str
  [s]
  (-> (or s "")
      (string/replace #"\." "")
      (string/replace #"\," "")
      (string/lower-case)))

(defn whitespace?
  [c]
  (or (= \space c) (= \tab c) (= \newline c)))

(defn tokenize-str
  [s]
  (map #(apply str %) (take-nth 2 (partition-by whitespace? (sanitize-str s)))))

(defn word-probabilities
  [tokens]
  (let [n (count tokens)
        freqs (frequencies tokens)
        vocab-size (count (keys freqs))
        smoothing 1]
    (reduce-kv (fn [mem word word-count]
                 (assoc mem word (/ (+ word-count smoothing)
                                    (+ n (* smoothing vocab-size)))))
               {}
               freqs)))

(defn word-class-stats
  "Returns the word probabilities p(word_i | c_j) for each class:
  {<class_j> {<word> <p(w_i | class_j)> ...}"
  [docs]
  (let [by-class (group-by second docs)]
    (reduce-kv (fn [mem clazz docs]
                 (assoc mem clazz
                        (word-probabilities (apply concat
                                                   (map #(tokenize-str (first %)) docs)))))
               {} by-class)))

(defn sample-class-probabilities
  [{:keys [class-probs word-probs]} sample]
  (let [tokens (set (tokenize-str sample))]
    (reduce-kv (fn [mem clazz prob]
                 (let [wp (get word-probs clazz)]
                   (assoc mem clazz
                          (double (apply * prob
                                         (map #(get wp % 1) tokens))))))
               {} class-probs)))

(defn train-word-model
  [docs]
  (let [class-probs (word-class-probabilities docs)
        word-probs (word-class-stats docs)]
    {:class-probs class-probs
     :word-probs word-probs}))

(defn load-test-docs
  []
  [["this is a test" :a]
   ["another test it is" :a]
   ["how about me" :b]
   ["is it about you" :b]
   ["or about me" :b]
   ["what about you" :b]])

(defn test-word-model
  []
  (let [docs (load-test-docs)
        model (train-word-model (drop-last docs))]
    ;(println model)
    (sample-class-probabilities model (last docs))))

