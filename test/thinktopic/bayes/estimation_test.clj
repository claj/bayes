(ns thinktopic.bayes.estimation-test
  (:require [thinktopic.bayes.estimation :as est]
            [clojure.core.matrix :as mat]
            [incanter.core :refer [view]]
            [incanter.stats :as stats]
            [incanter.charts :as chart]
            [clojure.tools.trace :refer [trace]])
  (:use clojure.core.matrix.operators))

(mat/set-current-implementation :vectorz)  ;; use Vectorz as default matrix implementation


(defn rand-location
  [grid-size]
  (take 2 (repeatedly #(rand-int grid-size))))

(defn ones
  [shape]
  (mat/fill (mat/new-array shape) 1.0))


(defn normal-pdf
  [mean cov sample]
  (let [normalizer (/ 1.0
                      (Math/sqrt (* (Math/pow (* 2 Math/PI) 2)
                                    (mat/det cov))))
        mean-diff  (mat/sub sample mean)
        v (mat/mmul (mat/transpose mean-diff) (mat/inverse cov) mean-diff)
        likelihood (Math/exp (* -0.5 (.get v)))]
    (* likelihood normalizer)))


(defn test-estimator
  []
  (let [grid-size 10
        n-samples 100
        sample-std 3
        location (rand-location grid-size)
        sampler #(stats/sample-normal n-samples :mean % :sd sample-std)
        x-samples (sampler (first location))
        y-samples (sampler (second location))
        ;sensor-vals (partition 2 (interleave x-samples, y-samples))
        guess-location (rand-location grid-size)
        location-plot (chart/scatter-plot x-samples y-samples :title "Sensor Samples")

        ; State space for where object could be
        grid-states [(range 0 grid-size 0.5) (range 0 grid-size 0.5)]
        grid-len (count (first grid-states))
        prior-belief (ones [grid-len grid-len])
        posterior-belief (ones [grid-len grid-len])
        total-mass (mat/esum prior-belief)

        ; Convert states to probability mass function (PMF)
        ; - like a PDF, but for discrete random variables
        prior-belief (mat/div prior-belief total-mass)
        prior-covariance (mat/matrix [[4 0] [0 4]])
        posterior-belief (mat/div posterior-belief total-mass)
        posterior-belief (mat/mset posterior-belief 0 0 (+ 0.0001 (mat/mget posterior-belief 0 0)))
        heat-fn (fn [x y]
                  (let [x (Math/abs (int x))
                        y (Math/abs (int y))
                        z (mat/mget posterior-belief x y)]
                    z))
        belief-plot (chart/heat-map heat-fn 0 10 0 10)

        ; Compute likelihood of a sample given a multivariate normal distribution
        norm-likelihood (fn [mean cov sample]
                          (let [normalizer (/ 1.0
                                              (Math/sqrt (* (Math/pow (* 2 Math/PI) 2)
                                                            (mat/det cov))))
                                mean-diff (mat/sub sample mean)
                                likelihood (Math/exp (mat/scale -0.5
                                                                (mat/mmul
                                                                  (mat/transpose mean-diff)
                                                                  (mat/inverse cov)
                                                                  mean-diff)))]
                            (mat/scale likelihood normalizer)))
        update-fn (fn [prior]
                    (for [i (range grid-len)]
                      (for [j (range grid-len)]
                        (let [[state-xs state-ys] grid-states
                              mean (mat/matrix [(nth state-xs i) (nth state-ys j)])
                              x (nth x-samples i)
                              y (nth y-samples j)
                              likelihood (norm-likelihood mean prior-covariance (mat/matrix [x y]))]))))]
    (doto location-plot
      (chart/add-points x-samples y-samples)
      ;(chart/set-stroke-color java.awt.Color/red)
      ; Try adding a polygon to mark actual location instead...
      (chart/add-points (first location) (second location))
      (chart/set-x-range (- sample-std) (+ grid-size sample-std))
      (chart/set-y-range (- sample-std) (+ grid-size sample-std)))
    (view location-plot)
    (view belief-plot)))


(test-estimator)
