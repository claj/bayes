(ns thinktopic.bayes.sensor-example
  (:require [clojure.core.matrix :as mat]
            [incanter.stats :as stats]
            [incanter.charts :as chart]
            [incanter.core :refer (view)]
            [thinktopic.bayes.kalman :refer [kalman-filter]]))

(defn simulated-fixed-sensor
  [v noise]
  #(stats/sample-normal 1 :mean v :sd noise))


(defn- series-append-point
  [series x y]
    ;(println "adding to series: " x y)
    (.add series (double x) (double y)))


(defn sensor-filter
  "Simulate a noisy sensor which is sensing a stable value, and then
  filter the sensor values to get a stable estimate."
  [value]
  (let [v-noise 0.25
        sensor     (simulated-fixed-sensor value v-noise)
        update     [[1]]
        control    [[0]]
        observe    [[1]]
        proc-error [[0.00001]]
        meas-error [[0.1]]
        init-state [[3.0]]
        init-cov   [[1]]
        iters 50
        kalman (kalman-filter update control observe proc-error meas-error)]
    (loop [i 1 data [[(sensor) init-state]] state init-state cov init-cov]
      (let [measurement (sensor)
            [next-state next-cov] (kalman state cov [0] [measurement])
            data (conj data [measurement next-state])]
        (if (< i iters)
          (recur (inc i) data next-state next-cov)
          data)))))


(defn plot-sensor-data
  [value data]
  (let [vs   (map first data)
        ps   (map (comp ffirst second) data)
        size (count data)
        xs   (range size)
        p  (chart/xy-plot xs vs :title "Kalman Filtered Voltmeter"
                          :x-label "time" :y-label "volts"
                          :legend true
                          :series-label "Measurements"
                          :points true)]
    (chart/add-lines p [0 size] [value value] :series-label "True Voltage")
    (chart/add-lines p xs ps :series-label "Kalman Estimate")
    (view p)))


(defn run
  []
  (plot-sensor-data 1.25 (sensor-filter 1.25)))


