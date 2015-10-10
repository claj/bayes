(ns thinktopic.bayes.projectile-example
  (:require [clojure.core.matrix :as mat]
            [incanter.stats :as stats]
            [incanter.charts :as chart]
            [incanter.core :refer (view)]
            [thinktopic.bayes.kalman :refer [kalman-filter kalman-filter2]]))

; An implementation of the projectile example described here:
; http://greg.czerniak.info/guides/kalman1/

(def GRAVITY -9.80665)

(defn simulated-projectile
  [angle speed]
  (let [init-dx (* speed (Math/cos (* angle (/ Math/PI 180))))
        init-dy (* speed (Math/sin (* angle (/ Math/PI 180))))
        update-fn (fn [dt [[x] [dx] [y] [dy]]]
                    (let [acceleration (mat/scale [0 GRAVITY] dt)
                          vel (mat/add [dx dy] acceleration)
                          vel-step (mat/scale vel dt)
                          pos (mat/add [x y] vel-step)
                          [dx dy] vel
                          [x y] (mat/emap #(max 0.0 %) pos)] ; No negative positions
                      [[x] [dx] [y] [dy]]))]
    [[[0] [init-dx] [0] [init-dy]] update-fn]))

(defn sample-multi-normal
  [mean sd]
  (mat/emap #(stats/sample-normal 1 :mean % :sd sd) mean))

(defn unravel
  [n coll]
  (map #(take-nth n (drop % coll)) (range n)))

(defn plot-projectile
  []
  (let [[s0 cannon] (simulated-projectile 45 100)
        step-fn (partial cannon 0.1)
        steps  160
        states (take steps (iterate step-fn s0))
        xs  (map ffirst states)
        ys  (map #(first (nth % 2)) states)
        nys (map #(stats/sample-normal 1 :mean % :sd 10) ys)
        p   (chart/xy-plot xs ys :title "Kalman Filtered Projectile"
                          :x-label "distance (X pos)" :y-label "height (Y pos)"
                          :legend true
                          :series-label "True Trajectory"
                          :auto-sort false
                          :points false)]
    (chart/add-lines p xs nys :series-label "Measurements")
    (view p)))

(defn rand-matrix
  [m n]
  (mat/matrix (partition n (take (* m n) (repeatedly rand)))))

(defn projectile-filter
  "Simulate a flying projectile, like a rock thrown out of a catapult, which is being
  'sensed' by a person with a telescope who is quickly noting down the estimated state."
  []
  (let [dt 0.1        ; seconds
        iters 150
        angle 45 ; at which object is thrown
        speed 100
        init-height 50
        sensor-noise 50
        [start-state proj-fn] (simulated-projectile angle speed)
        projectile (partial proj-fn dt)
        sensor     (fn [s0]
                     (let [[[x] [dx] [y] [dy]] (projectile s0)
                           nx (stats/sample-normal 1 :mean x :sd sensor-noise)
                           ny (stats/sample-normal 1 :mean y :sd sensor-noise)]
                       [[nx] [dx] [ny] [dy]]))
        init-state (mat/matrix [[0] [200] [init-height] [30]])
        init-cov   (mat/identity-matrix 4)
        update     (mat/matrix [[1 dt 0 0] [0 1 0 0] [0 0 1 dt] [0 0 0 1]])
        control    (mat/matrix [[0 0 0 0] [0 0 0 0] [0 0 1 0] [0 0 0 1]])
        control-input (mat/matrix [[0] [0] [(* 0.5 GRAVITY dt dt)] [(* GRAVITY dt)]])
        observe    (mat/identity-matrix 4)
        P-err      0.8
        proc-error (mat/diagonal-matrix (take 4 (repeat P-err)))
        ;proc-error (mat/zero-matrix 4 4)
        Q-err      0.001
        meas-error (mat/diagonal-matrix (take 4 (repeat Q-err)))
        kalman     (kalman-filter update control observe proc-error meas-error)]
    (loop [i          1
           data       []
           pred-state init-state
           pred-cov   init-cov
           true-state start-state]
      (let [next-state (projectile true-state)
            measurement (sensor true-state)
            [pred-state pred-cov] (kalman pred-state pred-cov control-input measurement)
            data (conj data [next-state measurement pred-state])]
        (if (< i iters)
          (recur (inc i) data pred-state pred-cov next-state)
          data)))))

(defn projectile-filter2
  "Simulate a flying projectile, like a rock thrown out of a catapult, which is being
  'sensed' by a person with a telescope who is quickly noting down the estimated state."
  []
  (let [dt 0.1                          ; seconds
        iters 150
        angle 45                        ; at which object is thrown
        speed 100
        init-height 50
        sensor-noise 50
        [start-state proj-fn] (simulated-projectile angle speed)
        projectile (partial proj-fn dt)
        sensor     (fn [s0]
                     (let [[[x] [dx] [y] [dy]] (projectile s0)
                           nx (stats/sample-normal 1 :mean x :sd sensor-noise)
                           ny (stats/sample-normal 1 :mean y :sd sensor-noise)]
                       [[nx] [dx] [ny] [dy]]))
        init-state (mat/matrix [[0] [200] [init-height] [30]])
        init-cov   (mat/identity-matrix 4)
        update     (mat/matrix [[1 dt 0 0] [0 1 0 0] [0 0 1 dt] [0 0 0 1]])
        control    (mat/matrix [[0 0 0 0] [0 0 0 0] [0 0 1 0] [0 0 0 1]])
        control-input (mat/matrix [[0] [0] [(* 0.5 GRAVITY dt dt)] [(* GRAVITY dt)]])
        observe    (mat/identity-matrix 4)
        P-err      0.8
        proc-error (mat/diagonal-matrix (take 4 (repeat P-err)))
                                        ;proc-error (mat/zero-matrix 4 4)
        Q-err      0.001
        meas-error (mat/diagonal-matrix (take 4 (repeat Q-err)))
        kalman     (kalman-filter2 (constantly update) (constantly control)
                                   (constantly observe)
                                   (constantly proc-error)
                                   (constantly meas-error)
                                   (constantly control-input)
                                   identity
                                   (fn [new raw] new))]
    (loop [i          1
           data       []
           pred-state init-state
           raw-pred-state init-state
           pred-cov   init-cov
           true-state start-state]
      (let [next-state (projectile true-state)
            measurement (sensor true-state)
            [pred-state raw-pred-state pred-cov] (kalman pred-state raw-pred-state pred-cov
                                                         measurement)
            data (conj data [next-state measurement raw-pred-state])]
        (if (< i iters)
          (recur (inc i) data pred-state raw-pred-state pred-cov next-state)
          data)))))

(defn plot-filtered-projectile
  [d]
  (let [;[reals noisy predictions] (unravel 3 d)
        reals (map first d)
        noisy (map second d)
        predictions (map #(nth % 2) d)
        size (count reals)
        xs (map ffirst reals)
        ys (map #(mat/mget % 2 0) reals)
        nxs (map ffirst noisy)
        nys (map #(mat/mget % 2 0) noisy)
        pxs (map ffirst predictions)
        pys (map #(mat/mget % 2 0) predictions)
        p  (chart/xy-plot xs ys :title "Kalman Filtered Projectile"
                          :x-label "distance (X pos)" :y-label "height (Y pos)"
                          :legend true
                          :series-label "True Trajectory"
                          :points false)]
    (chart/add-lines p nxs nys :series-label "Measurements")
    (chart/add-lines p pxs pys :series-label "Predictions")
    (view p)))

