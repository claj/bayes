(ns thinktopic.bayes.gps-example
  (:require [clojure.core.matrix :as mat]
            [incanter.stats :as stats]
            [incanter.charts :as chart]
            [incanter.core :refer (view)]
            [clj-time.coerce    :as timec]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [thinktopic.bayes.kalman :refer [kalman-filter2]]))

;;;
;;; Change lat lng -> x y Cartesian
;;;
;;
;; https://en.wikipedia.org/wiki/Geographic_coordinate_system#Expressing_latitude_and_longitude_as_linear_units
;;
(defn lat->meters [lat]
  (let [rlat (Math/toRadians lat)]
    [(+ 111132.95255
        (-> (* 2 rlat)
            Math/cos
            (* -559.84957))
        (-> (* 4 rlat)
            Math/cos
            (* 1.17514))
        (-> (* 6 rlat)
            Math/cos
            (* -0.00230)))
     (+ (-> (Math/cos rlat)
            (* 111412.87733))
        (-> (* 3 rlat)
            Math/cos
            (* -93.50412))
        (-> (* 5 rlat)
            Math/cos
            (* -0.11774))
        -0.000165)]))

(defn lat-lng->xy-coord
  "Map lat-org and lng-org to (0, 0) and return (x, y) for the target point"
  [lat-org lng-org lat lng]
  (let [[lat-1-deg lng-1-deg] (lat->meters lat-org)]
    [(-> (- lng lng-org) (* lng-1-deg))
     (-> (- lat lat-org) (* lat-1-deg))]))

(defn points-distance [x1y1 x2y2]
  (->> (map - x1y1 x2y2)
       (map #(* % %))
       (apply +)
       (Math/sqrt)))

;;;
;;; Utiliity function to manipulate GPS data
;;;
(def half-pi (/ Math/PI 2))
(def two-pi (* 2 Math/PI))

(defn- compute-angle [x1 y1 x2 y2]
  (-> (Math/atan2 (- y2 y1) (- x2 x1))
      (mod two-pi)))

(defn radian->compass [angle-radian]
  (mod (->> (Math/toDegrees angle-radian)
            (- 90))
       360))

(defn- angle-degree [lat1 lng1 lat2 lng2]
  (->> (lat-lng->xy-coord lat1 lng1 lat2 lng2)
       (apply compute-angle 0 0)
       radian->compass))

;;
;; example data
;;
(defn- build-test-data [filename]
  (letfn [(data->map [data]
            (->> (map clojure.edn/read-string data)
                 (zipmap [:lat :lng :error])))
          (update-time [time-string data-vector]
            (let [time-string (clojure.string/replace time-string #"\s" "T")
                  msec (/ 1000 (count data-vector))
                  msec-seq (->> (range 0 1000 msec))]
              (mapv #(->> (str time-string "." %2)
                          timec/from-string
                          (assoc %1 :timestamp))
                    data-vector msec-seq)))]
   ;; file format:
   ;;    date time <tab> lat <tab> lng <tab> accuracy
   (with-open [in-file (io/reader filename)]
     (let [[[first-date-time & first-data] & more-data] (csv/read-csv in-file :separator \tab)]
       (loop [[[date-time & data] & more-vec] more-data
              current-time first-date-time
              current [(data->map first-data)]
              result []]
         (if (= date-time current-time)
           (recur more-vec current-time (conj current (data->map data)) result)
           (if (nil? date-time)
             (->> (update-time current-time current)
                  (into result))
             (recur more-vec
                    date-time
                    [(data->map data)]
                    (into result (update-time current-time current))))))))))

(def gps-test-data (build-test-data "test/thinktopic/bayes/gps.txt"))

(def speed-upper-bound 34)              ; 120km/h

(def error-upper-bound 100)             ; 100m

(defn pre-process-data [data]
  (for [entry data
        :when (-> (:error entry) (< 100))]
    entry))



;; (defn- update-location-matrix [raw-state covariance raw-measurement]
;;   (let [dt (- (:elapsed-time raw-measurement) (:elapsed-time raw-state))]
;;     ;; NOTE: direction is useless
;;     ;;          Compute it!!
;;     (let [rad (if-let [direction (:direction raw-state)] ;; get direction from previous state
;;                 (Math/toRadians direction)
;;                 ;; FIXME: check if this is correct
;;                 ;; manually compute direction
;;                 (compute-angle (:x raw-state) (:y raw-state) (:x raw-measurement) (:y raw-measurement)))]
;;         (mat/matrix [[1 0 0 (-> (Math/cos rad) (* dt))]
;;                      [0 1 0 (-> (Math/sin rad) (* dt))]
;;                      [0 0 1 0]
;;                      [0 0 0 1]]))))

;; (def zero-4x4-matrix (mat/matrix [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]))

;; (defn- speed->noise [m-per-sec]
;;   ;; high speed = more chances of errors
;;   (cond (<= m-per-sec 8.5) 0            ; about 30
;;         (<= m-per-sec 14) (-> (rand 0.001) (+ 0.05)) ; about 50
;;         (<= m-per-sec 20) (-> (rand 0.01)  (+ 0.3)) ; about 70
;;         (<= m-per-sec 25) (-> (rand 0.03)  (+ 0.5)) ; about 90
;;         (<= m-per-sec 31) (-> (rand 0.06)  (+ 0.8)) ; about 111
;;         (<= m-per-sec 42) (-> (rand 0.09)  (+ 0.9)) ; about 150
;;         :else 1))

;; (defn- accuracy->noise [accuracy]
;;   (if (>= accuracy 100)
;;     1
;;     0))

;; (defn- direction->noise [direction-per-sec1 direction-per-sec2]
;;   (let [d-per-sec  (-> (- direction-per-sec1 direction-per-sec1)
;;                        Math/abs)]
    
;;     (cond (<= d-per-sec (/ Math/PI 4)) 0
;;           (<= d-per-sec (/ Math/PI 3)) (+ 0.5 (rand 0.2))
;;           (<= d-per-sec (/ Math/PI 2)) (+ 0.7 (rand 0.3))
;;           :else 1)))

;; (defn location-process-noise [raw-state covariance raw-measurement]
;;   ;; speed, direction, time period, accuracy ...
;;   (let [])
;;   )

;; (defn location-measurement-noise [raw-state covariance raw-measurement]
;;   ;; Assumptions
;;   ;; - high speed = more measurement errors
;;   ;; - quick direction changes = measurement errors
;;   )


;; (defn- raw-location-record->state [rec]
;;   ;; [x y angle ...]
;;   (let [[x y]]))

;; (defn location-filter
;;   [path]
;;   (let [kalman (kalman-filter2 update-location-matrix
;;                                (constantly zero-4x4-matrix) ; control
;;                                (constantly zero-4x4-matrix) ; observe
;;                                location-process-noise
;;                                location-measurement-noise
;;                                (constantly (mat/matrix [[0] [0] [0] [0]])) ; control-input
;;                                ??
;;                                ??)]
;;    (loop [[location & more-locations] path
         

;;           i          1
;;           data       []
;;           pred-state init-state
;;           raw-pred-state init-state
;;           pred-cov   init-cov
;;           true-state start-state]
;;      (let [next-state (projectile true-state)
;;            measurement (sensor true-state)
;;            [pred-state raw-pred-state pred-cov] (kalman pred-state raw-pred-state pred-cov
;;                                                         measurement)
;;            data (conj data [next-state measurement raw-pred-state])]
;;        (if (< i iters)
;;          (recur (inc i) data pred-state raw-pred-state pred-cov next-state)
;;          data))))
;;     )

;; (defn projectile-filter2
;;   "Simulate a flying projectile, like a rock thrown out of a catapult, which is being
;;   'sensed' by a person with a telescope who is quickly noting down the estimated state."
;;   []
;;   (let [dt 0.1                          ; seconds
;;         iters 150
;;         angle 45                        ; at which object is thrown
;;         speed 100
;;         init-height 50
;;         sensor-noise 50
;;         [start-state proj-fn] (simulated-projectile angle speed)
;;         projectile (partial proj-fn dt)
;;         sensor     (fn [s0]
;;                      (let [[[x] [dx] [y] [dy]] (projectile s0)
;;                            nx (stats/sample-normal 1 :mean x :sd sensor-noise)
;;                            ny (stats/sample-normal 1 :mean y :sd sensor-noise)]
;;                        [[nx] [dx] [ny] [dy]]))
;;         init-state (mat/matrix [[0] [200] [init-height] [30]])
;;         init-cov   (mat/identity-matrix 4)
;;         update     (mat/matrix [[1 dt 0 0] [0 1 0 0] [0 0 1 dt] [0 0 0 1]])
;;         control    (mat/matrix [[0 0 0 0] [0 0 0 0] [0 0 1 0] [0 0 0 1]])
;;         control-input (mat/matrix [[0] [0] [(* 0.5 GRAVITY dt dt)] [(* GRAVITY dt)]])
;;         observe    (mat/identity-matrix 4)
;;         P-err      0.8
;;         proc-error (mat/diagonal-matrix (take 4 (repeat P-err)))
;;                                         ;proc-error (mat/zero-matrix 4 4)
;;         Q-err      0.001
;;         meas-error (mat/diagonal-matrix (take 4 (repeat Q-err)))
;;         kalman     (kalman-filter2 (constantly update) (constantly control)
;;                                    (constantly observe)
;;                                    (constantly proc-error)
;;                                    (constantly meas-error)
;;                                    (constantly control-input)
;;                                    identity
;;                                    (fn [new raw] new))]
;;     (loop [i          1
;;            data       []
;;            pred-state init-state
;;            raw-pred-state init-state
;;            pred-cov   init-cov
;;            true-state start-state]
;;       (let [next-state (projectile true-state)
;;             measurement (sensor true-state)
;;             [pred-state raw-pred-state pred-cov] (kalman pred-state raw-pred-state pred-cov
;;                                                          measurement)
;;             data (conj data [next-state measurement raw-pred-state])]
;;         (if (< i iters)
;;           (recur (inc i) data pred-state raw-pred-state pred-cov next-state)
;;           data)))))

;; (defn plot-filtered-projectile
;;   [d]
;;   (let [;[reals noisy predictions] (unravel 3 d)
;;         reals (map first d)
;;         noisy (map second d)
;;         predictions (map #(nth % 2) d)
;;         size (count reals)
;;         xs (map ffirst reals)
;;         ys (map #(mat/mget % 2 0) reals)
;;         nxs (map ffirst noisy)
;;         nys (map #(mat/mget % 2 0) noisy)
;;         pxs (map ffirst predictions)
;;         pys (map #(mat/mget % 2 0) predictions)
;;         p  (chart/xy-plot xs ys :title "Kalman Filtered Projectile"
;;                           :x-label "distance (X pos)" :y-label "height (Y pos)"
;;                           :legend true
;;                           :series-label "True Trajectory"
;;                           :points false)]
;;     (chart/add-lines p nxs nys :series-label "Measurements")
;;     (chart/add-lines p pxs pys :series-label "Predictions")
;;     (view p)))

;; ;;(angle-degree 43.682213, -70.450696 43.682194, -70.450769)
;; ;;-> 250.27027877401773
;; ;;(angle-degree 43.682194 -70.450769 43.682179, -70.450837)

;; (defn- get-angle2 [x1 y1 x2 y2]
;;   (-> (Math/atan2 (- y2 y1) (- x2 x1))
;;       Math/toDegrees
;;       (mod 360)))

;; (defn- angle-test [x1 y1 x2 y2]
;;   (let [rad (compute-angle x1 y1 x2 y2)]
;;     [rad (Math/toDegrees rad) (radian->compass rad)]))






;; (defn- angle-test2 [lat1 lng1 lat2 lng2]
;;   (->> (lat-lng->xy-coord lat1 lng1 lat2 lng2)
;;       (apply angle-test 0 0)))


;; (defn projectile-filter
;;   "Simulate a flying projectile, like a rock thrown out of a catapult, which is being
;;   'sensed' by a person with a telescope who is quickly noting down the estimated state."
;;   []
;;   (let [dt 0.1        ; seconds
;;         iters 150
;;         angle 45 ; at which object is thrown
;;         speed 100
;;         init-height 50
;;         sensor-noise 50
;;         [start-state proj-fn] (simulated-projectile angle speed)
;;         projectile (partial proj-fn dt)
;;         sensor     (fn [s0]
;;                      (let [[[x] [dx] [y] [dy]] (projectile s0)
;;                            nx (stats/sample-normal 1 :mean x :sd sensor-noise)
;;                            ny (stats/sample-normal 1 :mean y :sd sensor-noise)]
;;                        [[nx] [dx] [ny] [dy]]))
;;         init-state (mat/matrix [[0] [200] [init-height] [30]])
;;         init-cov   (mat/identity-matrix 4)
;;         update     (mat/matrix [[1 dt 0 0] [0 1 0 0] [0 0 1 dt] [0 0 0 1]])
;;         control    (mat/matrix [[0 0 0 0] [0 0 0 0] [0 0 1 0] [0 0 0 1]])
;;         control-input (mat/matrix [[0] [0] [(* 0.5 GRAVITY dt dt)] [(* GRAVITY dt)]])
;;         observe    (mat/identity-matrix 4)
;;         P-err      0.8
;;         proc-error (mat/diagonal-matrix (take 4 (repeat P-err)))
;;         ;proc-error (mat/zero-matrix 4 4)
;;         Q-err      0.001
;;         meas-error (mat/diagonal-matrix (take 4 (repeat Q-err)))
;;         kalman     (kalman-filter update control observe proc-error meas-error)]
;;     (loop [i          1
;;            data       []
;;            pred-state init-state
;;            pred-cov   init-cov
;;            true-state start-state]
;;       (let [next-state (projectile true-state)
;;             measurement (sensor true-state)
;;             [pred-state pred-cov] (kalman pred-state pred-cov control-input measurement)
;;             data (conj data [next-state measurement pred-state])]
;;         (if (< i iters)
;;           (recur (inc i) data pred-state pred-cov next-state)
;;           data)))))
