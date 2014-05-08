(ns thinktopic.bayes.kalman
  (:require [clojure.core.matrix :as mat]))

; The Kalman filter is a means of combining the predicted state with measured state.

; The state vector represents the system variables of interest, so for a moving
; vehicle these might be [position, velocity, acceleration, orientation].

; ## Equations

; State Prediction (predict next state)
; x(t+1) = Ax(t) + Bu(t+1) + w(t)
; A: An NxN matrix that updates the state from one timestep to another
; B: Nx1 Matrix relating an optional control input to the state
; u: control input
; w: process noise (implicit, not computed)
(defn- state-update
  [A B x u]
  (mat/add (mat/mmul A x) (mat/mmul B u)))

; Covariance Prediction (predict how much error)
; P(t+1) = A * P(t) * A^T + Q
; P: estimate of average error (probabilities) for each component of the state
; A: state update matrix
; A^T: transposed state update matrix
; Q: estimated process error covariance
(defn- probability-estimate
  [A A-trans prev-estimate Q]
  (mat/add (mat/mmul A (mat/mmul prev-estimate A-trans)) Q))

; Measurement (convert from state to expected sensor values)
; y(t+1) = Hx(t) + v(t)
; H: a state -> measurement, transform matrix
; v: measurement noise (implicit, not computed)
(defn- state->measurement
  [H state]
  (mat/mmul H state))

; Innovation (compare prediction against sensed values)
; I(t) = Z(t) - (H * X(t))
; Z: measurement vector from sensors
; H: MxN observation matrix converting from the state to the expected sensor values
(defn- measured-diff
  [H state measurement]
  (mat/sub measurement (state->measurement H state)))

; Innovation Covariance (compare real vs predicted error)
; S(t+1) = H * P(t+1) * H^T + R
(defn- innovation-covariance
  [H H-trans cov meas-error]
  (mat/add (mat/mmul H (mat/mmul cov H-trans)) meas-error))

; Kalman Gain (weighting prediction vs sensor values)
; K(t) = P(t) * H^T * S^-1
; S: innovation covariance
(defn- kalman-gain
  [P H-trans S]
  (mat/mmul P (mat/mmul H-trans (mat/inverse S))))

; State Update (new weighted, smoothed estimate)
; x(t) = x(t)(predicted) + K * y(t)
(defn- smoothed-state-update
  [x K y]
  (mat/add x (mat/mmul K y)))

; Covariance update (estimate of current error)
; P(t) = P(predicted) - K * S * K^T
(defn- update-covariance
  [k S P]
  (mat/sub P (mat/mmul k S (mat/transpose k))))

; TODO: investigate the different variations of the covariance update equation further...
; P(t) = (I(t) - K * H) * P(predicted)
;(defn- update-covariance
;  [H k P]
;  (let [I (mat/identity-matrix (first (mat/shape P)))]
;    (mat/mmul (mat/sub (mat/mmul k H) I) P)))

(defn kalman-filter
  [update control observation proc-error meas-error]
  (let [update-trans (mat/transpose update)
        observation-trans (mat/transpose observation)]
  (fn [state covariance control-input measurement]
    (let [; Prediction
          state-pred     (state-update update control state control-input)
          cov-pred       (probability-estimate update update-trans covariance proc-error)

          ; Observation
          innovation     (measured-diff observation state-pred measurement)
          innovation-cov (innovation-covariance observation observation-trans cov-pred meas-error)

          ; Update
          gain           (kalman-gain cov-pred observation-trans innovation-cov)
          next-state     (smoothed-state-update state-pred gain innovation)
          next-probs     (update-covariance gain innovation-cov covariance)]
      [next-state next-probs]))))
