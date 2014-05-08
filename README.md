# Bayes

Recursive Bayes and Kalman filtering in Clojure.

The Kalman filter is a recursive filter that combines incoming noisy
sample data with predictions from an internal model of the data. For
example if you are measuring someone's position with a GPS device than
typically the samples will jump around noisily, but if you know the
person or vehicle's velocity than you can predict where they are from
one moment to the next based on their most recent location. Using the
Kalman filter these two sources of data can be efficiently combined in
such a way as to provide a smoother, more accurate estimate of the true
location. This is done by adjusting the weight of the model versus the
sensor data based on how noisy or accurate they are.

* the computational complexity is cubic in the size of the state space
* The basic Kalman filter can only be used for linear systems with gaussian noise

Extended Kalman filter (EKF):
* http://en.wikipedia.org/wiki/Extended_Kalman_filter
* A very similar estimator that works for non-linear systems, using
Taylor series expansion to linearize around the current mean estimate

Unscented Kalman filter (UKF):
* http://en.wikipedia.org/wiki/Kalman_filter#Unscented_Kalman_filter
* Similar to the EKF, but uses a sampling approach for the model estimation step

Particle filter:
* Represents the underlying system with a set of samples (particles),
which are updated, removed and spawned in a probabilistic fashion based
on how well they approximate the incoming sensor data.

## Resources

Wikipedia:
* http://en.wikipedia.org/wiki/Kalman_filter

Kalman Filters for Undergrads:
* http://greg.czerniak.info/guides/kalman1/

Student Dave's tutorials:
* http://studentdavestutorials.weebly.com/

An Introduction to the Kalman Filter:
* http://www.cs.unc.edu/~welch/media/pdf/kalman_intro.pdf

Further resources:
* http://www.cs.unc.edu/~welch/kalman/

## License

Copyright © 2014 Jeff Rose - ThinkTopic Labs

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
