(ns adventofcode2017.day20
  (:use [clojure.set :refer [union intersection difference]]))

;; Advent of Code challenges
;; http://adventofcode.com/2017/day/20
;; Part 1

;; Problem setup data
(def input-file "resources/2017/day20-input.txt")

(def sample-input-file "resources/2017/day20-sample-input.txt")

;; In the long run the starting point doesn't matter except in the case of a tie.
;; First, consider only those with the lowest acceleration vector.
;; then consider only those the the slowest initial velocity along that acceleration line
;; then consider only those with the closest starting point projected along that accel line

(def p-regex #"p=< ?(-?\d+),(-?\d+),(-?\d+)>, v=< ?(-?\d+),(-?\d+),(-?\d+)>, a=< ?(-?\d+),(-?\d+),(-?\d+)>")

(defn distance [tuple]
  (reduce + (map #(Math/abs %) tuple)))

(defn build-particles
  "Parse the particle buffer into a sequence of particle maps"
  [s]
  (map-indexed (fn [i line]
                 (let [[position velocity acceleration]
                       (->> line (re-find p-regex) rest (map #(Integer/parseInt %)) (partition 3))]
                   {:id i :p position :v velocity :a acceleration}))
               (clojure.string/split s #"\n")))

(defn compute-distance
  "Decorate the particle with its distance from center"
  [p]
  (merge p {:p-dist (distance (:p p))
            :v-dist (distance (:v p))
            :a-dist (distance (:a p))}))

(defn closest [particles]
  (first (sort (fn [x y]
                 (reduce (fn [answer f]
                           (if (nil? answer)
                             (if (= x y) nil (< (f x) (f y)))
                             answer))
                         nil [:a-dist :v-dist :p-dist]))
               (map compute-distance particles))))

(defn part1
  ([] (part1 input-file))
  ([infile]
   (->> infile slurp build-particles closest :id)))

; (part1)
; => 300

;; Part 2: removing colliding points.

;; Pairwise compare points O(n^2) to solve for a time "t" when a point in 3d is the same
;; for each of the two particles.
;;
;; For each dimension, the distance from 0 at time t is
;;         d = p0 + v0t + 0.5(at^2)
;;
;; For 2 points, solve for a time t when all 3 dimensions align.
;; Reduces to the following quadratic equation for each dimension of p
;;         t^2 + (2(v1-v2)/(a1-a2))t + 2(p1-p2)/(a1-a2) = 0
;;
;; If p,v,a are all the same, they intersect always.
;; If v and a are the same, you'll never intersect
;; If only a is the same, then you may intersect 0, 1, or 2 times.
;;
;;   Solve
;;
;; Solve with quadratic equation
;;         x = (-b +/- sqrt(b^2-4ac))/2a
;;
;; where
;;    x = t ; the time at which these are at the same dimension coordinate
;;    b = 2(v1-v2)/(a1-a2)
;;    c = 2(p2-p1)/(a1-a2)
;;
;; If there are 1 or more solutions for x, calculate the starting time t for each of the pair
;; because you don't care about intersections in the past.
;;
;; Discard solutions for X that are < either of these starting points.
;;
;; Do this for each dimension, and if you have at least one t value for each dimension,
;; then that is a collision point. Mark both points to be removed at time t.
;;
;; If a point is to be removed at time t0 and t1, where t0 < t1, record the t0 only as a collision,
;; and not the t1 since it is already gone before that.

(defn binomial
  "Solve binary formula for a=1. Return nil if there is no non-imaginary solution"
  [b c]
  ;{:pre [(do (printf "--> (binomial b=%d c=%d)\n" b c) true)]}
  (let [sq (- (* b b) (* 4 c))]
    (if (>= sq 0)
      [(/ (+ (- b) (Math/sqrt sq)) 2)
       (/ (- (- b) (Math/sqrt sq)) 2)])))

(defn collision1d
  "Report times when these 2 single-dimension lines collide. Could be empty list (never) or 1 or 2 points, or :always"
  [[p1 v1 a1 :as l1] [p2 v2 a2 :as l2]]
  (if (= l1 l2)
    :always
    (if (= a1 a2)
      (if (not= v1 v2)
        (list (/ (float (- p2 p1)) (- v1 v2))))
      (filter pos?
              (binomial (/ (* 2.0 (- v1 v2)) (- a1 a2))
                        (/ (* 2.0 (- p1 p2)) (- a1 a2)))))))

(defn flatten-coordinates
  "Given an n-dimensional coordinate, flatten to list of one point velocity and acceleration per dimension"
  [p]
  (partition (count (:p p)) (interleave (:p p) (:v p) (:a p))))

(defn collision-times
  "Given 2 lines, return a relative time t when they intersect. nil if they don't."
  [p1 p2]
  ; Determine if/when they may intersect in each dimension
  (let [ts (map collision1d
                (flatten-coordinates p1)
                (flatten-coordinates p2))]
    (if (every? #{:always} ts)
      :always
      (if-not (empty? ts)
        ; Find the intersection of times they meet in each dimension
        (let [finite-ts (remove #{:always} ts)]
          (reduce (fn [s v] (intersection s (set v)))
                  (set (first finite-ts))
                  (rest finite-ts)))))))

(defn collision-points [p1 p2]
  (map #(vector % p1 p2) (collision-times p1 p2)))

;;; Samples
(def samples (->> sample-input-file slurp build-particles))
(def p1 (first samples))
(def p2 (second samples))

; should intersect at t=1.0

(defn collisions
  "Return a sorted map with ascending time keys, and values are the set of points that
  collide at that time."
  [particles]
  (->> (iterate rest particles)
       (take-while (comp not-empty rest))
       (mapcat (fn [[p & ps]] (reduce concat (pmap (partial collision-points p) ps))))
       (map (fn [x] (println x) x))
       (reduce (fn [m [t & points]]
                 (assoc m t (union (set points) (get m t #{}))))
               (sorted-map))))

(defn pruned-by-collisions [particles]
  (reduce (fn [gone [_ ps]]
            (let [remaining (difference ps gone)]
              (if (> (count remaining) 1)
                (union gone remaining)
                gone)
              ))
          #{}
          (collisions particles)))

(defn part2
  ([] (part2 input-file))
  ([infile]
   (let [particles (->> infile slurp build-particles)]
     (- (count particles) (count (pruned-by-collisions particles))))))

; => (part2)
; => 1000

; I guess there's still a bug here. They can't all not collide.

; Iterator to recalc position at time t, t+1, t+2, ...
(defn path [{coords :p velocity :v acceleration :a :as pt}]
  (cons pt (lazy-seq (path (merge pt {:p (map + coords velocity)
                                      :v (map + velocity acceleration)})))))

; take 2 points and walk 1k steps to see if they itersect.
(defn simulate [n p1 p2]
  (mapcat (fn [i p1 p2] (if (= (:p p1) (:p p2)) (vector i p1 p2)))
          (range n)
          (path p1)
          (path p2)))