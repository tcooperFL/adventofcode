(ns adventofcode2018.day13
  (:require [clojure.java.io :as io]))

;; Advent of Code challenges
;; https://adventofcode.com/2018/day/13

(def input-data "resources/2018/day13-input.txt")
(def sample-data "resources/2018/day13-sample.txt")
(def sample2-data "resources/2018/day13-sample2.txt")
(def sample3-data "resources/2018/day13-sample3.txt")

(defmulti advance "Advance the cart 1 position and return that new position" (fn [cart] (:dir cart)))
(defmethod advance \> [{[x y] :loc}] [(inc x) y])
(defmethod advance \< [{[x y] :loc}] [(dec x) y])
(defmethod advance \v [{[x y] :loc}] [x (inc y)])
(defmethod advance \^ [{[x y] :loc}] [x (dec y)])
(defmethod advance nil [_] nil)

(defn create-cart
  "We maintain direction and location for each cart"
  [loc dir]
  {:id (gensym "cart") :loc loc :dir dir :turns (cycle '(:left :straight :right))})

(defn loc-comparator
  "Sort comparator function for locations"
  [[x1 y1] [x2 y2]]
  (if (= x1 x2) (compare y1 y2) (compare x1 x2)))

(def faces #{\^ \v \> \<})

(defn create-simulation
  "Parse the input file to consruct a new simulation."
  [file]
  (with-open [rdr (io/reader file)]
    (let [track-pieces
          (for [[y line] (map-indexed vector (line-seq rdr))
                [x piece] (map-indexed vector (seq line))
                :when (not= piece \space)]
            [[x y] piece])]
      {:track      (into {} track-pieces)
       :carts      (reduce (fn [carts [loc piece]]
                             (if (faces piece)
                               (assoc carts loc (create-cart loc piece))
                               carts))
                           (sorted-map-by loc-comparator)
                           track-pieces)
       :iterations 0
       :removed    #{}})))

(def steering
  "Which way we are pointing after encountering a curve or a turn."
  {[\^ \/]    \>, [\^ \\] \<,
   [\v \/]    \<, [\v \\] \>,
   [\< \/]    \v, [\< \\] \^,
   [\> \/]    \^, [\> \\] \v,
   [\^ :left] \<, [\^ :right] \>,
   [\v :left] \>, [\v :right] \<,
   [\< :left] \v, [\< :right] \^,
   [\> :left] \^, [\> :right] \v,})

(defn move-to
  "Move and orient the cart at the given location on this track."
  [{:keys [dir turns] :as cart} new-loc track]
  (let [piece (get track new-loc)]
    (assoc
      (if (= piece \+)
        (merge cart {:dir (get steering [dir (first turns)] dir) :turns (rest turns)})
        (assoc cart :dir (get steering [dir piece] dir)))
      :loc new-loc)))

(defn tick
  "Every cart moves one step. If it moves to an occupied space record a collision and remove the colliders."
  [{:keys [carts track] :as simulation}]
  (reduce (fn [state cart]
            (if (contains? (:removed state) (:id cart))
              state
              (let [cart-map (:carts state)
                    new-loc (advance cart)
                    hit (get cart-map new-loc)]
                (update
                  (if (nil? hit)
                    (assoc state
                      :carts (assoc (dissoc cart-map (:loc cart))
                               new-loc (move-to cart new-loc track)))
                    (assoc state
                      :carts (dissoc cart-map new-loc (:loc cart))
                      :collision [new-loc cart hit]
                      :removed (reduce conj (:removed state) (list (:id cart) (:id hit)))))
                  :iterations inc))))
          simulation
          (vals carts)))

(defn part1
  "Report the location of the first crash"
  [input]
  (let [state (first (drop-while #(not (:collision %)) (iterate tick (create-simulation input))))]
    [(first (:collision state)) :at (:iterations state)]))

;; (part1 input-data)
;; => [76 108]

;; Part 2

(defn part2
  "Tell where the last remaining cart is after all others have crashed."
  [input]
  (->> (create-simulation input)
       (iterate tick)
       (drop-while #(> (count (:carts %)) 1))
       first :carts vals first :loc))

;; (part2 input-data)
;; => [2 84]

