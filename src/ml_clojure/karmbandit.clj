(ns ml-clojure.karmbandit
  (:require
   [tesser.core :as t]
   [tesser.math :as m]
   [clojure.core.matrix :as x]
   [criterium.core :as cri]
   [incanter.core :as i]
   [incanter.stats :as s]
   [incanter.distributions :as dist]
   [iota :as iota]
   [incanter.charts :as c]))

(x/set-current-implementation :vectorz) 

(defn k-actions
  "Returns a vector of k random (gaussian) variables with
  a mean randomly selected from standard normal distribution
  and unit variance"
  [k]
  (let [ks (s/sample-normal k :mean 0 :sd 1)]
    (for [mean ks]
      (dist/normal-distribution mean 1))))

(defn init-action [a]
  (let [val (dist/draw a)]
    {:k a
     :high val
     :sum val
     :mean val
     :steps 1}))

(defn update-explore
  "Tries a random actions and resort the list of actions, makes sure the highes action is first"
  [action count]
  (let [{:keys [k high sum mean]} action
        reward (dist/draw k)
        new-high (if (> reward high) reward high)
        new-sum (+ sum reward)
        new-mean (/ new-sum count)]
    (merge action {:high new-high
                   :sum new-sum
                   :mean new-mean})))

(defn explore [actions count]
  (update actions 0 (fn [action] (update-explore action count))))

(defn update-exploit [action count]
  (let [{:keys [high sum mean]} action
        new-sum (+ sum high)
        new-mean (/ new-sum count)]
    (merge action {:sum new-sum
                   :mean new-mean})))

(defn exploit
  "Assume list is sorted with higest action first, performes the first action"
  [actions count]
  (->> (update actions (rand-int (count actions)) (fn [action] (update-exploit action count)))
       (sort-by :high)))

(defn one-step [dist]
  "dist a binominal dist for selecting to explore or exploit"
  (fn [acc count] ;; a reduce function
    (let [explore? (= 1 (dist/draw dist))
          last-update (peek acc)]
      (if explore?
        (conj acc (explore last-update count))
        (conj acc (exploit last-update count))))))

(defn run-simulation
  "Run k arms simulation for k bandits
  e is the % of times to explore"
  [k e]
  (let [num-actions k
        explore-exploit-dist (dist/binomial-distribution 1 e)
        reduce-fn (one-step explore-exploit-dist)]
    (->> (k-actions num-actions)
         (map init-action)
         #(reduce reduce-fn [%] (range 1 5))
         (take 10))))

;; Plot average reward for each e value as a function of number of steps
;; Basic vector stuff
(def v (x/array [1 2 3])) 

;; Basic matrix stuff

(defn myfold [nrows ncols]
  (let [zero-matrix (x/zero-matrix nrows ncols)]
    {:reducer-identity (constantly zero-matrix)
     :reducer (fn [s v] (x/conjoin s v))
     :combiner-identity (constantly [])
     :combiner (fn [s v] (conj s v))
     })) 

(defn sum-fold [nrows ncols]
  (let [zeros-matrix (x/zero-matrix nrows ncols)]
    {:identity  (constantly zeros-matrix)
     :reducer           x/add
     })) 

(defn mean-fold [nrows ncols]
  (let [zeros-matrix (x/zero-matrix nrows ncols)]
    {:identity  (constantly [zeros-matrix 0])
     :reducer   (fn [[sum counter] x]
                  [(x/add sum x) (inc counter)])
     })) 

(defn rand-square-matrix [n]
  (x/matrix (repeatedly n #(map rand-int (repeat n 100))))) 

(defn run-sum-columns []
  (let [M  (rand-square-matrix 3000000)
        ncols (x/column-count M)
        nrows 2]
    (->> (t/fold (sum-fold 1 ncols))
         (t/tesser (t/chunk nrows M))))) 
