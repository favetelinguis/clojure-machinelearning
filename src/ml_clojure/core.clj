(ns ml-clojure.core
  (:require
   [iota :as iota]
   [clojure.java.io :as io]
   [clojure.core.reducers :as r]
    [tesser.core :as t]
            [tesser.math :as m]
            [incanter.core :as i]
            [incanter.optimize :as o]
            [incanter.charts :as c]
            [incanter.io :as iio]
            [criterium.core :as cri]
            [clojure.string :as str]))

(defn parse-double [x]
  (Double/parseDouble x))

(defn parse-line [line]
  (let [[text-fields double-fields] (->> (str/split line #",")
                                         (split-at 2))]
    (concat text-fields
            (map parse-double double-fields))))

(def line-formatter
  (r/map parse-line))

(defn record-formatter [column-names]
  (r/map (fn [fields]
           (zipmap column-names fields))))

(def remove-zero-zip
  (r/remove (fn [record]
              (zero? (:zipcode record)))))

(defn parse-columns [line]
  (->> (str/split line #",")
       (map keyword)))

(defn chunks [coll]
  (->> (into [] coll)
       (t/chunk 1024)))

(defn format-record [column-names line]
  (zipmap column-names line))

(defn prepare-data []
  (->> (t/remove #(.startsWith % "STATEFIPS"))
       (t/map parse-line)
       (t/map (partial format-record column-names))
       (t/remove #(zero? (:zipcode %)))))

;; The gradient decent update rule
;; To apply this function we need to know the gradient of the cost function
;; with the current parameters.
;; We also need feature scaling
(defn feature-scales [features]
  (->> (prepare-data)
       (t/map #(select-keys % features))
       (t/facet)
       (t/fuse {:mean (m/mean)
                :sd (m/standard-deviation)})))

(defn feature-matrix [record features]
  (let [xs (map #(get record %) features)]
    (i/matrix (cons 1 xs)))) 

(defn extract-features [fy features]
  (fn [record]
    {:y  (fy record)
     :xs (feature-matrix record features)})) 

(defn scale-features [factors]
  (let [f (fn [x {:keys [mean sd]}]
            (/ (- x mean) sd))]
    (fn [x]
      (merge-with f x factors)))) 

(defn unscale-features [factors]
  (let [f (fn [x {:keys [mean sd]}]
            (+ (* x sd) mean))]
    (fn [x]
      (merge-with f x factors))))

(defn calculate-error
  "Given the transposed coefficients vector a functions is returned that
  will calculate (y-hat - y)x"
  [coefs-t]
  (fn [{:keys [y xs]}]
    (let [y-hat (first (i/mmult coefs-t xs))
          error (- y-hat y)]
      (i/mult xs error))))

(defn matrix-mean [nrows ncols]
  (let [zeros-matrix (i/matrix 0 nrows ncols)]
    {:reducer-identity (constantly [zeros-matrix 0])
     :reducer (fn [[sum counter] x]
                [(i/plus sum x) (inc counter)])
     :combiner-identity (constantly [zeros-matrix 0])
     :combiner (fn [[sum-a count-a] [sum-b count-b]]
                 [(i/plus sum-a sum-b)
                  (+ count-a count-b)])
     :post-combiner (fn [[sum count]]
                      (i/div sum count))}))

(defn update-coefficients [coefs alpha]
  (fn [cost]
    (->> (i/mult alpha cost)
         (i/minus coefs))))

(defn gradient-descent-fold
  "Performe one step of gradient decent"
  [{:keys [fy features factors coefs alpha]}]
  (let [zeros-matrix (i/matrix 0 (count features) 1)]
    (->> (prepare-data)
         (t/map (scale-features factors))
         (t/map (extract-features fy features))
         (t/map (calculate-error (i/trans coefs)))
         (t/fold (matrix-mean (inc (count features)) 1))
         (t/post-combine (update-coefficients coefs alpha)))))

(defn descend [options data]
  (fn [coefs]
    (->> (gradient-descent-fold (assoc options :coefs coefs))
         (t/tesser data))))

(defn stochastic-gradient-descent [options data]
  (let [batches (->> (into [] data)
                     (shuffle)
                     (partition 250))
        descend (fn [coefs batch]
                  (->> (gradient-descent-fold
                        (assoc options :coefs coefs))
                       (t/tesser (chunks batch))))]
    (reductions descend (:coefs options) batches)))

;;;;;;;;;;;;;;;;;;;
(defn ex-iterations-bgd []
  (let [features [:A00200 :AGI_STUB :NUMDEP :MARS2]
        fcount   (inc (count features))
        coefs    (vec (replicate fcount 0))
        data     (chunks (iota/seq "resources/soi-sample.csv"))
        factors  (->> (feature-scales features)
                      (t/tesser data))
        options {:fy :A02300 :features features
                 :factors factors :coefs coefs :alpha 0.1}
        iterations 100
        xs (range iterations)
        ys (->> (iterate (descend options data) coefs)
                (take iterations))]
    (-> (c/xy-plot xs (map first ys)
                   :x-label "Iterations"
                   :y-label "Coefficient")
        (c/add-lines xs (map second ys))
        (c/add-lines xs (map #(nth % 2) ys))
        (c/add-lines xs (map #(nth % 3) ys))
        (c/add-lines xs (map #(nth % 4) ys))
        (i/view))))

;; use partition and map to SGD how to abort early and have a max of itterations
;; notworking try to fix!
(defn ex-sgd []
  (let [features [:A00200 :AGI_STUB :NUMDEP :MARS2]
        fcount   (inc (count features))
        coefs    (vec (replicate fcount 0))
        data     (chunks (iota/seq "resources/soi.csv"))
        factors  (->> (feature-scales features)
                      (t/tesser data))
        options  {:fy :A02300 :features features
                  :factors factors :coefs coefs :alpha 1e-3}
        ys       (stochastic-gradient-descent options data)
        xs       (range (count ys))]
    (-> (c/xy-plot xs (map first ys)
                   :x-label "Iterations"
                   :y-label "Coefficient")
        (c/add-lines xs (map #(nth % 1) ys))
        (c/add-lines xs (map #(nth % 2) ys))
        (c/add-lines xs (map #(nth % 3) ys))
        (c/add-lines xs (map #(nth % 4) ys))
        (i/view))))
