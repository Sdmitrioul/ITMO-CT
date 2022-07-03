(ns multisetclj.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn _equals [comparator] (fn [x y] (comparator x y)))

(definterface jPair
  (getF [])
  (getS [])
  (setF [arg]))

(deftype Pair [first second]
  jPair
  (getF [this] (.first this))
  (getS [this] (.second this))
  (setF [this arg] (Pair. (.first this) arg)))

(defn nPair [x] (Pair. x 1))
(defn nlPair [x y] (Pair. x y))


(def +eqPair (_equals #(if (and (= (.getF %1)  (.getF %2)) (= (.getS %1) (.getS %2))) true)))
(def ncom (fn [x y] (cond (= x y) 0
                          (< x y) -1
                          :else 1)))

(definterface jSet
  (add [arg])
  (remove [arg])
  (intersect [arg])
  (union [arg])
  (equal [arg])
  (toStr []))

(deftype jMSet [comp mas]
  jSet
  (add [this arg] (letfn [(ad [ch i v res p] (cond (= i (count v)) (if (true? p) (conj res (nPair ch))
                                                                                 res)
                                                   (and (< i (count v)) (= 0 ((.comp this) ch (.getF (nth v i))))) (ad ch (+ 1 i) v (conj res (.setF (nth v i) (+ 1 (.getS (nth v i))))) false)
                                                   (and (< i (count v)) (= -1 ((.comp this) ch (.getF (nth v i))))) (if (true? p) (ad ch (+ 1 i) v (conj res (nPair ch) (nth v i)) false)
                                                                                                                                  (ad ch (+ 1 i) v (conj res (nth v i)) false))
                                                   (and (< i (count v)) (= 1 ((.comp this) ch (.getF (nth v i))))) (ad ch (+ 1 i) v (conj res (nth v i)) p)
                                                   :else (vector (nPair 666))))] (jMSet. (.comp this) (ad arg 0 (.mas this) '[] true))))
  (remove [this arg] (jMSet. (.comp this) (reduce #(cond (= 0 ((.comp this) arg (.getF %2))) %1
                                                         :else (conj %1 %2)) '[] (.mas this))))
  (intersect [this arg] (letfn [(inter [i j v1 v2 res] (if (and  (< i (count v1)) (< j (count v2)))
                                                         (cond (= 0 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter (+ 1 i) (+ 1 j) v1 v2 (conj res (nlPair (.getF (nth v1 i)) (min (.getS (nth v1 i)) (.getS (nth v2 j))))))
                                                               (= -1 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter (+ 1 i) j v1 v2 res)
                                                               (= 1 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter i (+ j 1) v1 v2 res))
                                                         res))] (jMSet. (.comp this) (inter 0 0 (.mas this) (.mas arg) '[]))))
  (union [this arg] (letfn [(inter [i j v1 v2 res] (cond (and  (< i (count v1)) (< j (count v2)))
                                                         (cond (= 0 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter (+ 1 i) (+ 1 j) v1 v2 (conj res (nlPair (.getF (nth v1 i)) (+ (.getS (nth v1 i)) (.getS (nth v2 j))))))
                                                               (= -1 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter (+ 1 i) j v1 v2 (conj res (nth v1 i)))
                                                               (= 1 ((.comp this) (.getF (nth v1 i)) (.getF (nth v2 j)))) (inter i (+ j 1) v1 v2 (conj res (nth v2 j))))
                                                         (< i (count v1)) (inter (+ 1 i) j v1 v2 (conj res (nth v1 i)))
                                                         (< j (count v2)) (inter i (+ 1 j) v1 v2 (conj res (nth v2 j)))
                                                         :else res))] (jMSet. (.comp this) (inter 0 0 (.mas this) (.mas arg) '[]))))
  (equal [this arg] (cond (not (= (.comp this) (.comp arg))) false
                          (not (= (count (.mas this)) (count (.mas arg)))) false
                          :else (reduce #(and %1 %2) true (map #(and (= (.getS %1) (.getS %2)) (= 0 (comp (.getF %1) (.getF %2)))) (.mas this) (.mas arg)))))
  (toStr [this] (str "[" (clojure.string/join ", " (mapv #(str (.getF %) ": " (.getS %)) (.mas this))) "]")))

(def a (jMSet. ncom '[]))

(defn ConstructSet [& args] (reduce #(.add %1 %2) (jMSet. ncom '[]) args))

(def b (ConstructSet 1 2 3 4 5 2 3 45 1 7 8 9))
(def g (ConstructSet 3 4 5 2 1 2 4))

