(ns knf.core)

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _show [result]
  (if (-valid? result)
    (str "->" (pr-str (-value result))
         " | " (pr-str (apply str (-tail result))))
    "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p] (fn [[c & cs]] (if (and c (p c)) (-return c cs))))
(defn _map [f result] (if (-valid? result) (-return (f (-value result)) (-tail result))))
(defn _combine [f a b] (fn [str]
                         (let [ar ((force a) str)]
                           (if (-valid? ar)
                             (_map (partial f (-value ar))
                                   ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000}))
             (str input \u0000)))))

(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps]
  (reduce _either p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +str [p] (+map (partial apply str) p))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))



(defn return-e [number] (cond (= 0 number) false
                              (= false number) false
                            :else true))
(defn return-s [number] (cond (= 0 number) 0
                              :else 1))
(defn my-and [first second] (= true first second))
(defn my-or [first second] (not (= false first second)))
(defn operToString [f & operands] (cond (= 1 (count operands)) (str f " " (first operands))
                                        :else (clojure.string/join (str " " f " ") operands)))




(definterface jInterface
  (jEvaluate [args])
  (jLevel [])
  (pushNegate [arg])
  (getVariables [arg])
  (jToString []))

(declare Negate Conjunction Disjunction)

(deftype jConstant [value]
  jInterface
  (jEvaluate [this _] (return-e (.value this)))
  (jLevel [this] 1)
  (pushNegate [this arg] (cond (= arg true) (Negate this)
                               :else this))
  (getVariables [this arg] arg)
  (jToString [this] (str (return-s (.value this)))))

(defn Constant [value] (jConstant. value))

(deftype jVariable [name]
  jInterface
  (jEvaluate [this args] (return-e (args (.name this))))
  (jLevel [this] 1)
  (pushNegate [this arg] (cond (= arg true) (Negate this)
                               :else this))
  (getVariables [this arg] (conj arg (.name this)))
  (jToString [this] (str (.name this))))

(defn Variable [name] (jVariable. name))

(deftype jOperaion [f f-s level negate operands]
  jInterface
  (jEvaluate [this args] (apply f (map #(.jEvaluate % args) (.operands this))))
  (jLevel [this] (.level this))
  (pushNegate [this arg] ((.negate this) arg))
  (getVariables [this arg] (apply clojure.set/union (map #(.getVariables % arg) (.operands this))))
  (jToString [this] (cond (= 1 (count (.operands this))) (if (<= (.jLevel (first (.operands this))) (.level this)) (str f-s (.jToString (first (.operands this))))
                                                                                                                   (str f-s  "(" (.jToString (first (.operands this))) ")"))
                          :else (clojure.string/join (str " " f-s " ") (map #(if (<= (.jLevel %) (.level this)) (.jToString %)
                                                                                                                (str "(" (.jToString %) ")")) (.operands this)))))
  )

(defn Conjunction [& args] {:pre (= 2 (count args))} (jOperaion. my-and "&" 2
                                                                 (fn [arg] (cond (= arg true) (Disjunction (.pushNegate (first args) arg) (.pushNegate (second args) arg))
                                                                                 :else (Conjunction (.pushNegate (first args) arg) (.pushNegate (second args) arg))
                                                                                 ))
                                                                 args))

(defn Disjunction [& args] {:pre (= 2 (count args))} (jOperaion. my-or "|" 3
                                                                 (fn [arg] (cond (= arg true) (Conjunction (.pushNegate (first args) arg) (.pushNegate (second args) arg))
                                                                                 :else (Disjunction (.pushNegate (first args) arg) (.pushNegate (second args) arg))
                                                                                 ))
                                                                 args))

(defn Negate [& args] {:pre (= 1 (count args))} (jOperaion. not "~" 1
                                                            (fn [arg] (.pushNegate (first args) (not arg)))
                                                            args))

(defn toString [this] (.jToString this))
(defn evaluate [this args] (.jEvaluate this args))
(defn pushNegation [this] (.pushNegate this false))
(defn getVariables [this] (.getVariables this #{}))

(def example (Conjunction (Constant 1) (Disjunction (Variable "x") (Negate (Conjunction (Negate (Variable "y")) (Negate (Constant 1)))))))
(toString example)
(toString (pushNegation example))
(evaluate example {"x" true
                   "y" false})


(defn -show [result]
  (if (-valid? result)
    (str "->" (pr-str (toString (-value result)))
         " | " (pr-str (apply str (-tail result))))
    "!"))

(defn -tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (-show (parser input)))) inputs))

(def *all-chars (mapv char (range 0 128)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))
(def *digit (+char "01"))
(def *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars))))
(def *ws (+ignore (+star *space)))
(defn *seq [begin p end]
  (+seqn 1 (+char begin) (+opt (+seqf cons *ws p (+star (+seqn 1 *ws (+char ",") *ws p)))) *ws (+char end)))

(def *constant (+map #(Constant (read-string %)) (+map str (+seqn 0 *ws *digit))))
(-tabulate *constant [" xy" "x 1" " 0" "01"])

(def *variable (+map #(Variable %) (+str (+seqn 0 *ws (+plus *letter)))))
(-tabulate *variable [" x y" "x 1" " 0" "01"])

(defn *negation [p] (+map #(Negate %) (+seqn 1 *ws (+char "~") p)))
(-tabulate (*negation *constant) ["~x"
                                  "1"
                                  "0"
                                  "~0"
                                  "~1"
                                  "x&y"
                                  "x|y"])

(defn *brackets [p] (+seqn 1 *ws (+char "(") *ws p *ws (+char ")")))
(-tabulate (*brackets *variable) ["(xty)"
                                  "x"
                                  "(1)"
                                  "1"
                                  "0"
                                  "~0"
                                  "~1"
                                  "x&y"
                                  "x|y"])

(defn *absLevel [f c p] (+or (+map #(reduce f %) (+seqf cons p (+plus (+seqn 1 *ws (+char c) p)))) p))
(-tabulate (*absLevel Conjunction "&" *variable) ["(xty)"
                                  "x"
                                  "(1)"
                                  "1"
                                  "0"
                                  "~0"
                                  "~1"
                                  "x&y"
                                  "x|y"])

(defn *firstLevel [] (delay (+or *constant *variable (*negation (*firstLevel)))))

(defn *value []
  (delay (*absLevel Disjunction "|"
                    (*absLevel Conjunction "&"
                               (+or *constant
                                    *variable
                                    (*brackets (*value))
                                    (*negation (+or (*firstLevel)
                                                    (*brackets (*value))
                                                    ))
                                    )))))

(-tabulate (force (*value))
           ["x"
            "x&y"
            "x|y"
            "x|t&z"
            "x&0&1&y"
            "1&(x|y|1)"
            "1& ( x | y | 1 )"
            "1 & (x)"
            "1 & (x | 1)"
            "1 & (x |~1)"
            "~(~y)"
            "~(t|(x&y))"
            "(~(x))"
            "(x|~(x|u))"
            "(~a)"
            "(~(s))"
            "~(~(z))"
            "~(~(x|y))"
            "1&(x|~(~y&~1))"
            "1 & (x | ~(~y))"
            "1 & (x | ~( ~y))"
            "1 & (x | ~( ~y ) )"
            "1 & (x | ~(~y) )"
            "(~x|r)"
            "~~r"
            "~~~r"
            "1 & (x | ~(~y & ~1))"])

(def boolExpr (+parser (+seqn 0 *ws (*value))))
(toString (boolExpr "1 & (x | ~(~y & ~1))"))

(defn generate [n pos] (if (= n (count pos)) (vector pos)
                                             (reduce conj (generate n (conj pos 0)) (generate n (conj pos 1)))))

(defn scnf [expreession] (let [vars (vec (getVariables expreession))
                               n (count vars)
                               vec (generate n [])]
                           (letfn [(ms [k v] (if (= 1 (first v)) (str "~" (first k))
                                                                 (first k)))
                                   (makeString [k v] (if (= 1 (count k)) (ms k v)
                                                                         (str (ms k v) " | " (makeString (rest k) (rest v)))))
                                   (mConj [arg] (if (false? (evaluate expreession (zipmap vars arg))) (str "(" (makeString vars arg) ")")))]
                             (clojure.string/join " & " (filterv #(not (nil? %)) (mapv #(mConj %) vec)))))
  )

(def fi (boolExpr "x|t&z"))