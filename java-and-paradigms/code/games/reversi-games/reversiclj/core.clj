(ns reversi.core)

(comment "Helping functions")
(defn my-or [first second] (not (= false first second)))
(comment "----------------------------------------------")


(def cols '["A" "B" "C" "D" "E" "F" "G" "H"])
(def cols-check (set cols))
(def cords-translate (zipmap cols (range 8)))
(def ve (filterv #(not (= [0 0] %)) (reduce into (mapv #(mapv (fn [x] (vector x %)) (range -1 2)) (range -1 2)))))
(def ee "_")
(def white "W")
(def black "B")
(def opposite {white black
               black white
               ee nil})
(defn ofbe [i] (if (or (> i 8) (< i 1)) "OUT" (nth cols (- i 1))))
(defn goodCoordinates? [c] (and (contains? cols-check (first c)) (> 9 (last c)) (< 0 (last c))))
(defn coordinates [first second] (if (number? first) (vector (ofbe first) second)
                                                     (vector first second)))

(def all-coordinates (reduce into (mapv #(mapv (fn [x] (coordinates x %)) (range 1 9)) (range 1 9))))


(def eL (vector ee ee ee ee ee ee ee ee))
(def fouthLine (vector ee ee ee "B" "W" ee ee ee))
(def fifthLine (vector ee ee ee "W" "B" ee ee ee))
(def emptyBoard (vector eL eL eL fifthLine fouthLine eL eL eL))


(defn showBoard [board] (str (reduce #(str %1 (- 9 %2) " " (clojure.string/join " " (nth board (- %2 1))) "\n")
                                  "" (range 1 9))
                             "  " (clojure.string/join " " cols)))

(defn put [board c s] {:pre [(contains? #{"W" "B"} s)]} (update board (- 8 (last c)) #(update % (cords-translate (first c)) (constantly s))))
(defn get-p [board c] {:pre [(goodCoordinates? c)]} (nth (nth board (- 8 (last c))) (cords-translate (first c))))

(defn cv [c v] (coordinates (+ (first v) 1 (cords-translate (first c))) (+ (last v) (last c))))
(defn dcv [c v] (coordinates (+ (* 2 (first v)) 1 (cords-translate (first c))) (+ (* 2 (last v)) (last c))))
(defn onVector? [board c v s] (if (and (goodCoordinates? c)
                                       (goodCoordinates? (cv c v))
                                       (goodCoordinates? (dcv c v)))
                                (and (= (opposite s) (get-p board (cv c v))) (= s (get-p board (dcv c v))))
                                false))

(defn goodMove? [board c s] (reduce #(or %1 %2) (mapv #(and (= ee (get-p board c)) (onVector? board c % s)) ve)))
(defn paint-v [board c v s] (put (put board (cv c v) s) c s))
(defn paint-move [board c s] (reduce #(if (onVector? %1 c %2 s) (paint-v %1 c %2 s)
                                                                %1) board ve))

(defn haveValidMoves? [board s] (reduce #(or %1 (goodMove? board %2 s)) false all-coordinates))
(defn countCells [board s] (reduce (fn [x y] (+ x (reduce #(+ %1 (if (= s %2) 1 0)) 0 y))) 0 board))

(def UKNOWN "Unkown")
(def WIN "Win")
(def LOSE "Lose")
(def DRAW "Draw")

(defn getResult [board s] (cond (or (haveValidMoves? board s) (haveValidMoves? board (opposite s))) UKNOWN
                                (> (countCells board s) (countCells board (opposite s))) WIN
                                (< (countCells board s) (countCells board (opposite s))) LOSE
                                :else DRAW))

(defn getNext [board s] (if (haveValidMoves? board (opposite s)) (opposite s)
                                                                 s))
(definterface BoardConfiguration
  (getCell [])
  (isValid [arg])
  (toStringM []))

(deftype Configuration [board s]
  BoardConfiguration
  (getCell [this] (.s this))
  (isValid [this arg] (goodMove? (.board this) arg (.s this)))
  (toStringM [this] (showBoard (.board this))))


(definterface Board
  (makeMove [arg])
  (getCell [])
  (getPosition [])
  (isValidMove [arg])
  (getResult [])
  (toStringM []))

(deftype ReversiBoard [board s]
  Board
  (makeMove [this arg] (if (.isValidMove this arg) (let [nb (paint-move (.board this) arg (.s this))]
                                                     (ReversiBoard. nb (getNext nb s)))))
  (getCell [this] (.s this))
  (getPosition [this] (Configuration. (.board this) (.s this)))
  (isValidMove [this arg] (goodMove? (.board this) arg (.s this)))
  (getResult [this] (getResult (.board this) (.s this)))
  (toStringM [this] (showBoard (.board this))))

(definterface Player
  (makeMove [configuration]))

(deftype HumanPlayer []
  Player
  (makeMove [this configuration] (do (println (.toStringM configuration))
                                     (println (str "It's -" (.getCell configuration) " term"))
                                     (println (str "Input your move (column row)"))
                                     (let [input (read-line)
                                           in (if (= 2 (count input)) (clojure.string/split input #"") (clojure.string/split input #"\s"))
                                           cor (coordinates (first in) (read-string (second in)))] cor))))

(deftype StupidPlayer []
  Player
  (makeMove [this configuration] (first (filterv #(.isValid configuration %) all-coordinates))))

(definterface Server
  (run []))

(deftype GameServer [board player1 player2]
  Server
  (run [this] (cond (or (= LOSE (.getResult (.board this))) (= WIN (.getResult (.board this)))) (println (str (.getCell (.board this)) "'s - " (.getResult (.board this))))
                    (= DRAW (.getResult (.board this))) (println (.getResult (.board this)))
                    :else (let [cell  (.getCell (.board this))
                                nboard (.makeMove board (.makeMove player1 (.getPosition (.board this))))]
                            (if (= cell (.getCell nboard)) (.run (GameServer. nboard player1 player2))
                                                           (.run (GameServer. nboard player2 player1)))))))
