(ns brainfuckclj.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def COMMANDSLIMIT 100000)
(def MEMORYLIMIT 1000)

(defn _out [x] (print (str (char x))))
(defn _in [mem memPoint j input] (update mem memPoint #(get input j)))

(defn createMas [i mas] (if (= i 0) mas (createMas (- i 1) (conj mas 0))))

(def EmptyMem (createMas MEMORYLIMIT '[]))

(defn _checkSize [string] (when-not (< (count string) COMMANDSLIMIT)
                            (let [message (format "Exception: too many commands, get: %s (MAX: %s)"
                                                  (count string)
                                                  COMMANDSLIMIT)]
                              (throw (new Exception message)))) true)

(defn _checkMem [i f] (when-not (< (f  i) MEMORYLIMIT)
                      (let [message (format "Exception: out of memory, index: %s (MIN: 0; MAX: %s)"
                                            (f i)
                                            MEMORYLIMIT)]
                        (throw (new Exception message)))) (f i))

(defn _checkWhile [list] (when-not (= (count list) 0)
                        (let [message (format "Exception: programs end's, but list not: last element: %s; list size %s;"
                                              (last list)
                                              (count list))]
                          (throw (new Exception message)))) true)

(defn  _exception [i] (let [message (format "Exception: something wrong at index: %s"
                                           i)]
                       (throw (new Exception message))))

(defn run [i mem memPoint inPoint program j input list] (cond (>= i (count program)) (_checkWhile list)
                                                              (= ">" (str (get program i))) (run (+ 1 i) mem (_checkMem memPoint inc) inPoint program j input list)
                                                              (= "<" (str (get program i))) (run (+ 1 i) mem (_checkMem memPoint dec) inPoint program j input list)
                                                              (= "+" (str (get program i))) (run (+ 1 i) (update mem memPoint inc) memPoint inPoint program j input list)
                                                              (= "-" (str (get program i))) (run (+ 1 i) (update mem memPoint dec) memPoint inPoint program j input list)
                                                              (= "." (str (get program i))) (do (_out (nth mem memPoint))
                                                                                          (run (+ 1 i) mem memPoint inPoint program j input list))
                                                              (= "," (str (get program i))) (run (+ 1 i) (_in mem memPoint j input) memPoint inPoint program (+ 1 j) input list)
                                                              (= "[" (str (get program i))) (run (+ 1 i) mem memPoint inPoint program j input (conj list i))
                                                              (= "]" (str (get program i))) (if (= 0 (nth mem memPoint)) (run (+ 1 i) mem memPoint inPoint program j input (butlast list))
                                                                                                                   (run (last list) mem memPoint inPoint program j input (butlast list)))
                                                              :else (_exception i)))

(defn runProgam [program input] (run 0 EmptyMem 0 0 program 0 input '[]))

(def pro "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.-----------------------.")
(def pro2 "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
(def in "")