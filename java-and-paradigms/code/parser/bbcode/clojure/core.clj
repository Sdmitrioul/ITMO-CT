(ns bbcode.core)

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

(comment "Node text")

(definterface jNode
  (toHTML []))

(deftype eNode [node text]
  jNode
  (toHTML [this] (str "<" node ">" text"</" node ">")))

(defn toHTML [x] (.toHTML x))

(defn Emphasis [text] (eNode. "em" text))
(defn Strong [text] (eNode. "strong" text))
(defn Strikeout [text] (eNode. "s" text))
(defn Underlined [text] (eNode. "u" text))
(defn _li [text] (eNode. "li" text))
(defn List [elements] (let [parts (mapv #(toHTML (_li %)) elements)
                              s (reduce #(str %1 %2) "" parts)]
                          (eNode. "ul" s)))

(comment "Parser")

(defn -show [result]
  (if (-valid? result)
    (str "->" (pr-str (toHTML (-value result)))
         " | " (pr-str (apply str (-tail result))))
    "!"))

(defn -tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (-show (parser input)))) inputs))

(def *all-chars (mapv char (range 0 2048)))
(def *chars (+char (apply str *all-chars)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))
(def *digit (+char (apply str (filter #(Character/isDigit %) *all-chars))))
(def *space (+char (apply str (filter #(Character/isWhitespace %) *all-chars))))
(def *ws (+ignore (+star *space)))
(defn *seq [begin p end]
  (+seqn 1 (+char begin) (+opt (+seqf cons *ws p (+star (+seqn 1 *ws (+char ",") *ws p)))) *ws (+char end)))

(def functions {"b" Strong
                "i" Emphasis
                "s" Strikeout
                "u" Underlined})

(def *text (+str (+plus (+or (+char-not "[") (+seq (+char "\\") (+char "["))))))

(defn *node [c p] (+map #(toHTML ((functions c) %)) (+seqn 3 (+char "[") (+char c) (+char "]") p (+char "[") (+char "/") (+char c) (+char "]"))))
(defn *b [p] (*node "b" p))
(defn *i [p] (*node "i" p))
(defn *s [p] (*node "s" p))
(defn *u [p] (*node "u" p))

(defn *partList [p] (+plus (+str (+seqn 3 (+char "[") (+char "*") (+char "]") p))))

(defn *list [p]  (+map #(toHTML (List %)) (+seqn 6 (+char "[") (+char "l") (+char "i") (+char "s") (+char "t") (+char "]") (*partList p) (+char "[") (+char "/") (+char "l") (+char "i") (+char "s") (+char "t") (+char "]"))))



(tabulate (*list (+str (+plus (+or *digit *letter *space)))) ["[list][*]fsjfjd nfdi[*]fsjfjd nfdi[*]fsjfjd nfdi[*]fsjfjd nfdi[/list]"
                                             "sjfnjfsi"])


(defn *value [] (delay (+str (+star (+or (*b (*value))
                                         (*i (*value))
                                         (*s (*value))
                                         (*u (*value))
                                         (*list (*value))
                                         *text)))))
(def bbCode (+parser (*value)))
(bbCode "[list][*]Список элементов [*]Список элементов[/list]\n# Заголовок первого уровня\n\n## Второго\n\n### Третьего ## уровня\n\n#### Четвертого\n# Все еще четвертого\n\nЭтот абзац текста,\nсодержит две строки.\n\n  # Может показаться, что это заголовок.\nНо нет, это абзац начинающийся с `#`.\n\n#И это не заголовок.\n\n###### Заголовки могут быть многострочными\n(и с пропуском заголовков предыдущих уровней)\n\nМы все любим *выделять* текст _разными_ способами.\n[i]Сильное выделение[/i], используется гораздо реже,\nно [u]почему бы и нет[/u]?\nНемного [s]зачеркивания[/s] еще ни кому не вредило.\nКод представляется элементом `code`.\n\nОбратите внимание, как экранируются специальные\nHTML-символы и проверочка еще-еще, такие как `<`, `>` и `&`.\n\nЗнаете ли вы, что в Markdown, одиночные * и _\nне означают выделение?\nОни так же могут быть заэкранированы\nпри помощи обратного слэша: \\*.\n\n[list][*]Список элементов [*]Список элементов[/list]\n\nЛишние пустые строки должны игнорироваться.\n\nЛюбите ли вы *вложеные __выделения__* так,\nкак [u][s]люблю[/s] gnjfdgnd [list][*]Список элементов [*]Список элементов[/list] dkgdl [/u] их я?")


(tabulate (force (*value)) ["[list][*]Список элементов [*]Список элементов[/list]"])

