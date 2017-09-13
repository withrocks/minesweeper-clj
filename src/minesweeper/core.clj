(ns minesweeper.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper, for when you need an old school Windows 95 break from your work
;;
;; ... or just an exercise in writing in clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example board, vectors of vectors containing field maps (see create-field)

(defn create-field
  "Creates a single field"
  []
  {:bomb false :surrounding 0})

(defn create-row
  [cols]
  (into [] (map (fn [ix] (create-field)) (range cols))))

(defn create-board
  "Creates a rows x cols board"
  [rows cols]
  {:fields (into [] (map (fn [ix] (create-field)) (range (* rows cols))))
   :rows rows
   :cols cols})
(create-board 3 3)

(def dbg true)

(defn field-to-string
  "Shows the field with all info for debug purposes"
  [field]
  (if dbg
    (format "%s%d" (if (:bomb field) "b" "-") 0)
    "x"))

(defn row-to-string
  [row]
  (clojure.string/join "|" (map field-to-string row)))

(defn board-to-string
  [board]
  (let [fields (:fields board)
        rows (:rows board)
        cols (:cols board)
        row-lists (partition cols fields)
        row-lists-str (map row-to-string row-lists)]
    (clojure.string/join "\n" (map row-to-string row-lists))))
(board-to-string (create-board 2 3))

(assert (= (board-to-string (create-board 2 3))
           "-0|-0|-0\n-0|-0|-0"))

(defn ix-to-coord
  "Maps from a index to coordinate"
  [ix rows cols]
  (let [row (int (/ ix cols))
        col (rem ix cols)]
    [row col]))
(map #(ix-to-coord % 3 2) (range (* 3 2))) ; i


(defn random-bombs
  "Returns count number of bomb locations in an row x col board"
  [bombs fields]
  (take bombs (shuffle (range fields))))
(random-bombs 3 9)

(defn place-bombs
  "Given a board, places bombs randomly and returns a new board with those placed"
  [board bombs]
  (let [rows (:rows board)
        cols (:cols board)]
    (reduce (fn [new-board ix]
              (assoc-in new-board [:fields ix :bomb] true))
            board
            bombs)))

(defn game
  []
  (let [rows 9
        cols 9
        bombs 10
        fields (* rows cols)
        board (create-board rows cols)
        board-with-bombs (place-bombs board (random-bombs bombs fields))]
    (println (board-to-string board-with-bombs))))

(defn -main
  "Runs a game of minesweeper, that's all"
  [& args]
  (println "Minesweeper")
  (println "-----------")
  (game))
