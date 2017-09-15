(ns minesweeper.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper, for when you need an old school Windows 95 break from your work
;;
;; ... or just an exercise in writing in clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example board, vectors of vectors containing field maps (see create-field)

(defn create-field
  "Creates a single field. Surrounding count is initialized to zero and is set to a value higher than that
   only if the user steps on a field near it"
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
   :cols cols
   :dead false})

(defn field-to-string
  "Shows the field with all info for debug purposes"
  [field dbg]
  (if dbg
    (format "%s%d%s" (if (:bomb field) "b" "-") (:surrounding field) (if (:stepped field) "s" " "))
    (let [stepped (:stepped field)
          bomb (:bomb field)
          surr (:surrounding field)]
      (if stepped
        (cond bomb "b"
              (> surr 0) (str surr)
              (= surr 0) "#")
        "x"))))

(defn row-to-string
  [row dbg]
  (clojure.string/join "|" (map #(field-to-string % dbg) row)))

(defn board-to-string
  [board dbg]
  (let [fields (:fields board)
        rows (:rows board)
        cols (:cols board)
        row-lists (partition cols fields)
        row-lists-str (map row-to-string row-lists)]
    (clojure.string/join "\n" (map #(row-to-string % dbg) row-lists))))

(defn index-to-coord
  "Maps from a index to coordinate"
  [ix board]
  (let [rows (:rows board)
        cols (:cols board)
        row (int (/ ix cols))
        col (rem ix cols)]
    [row col]))

(defn random-bombs
  "Returns count number of bomb locations in an row x col board"
  [bombs fields]
  (take bombs (shuffle (range fields))))

(defn place-bombs
  "Given a board, places bombs randomly and returns a new board with those placed"
  [board bombs]
  (let [rows (:rows board)
        cols (:cols board)]
    (reduce (fn [new-board ix]
              (assoc-in new-board [:fields ix :bomb] true))
            board
            bombs)))

(defn parse-input
  [line]
  (map read-string (clojure.string/split line #",")))
(parse-input "0,2")

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (parse-input input)))))

(defn make-move
  [board ix]
  ; TODO: Idiomatic way of setting two values on the same object, where one is conditional 
  (let [board (if (get-in board [:fields ix :bomb])
                (assoc board :dead true) board)
        board (assoc-in board [:fields ix :stepped] true)]
    board))

(defn add-coords
  [a b]
  [(+ (get a 0) (get b 0))
   (+ (get a 1) (get b 1))])
(add-coords [1 1] [0 0])

(defn get-surrounding-square
  "Returns the square surrounding this coordinate. Does not filter illegal coordinates"
  [coord]
  (let [deltas (for [x [-1 0 1] y [-1 0 1]] (vector x y))]
    (map #(add-coords % coord) deltas)))
(get-surrounding-square [0 0])

(defn is-out-of-bounds?
  [coord board]
  (let [[row col] coord
        rows (:rows board)
        cols (:cols board)]
    (or (< row 0) (< col 0)
        (>= row rows) (>= col cols))))
(is-out-of-bounds? [3 2] (create-board 3 3))

(defn coord-to-index
  [board coord]
  (let [[row col] coord
        rows (:rows board)
        cols (:cols board)]
    (+ (* row cols) col)))
(coord-to-index (create-board 3 3) [0 0])
(coord-to-index (create-board 3 3) [0 1])
(coord-to-index (create-board 3 3) [2 0])

(defn get-surrounding
  "Returns all fields surrounding this square, excluding those that are out of bounds"
  [board ix]
  (let [rows (:rows board)
        cols (:cols board)
        coord (index-to-coord ix board)]

    (map #(coord-to-index board %) (remove #(is-out-of-bounds? % board)
                                           (get-surrounding-square coord)))))
(get-surrounding (create-board 9 9) 12)

(-> (create-board 3 3)
    (#(place-bombs % [0]))
    (#(make-move % 0))
    (#(get-surrounding % 1)))

(defn game-over?
  [board]
  (:exploded board))

(defn game-over
  [board]
  (println "sorry, game over :(")
  (println (board-to-string board)))

(declare user-entered-valid-move user-entered-invalid-move)

(defn prompt-move
  "Prompts the user for a move. The move is fetched via the input-fn, which should return a board coordinate"
  [input-fn board]
  (println "Current state:")
  (println (board-to-string board))
  (println "Select a field, e.g. 0,0:")
  (let [input (input-fn)]
    (if-let [new-board (make-move board input)]
      (user-entered-valid-move new-board)  ; todo stack?
      (user-entered-invalid-move board))))

(defn user-entered-valid-move
  [board]
  (if (game-over? board)
    (game-over board)
    (prompt-move board)))

(defn user-entered-invalid-move
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn all-moves
  [rows cols]
  (map #(index-to-coord % rows cols) (range (* rows cols))))
(all-moves 3 3)

(defn game
  [prompt-move]
  (let [rows 3
        cols 3
        bombs 3
        fields (* rows cols)
        board (create-board rows cols)
        board (place-bombs board (random-bombs bombs fields))]
    (prompt-move board)))
  ; (let [rows 3
  ;       cols 3
  ;       bombs 3
  ;       fields (* rows cols)
  ;       board (create-board rows cols)
  ;       board (place-bombs board (random-bombs bombs fields))]
  ;   (prompt-move board (fn [] [0, 1]))))
;(game (partial prompt-move (fn [] [0, 1])))

(defn -main
  "Runs a game of minesweeper, that's all"
  [& args]
  (println "Minesweeper")
  (println "-----------")
  (game))
