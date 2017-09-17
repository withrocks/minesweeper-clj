(ns minesweeper.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper, for when you need an old school Windows 95 break from your work
;;
;; ... or just an exercise in writing in clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-field
  "Creates a single field. Surrounding count is initialized to zero"
  []
  {:bomb false :surrounding 0})

(defn create-board
  "Creates a board, modelled as one list. Elements are accessed by index (so element [0,0] is at 0).
   If you need to fetch by coordinates, you can use the functions index-to-coord and coord-to-index."
  [rows cols]
  {:fields (into [] (map (fn [ix] (create-field)) (range (* rows cols))))
   :rows rows
   :cols cols
   :dead false})

(defn field-to-string-main
  [field]
  (let [stepped (:stepped field)
        bomb (:bomb field)
        surr (:surrounding field)]
    (if stepped
      (cond bomb "b"
            (> surr 0) (str surr)
            (= surr 0) "#")
      "x")))

(defn field-to-string
  "Creates textual representation for a field, optionally with debug info"
  [field dbg]
  (let [main-string (field-to-string-main field)]
    (if dbg
      (format "%s:%s%d%s" main-string (if (:bomb field) "b" ".") (:surrounding field) (if (:stepped field) "s" "."))
      main-string)))

(defn row-to-string
  "Generates textual representation of a single row in a board"
  [row dbg]
  (clojure.string/join "|" (map #(field-to-string % dbg) row)))

(defn board-to-string
  "Generates a string representing an entire board"
  [board dbg]
  (let [fields (:fields board)
        rows (:rows board)
        cols (:cols board)
        row-lists (partition cols fields)
        row-lists-str (map row-to-string row-lists)]
    (clojure.string/join "\n" (map #(row-to-string % dbg) row-lists))))

(defn index-to-coord
  "Maps from an index to coordinate, mainly useful for easily generating surrounding coordinates."
  [board ix]
  (let [rows (:rows board)
        cols (:cols board)
        row (int (/ ix cols))
        col (rem ix cols)]
    [row col]))

(defn coord-to-index
  "Maps from coordinates to indexes"
  [board coord]
  (let [[row col] coord
        rows (:rows board)
        cols (:cols board)]
    (+ (* row cols) col)))
(coord-to-index (create-board 3 3) [0 0])
(coord-to-index (create-board 3 3) [0 1])
(coord-to-index (create-board 3 3) [2 0])

(defn random-bombs
  "Returns count number of bomb locations in an row x col board"
  [bombs field-count]
  (take bombs (shuffle (range field-count))))
(random-bombs 3 9)

(defn create-test-board
  []
  "Creates a 2x3 board for basic testing - bombs are always in the same place (position 0)"
  (let [rows 2 cols 3]
    (-> (create-board rows cols)
        (#(place-bombs % [0]))
        (#(apply-surrounding %)))))
(create-test-board)

(defn create-game-board
  [rows cols bomb-count]
  "Creates a board ready to be played on"
  (-> (create-board rows cols)
      (#(place-bombs % (random-bombs bomb-count (* rows cols))))
      (#(apply-surrounding %))))
(create-game-board 3 3 3)

;;;;;;;;;;;;;;;;;;;;
; Logic dealing with surrounding fields and counting of nearby bombs

(defn square-deltas
  "Returns the deltas that form a square around a coordinate on the xy plane"
  []
  (remove #(= % [0 0]) (for [x [-1 0 1] y [-1 0 1]] (vector x y))))
(square-deltas)

(defn get-surrounding-square
  "Returns the square surrounding this coordinate on the xy plane. Includes any potential out-of-bounds coordinates"
  [coord]
  (let [deltas (square-deltas)]
    (map #(add-coords % coord) deltas)))
(get-surrounding-square [0 1])

(defn get-surrounding-square-filtered
  "Returns indexes of fields surrounding this index, excluding those that are out of bounds. Fetch the actual
  fields through get-surrounding-fields"
  [board ix]
  (let [rows (:rows board)
        cols (:cols board)
        coord (index-to-coord board ix)]
    (map #(coord-to-index board %) (remove #(is-out-of-bounds? % board)
                                           (get-surrounding-square coord)))))
(assert (=
         (-> (create-board 2 3)
             (#(place-bombs % [0]))
             (#(get-surrounding-square-filtered % 1)))
         '(0 2 3 4 5)))

; TODO: move from global to an anonymous function
(defn get-tuple
  [board ix]
  [(get-in board [:fields ix]), ix])
(defn get-surrounding-fields
  [board ix]
  (map #(get-tuple board %) (get-surrounding-square-filtered board ix)))
(-> (create-board 2 3)
    (#(place-bombs % [0]))
    (#(get-surrounding-fields % 1)))

(defn count-surrounding
  "Counts bombs surrounding this field"
  [board ix]
  (count (filter #(true? %) (map #(get-in % [0 :bomb]) (get-surrounding-fields board ix)))))
(assert (= 3
           (-> (create-board 2 3)
               (#(place-bombs % [0 2 4]))
               (#(count-surrounding % 1)))))

(defn all-field-indexes
  "Returns all field indexes on a board"
  [board]
  (range (* (:rows board) (:cols board))))

(defn apply-surrounding
  "Given an index into the board, calculates the surrounding bombs and applies that count to this element.
   Without an index, applies the surrounding count to the whole board."
  ([board ix]
   (let [surr (count-surrounding board ix)]
     (assoc-in board [:fields ix :surrounding] surr)))
  ([board]
   (reduce #(apply-surrounding %1 %2)
           board
           (all-field-indexes board))))
(-> (create-board 2 3)
    (#(place-bombs % [0]))
    (apply-surrounding))

;;;;;;;;;;;;;;;;;;;;
; Bomb placement. NOTE: Random

(defn place-bombs
  "Given a board, places bombs at the randomly and returns a new board with those placed"
  [board bombs]
  (let [rows (:rows board)
        cols (:cols board)]
    (reduce (fn [new-board ix]
              (assoc-in new-board [:fields ix :bomb] true))
            board
            bombs)))

;;;;;;;;;;;;;;;;;;
; Screen drawing logic

(defn clear-screen
  "NOTE: This is not portable (terminal specific)"
  []
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H"))) ; move cursor to the top left corner of the screen

(defn dump-board
  "Dumps a board (prints it to the screen) and for convenience, returns the board again, so it can be
  used in e.g. thread macros while debugging"
  ([board dbg]
   (println (board-to-string board dbg))
   (println)
   board)
  ([board] (dump-board board false)))

;;;;;;;;;;;;;;;;;;
; Game board logic, e.g. making a move
(defn step-on-board
  "One step on the board at a particular index. Does not check if you actually can step on the field (caller should
  make sure of that before calling). After this: If there are no surrounding bombs, steps on all surrounding fields
  that haven't been stepped on before. Stops if there are surrounding bombs."
  [board ix]
  (if (= true (get-in board [:fields ix :stepped]))
    board
    (let [board (assoc-in board [:fields ix :stepped] true)]
      (if (not= (get-in board [:fields ix :surrounding]) 0) board
          (let [surr (get-surrounding-fields board ix)
                surr (remove #(get-in % [0 :bomb]) surr)
                surr-indexes (map #(get-in % [1]) surr)]
            (reduce (fn [new-board ix] (step-on-board new-board ix))
                    board
                    surr-indexes))))))
(-> (create-board 4 3)
    (#(place-bombs % [0 7]))
    (apply-surrounding)
    (#(dump-board % false))
    (#(step-on-board % 8))
    (#(dump-board % false)))

;;;;;;;;;;;;;;;;;;
; Input/output logic

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

(defn add-coords
  [a b]
  [(+ (get a 0) (get b 0))
   (+ (get a 1) (get b 1))])
(add-coords [1 1] [0 0])

(defn is-out-of-bounds?
  [coord board]
  (let [[row col] coord
        rows (:rows board)
        cols (:cols board)]
    (or (< row 0) (< col 0)
        (>= row rows) (>= col cols))))
(is-out-of-bounds? [3 2] (create-board 3 3))

(defn get-fields
  [indexes]
  (map #(get-in board [:fields %]) indexes))

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
