(ns minesweeper.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minesweeper, for when you need an old school Windows 95 break from your work
;;
;; ... or just an exercise in writing in clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: Use loop/recur instead of regular recursion

(defn create-field
  "Creates a single field. Surrounding count is initialized to zero"
  []
  {:bomb false :surrounding 0 :stepped false :marked false})

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
        surr (:surrounding field)
        marked (:marked field)]
    (if marked "m"
        (if stepped
          (cond bomb "b"
                (> surr 0) (str surr)
                (= surr 0) " ")
          "#"))))

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
  (str "*" (clojure.string/join "|" (map #(field-to-string % dbg) row)) "*"))

(defn board-to-string
  "Generates a string representing an entire board"
  ([board dbg]
   (let [fields (:fields board)
         rows (:rows board)
         cols (:cols board)
         row-lists (partition cols fields)
         row-lists-str (map row-to-string row-lists)
         up-down-border (apply str (repeat (* 2 cols) "*"))]
     (clojure.string/join "\n" (map #(row-to-string % dbg) row-lists))))
  ([board]
   (board-to-string board false)))
(board-to-string (create-game-board 3 3 3))

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

(def create-beginner-board (partial create-game-board 9 9 10))
(def create-intermediate-board (partial create-game-board 16 16 40))
(def create-expert-board (partial create-game-board 16 30 99))

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

(defn get-surrounding-square-indexes
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
             (#(get-surrounding-square-indexes % 1)))
         '(0 2 3 4 5)))

(defn get-surrounding-fields
  [board ix]
  (map (fn [ix] [(get-in board [:fields ix]), ix]) (get-surrounding-square-indexes board ix)))
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
(assert (= "x:b0.|x:.1.\nx:.1.|x:.1."
           (-> (create-board 2 2)
               (#(place-bombs % [0]))
               (apply-surrounding)
               (#(board-to-string % true)))))

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

(defn random-bombs
  "Returns count number of bomb locations in an row x col board"
  [bombs field-count]
  (take bombs (shuffle (range field-count))))
(random-bombs 3 9)

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
  ([board dbg msg]
   (println msg)
   (println (board-to-string board dbg))
   (println)
   board)
  ([board] (dump-board board false "Board state")))

;;;;;;;;;;;;;;;;;;
; Input/output logic

(defn parse-coord-input
  "Parses a string of comma separated integers into a tuple of integers"
  [line]
  (map read-string (clojure.string/split line #",")))
(parse-coord-input "0,2")

(defn get-input
  "Get's input from the user, returning the default value if empty"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input) default input))))

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

(defn game-over
  [board]
  (println "sorry, game over :(")
  (println (board-to-string board)))

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

;;;;;;;;;;;;;;;;;;
; Game board logic
(defn add-mark
  "Adds a mark on the board at the index. This is a flag added by the user, indicating that he thinks there is a bomb at this location"
  [board ix]
  (assoc-in board [:fields ix :marked] true))
(-> (create-game-board 9 9 10)
    (#(dump-board % false "No move made"))
    (#(add-mark % 3))
    (#(dump-board % false "Created a mark at 3"))
    (#(step-on-board % 8))
    (#(dump-board % false "After stepping on index 8"))
    ((fn [b] nil)))

(defn step-on-board
  "One step on the board at a particular index. Does not check if you actually can step on the field (caller should
  make sure of that before calling). After this: If there are no surrounding bombs, steps on all surrounding fields
  that haven't been stepped on before. Stops if there are surrounding bombs."
  [board ix]
  (if (or (get-in board [:fields ix :stepped]) (get-in board [:fields ix :marked]))  ; TODO: add bomb condition here for simplicity
    board
    (let [board (assoc-in board [:fields ix :stepped] true)]
      (if (not= (get-in board [:fields ix :surrounding]) 0) board
          (let [surr (get-surrounding-fields board ix)
                surr (remove #(get-in % [0 :bomb]) surr)
                surr-indexes (map #(get-in % [1]) surr)]
            (reduce (fn [new-board ix] (step-on-board new-board ix))
                    board
                    surr-indexes))))))
(-> (create-beginner-board)
    (#(dump-board % false "No move made"))
    (#(step-on-board % 8))
    (#(dump-board % false "After stepping on index 8"))
    (#(step-on-board % 36))
    (#(dump-board % false "After stepping on index 36"))
    ((fn [b] nil)))

(defn game-over?
  [board]
  (:exploded board))

(defn select-board
  []
  (print "Select board, (b)eginner/(i)ntermediate/(e)xpert: ")
  (let [input (get-input)]
    (cond (= input "b") (create-beginner-board)
          (= input "i") (create-intermediate-board)
          (= input "e") (create-expert-board)
          :else nil)))

(defn random-words-of-wisdom
  [] "Tread safely, there are bombs about!")

(defn get-selected-action
  "Returns either the step function or mark function, based on the user's input"
  []
  (println "Action, (s)tep*, (m)ark: ")
  (let [input (get-input "s")]
    (if (= "s" input) step-on-board add-mark)))

(defn get-selected-index
  [board]
  (println "Where do you want to move, e.g. 0,0?")
  ; TODO: Error handling
  (coord-to-index board (parse-coord-input (get-input))))

(defn play-round
  [board]
  (println (random-words-of-wisdom))
  (println (board-to-string board))
  (play-round
   ((get-selected-action) board (get-selected-index board)))
  ; TODO: check game-over condition
)

(defn game
  "Shows the board, asks for input, makes that move, then calls itself recursively until game-over"
  []
  ;(let [board (select-board)]; TODO: add back in
  (let [board (create-beginner-board)]
    (if (= board nil) (println "Thank you, come again!")
        (play-round board))))

(defn -main
  "Runs a game of minesweeper, that's all"
  [& args]
  (println "Minesweeper")
  (println "-----------")
  (game))
