;; Tic-Tac-Toe
;; Created by: Benjamin M. Singleton
;; Created: 04-24-2017
(ns tic-tac-toe.core
  (:gen-class))

(defn other-player
  "Returns the opposite of the given player (\"o\" or \"x\"), or \"x\" by default."
  ([]
    "x")
  ([player]
    (get {"x" "o" "o" "x"} player)))

(defn get-row-number
  "Get the row number (0-2) of a position (0-8)."
  [position]
  (int (/ position 3)))

(defn get-col-number
  "Get the column number (0-2) of a position (0-8)."
  [position]
  (mod position 3))

(defn extract-row
  "Return a list of elements on the specified row."
  [board row-number]
  (let [row-start (* row-number 3)]
    (map #(get board %) [row-start (+ 1 row-start) (+ 2 row-start)])))

(defn extract-col
  "Return a list of elements on the specified column."
  [board col-number]
  (map #(get board %) [col-number (+ 3 col-number) (+ 6 col-number)]))

(defn extract-left-right-diagonal
  "Return a list of elements in the diagonal from the upper-left to the lower-right."
  [board]
  (map #(get board %) [0 4 8]))

(defn extract-right-left-diagonal
  "Return a list of elements in the diagonal from the lower-left to the upper-right."
  [board]
  (map #(get board %) [6 4 2]))

(defn won-row
  "Check if a row is filled only with one player's pieces. Returns winning player letter or nil."
  [board row-number]
  (let [my-row (extract-row board row-number)]
    (if (every? #(= (first my-row) %) my-row)
    	(first my-row))))

(defn won-column
  "Check if a column is filled only with one player's pieces. Returns winning player letter or nil."
  [board col-number]
  (let [my-col (extract-col board col-number)]
    (if (every? #(= (first my-col) %) my-col)
    	(first my-col))))

(defn won-left-right-diagonal
  "Check if upper-left to lower-right diagonal is filled only with one player's pieces. Returns winning player letter or nil."
  [board]
  (let [my-diagonal (extract-left-right-diagonal board)]
    (if (every? #(= (first my-diagonal) %) my-diagonal)
    	(first my-diagonal))))

(defn won-right-left-diagonal
  "Check if lower-left to upper-right diagonal is filled only with one player's pieces. Returns winning player letter or nil."
  [board]
  (let [my-diagonal (extract-right-left-diagonal board)]
    (if (every? #(= (first my-diagonal) %) my-diagonal)
    	(first my-diagonal))))

(defn valid-move?
  "Check if a move to position is valid."
  [board position]
  (= nil (get board position)))

(defn winning-move
  "Check if the game is over based on the last move, made to position."
  [board position]
  (or (won-row board (get-row-number position))
      (won-column board (get-col-number position))
      (won-left-right-diagonal board)
      (won-right-left-diagonal board)))

(defn game-tied
  "Check if every position is filled, without checking for three in a row anywhere."
  [board]
  (every? #(not (= nil %)) (vals board)))

(defn build-board
  "Build an empty board, consisting of a map of keys 0-8, with all values set to nil."
  []
  (loop [iteration 0 board {}]
    (if (= iteration 9)
      board
      (recur (inc iteration) (assoc board iteration nil)))))

(defn make-move
  "Place player's piece at position on board, or return nil."
  [board position player]
  (if (or (< position 0) (> position 8))
      nil
      (if (= nil (get board position))
      	  (assoc board position player)
      	  nil)))

;; User interface code

(defn render-position
  "Return the letter corresponding to position, with \"_\" as an empty space."
  [board position]
  (if (= nil (get board position))
      "_"
      (get board position)))

(defn render-row
  "Return the string representing a given row, with one space between each position."
  [board row-number]
  (let [row-start (* row-number 3)]
       (str (render-position board row-start)
       	    " "
	    (render-position board (+ 1 row-start))
	    " "
	    (render-position board (+ 2 row-start)))))
	    
(defn print-board
  "Print out a representation of the board to the console."
  [board]
  (doseq [row (range 0 (inc 2))]
    (println (render-row board row))))

;; borrowed from peg-thing
;; https://github.com/flyingmachine/pegthing/blob/master/src/pegthing/core.clj
(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         (clojure.string/lower-case input)))))
;;

(defn get-number
  "Given a string of an single-digit integer, return the integer."
  [input]
  (if (= input "")
      nil
      (- (int (first input)) 48)))

;; modified from peg-thing
(defn prompt-move
  "The game engine. Repeatedly prompt for the moves of alternating players until game ends in a victory or tie."
  [board player]
  (println "\nHere's your board:")
  (print-board board)
  (println (str "Player " player " moves where:"))
  (if-let [input (get-number (get-input))]
    (let [new-board (make-move board input player)]
      (if (or (= nil input) (= nil new-board))
        (do
          (println "\n!!! That was an invalid move :(\n")
          (prompt-move board player))
        (if (winning-move new-board input)
          (println (str "Player " player " wins!"))
	  (if (game-tied new-board)
	    (println "Game ends in a tie!")
	    (prompt-move new-board (other-player player))))))
    (do
      (println "\n!!! That was an invalid move :(\n")
      (prompt-move board player))))
      
;;

(defn -main
  "Play a game of Tic-Tac-Toe."
  [& args]
  (prompt-move (build-board) (other-player)))
