(ns bebot.moving
  (:use bebot.utils))


(defn score [scores]
  "scores should be in format (p1 p2 p3)
            p1
             ^
             |     <-- Example: A move up
             _
     p2 <-- |_| --> p3"
  (let [p1 (nth scores 0)
	p2 (nth scores 1)
	p3 (nth scores 2)]
    (+
     (if (> p1 1) p1 0)
     (if (> (+ p2 p3) 1) (+ p2 p3) 0)
     ; +1 for the moved piece itself
     1)))

(defn look [board dir color sy sx]
  "Looks in a given direction, calculating the total number of same pieces in that direction."
  (case dir
	:up (if (som color (getp board (- sy 1) sx))
	      (+ 1 (look board :up (getp board (- sy 1) sx) (- sy 1) sx))
	      0)
	:right (if (som color (getp board sy (+ sx 1)))
		 (+ 1 (look board :right (getp board sy (+ sx 1)) sy (+ sx 1)))
		 0)
	:left (if (som color (getp board sy (- sx 1)))
		(+ 1 (look board :left (getp board sy (- sx 1)) sy (- sx 1)))
		0)
	:down (if (som color (getp board (+ sy 1) sx))
		(+ 1 (look board :down (getp board (+ sy 1) sx) (+ sy 1) sx))
		0)))

(defn calc-move [board dir fy fx ty tx scoringdirs]
  "Calculates the validity and the score of a move for a piece
   with start coordinates fx fy and end-coordinates tx ty.
   Scoring directions are to supplied according to scheme for (score)."
  (let [to (getp board ty tx)
	scores (if (not (nil? to))
		 (map #(look board % (getp board fy fx) ty tx) scoringdirs)
		 '(0 0 0))]
    (if (> (score scores) 2)
      {:dir dir :from {:x fx :y fy} :to {:x tx :y ty} :score (score scores)})))

(defn move [board dir y x]
  "Returns a 'move hash' if it's a valid move or nil if it's invalid."
  (case dir
   :up    (calc-move board :up y x (- y 1) x '(:up :left :right))
   :right (calc-move board :right y x y (+ x 1) '(:right :up :down))
   :left  (calc-move board :left y x y (- x 1) '(:left :down :up))
   :down  (calc-move board :down y x (+ y 1) x '(:down :right :left))))

(defn valid-moves [board y x]
  "Returns all valid moves for a position x y"
  (filter #(not (nil? %)) (map #(move board % y x) '(:up :right :down :left))))

(defn all-moves [board]
  "Returns all valid moves for the board"
  (let [range (for [i (range 8) j (range 8)] (list i j))]
    (flatten (filter #(not (empty? %)) (map #(valid-moves board (second %) (first %)) range)))))