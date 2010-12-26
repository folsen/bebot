(ns bebot.simulate
  (:use bebot.utils
	bebot.moving))

(defn remove-pieces [board dir color sy sx]
  "Removes all pieces of color 'color' in direction dir.
   This will not handle Multi pieces properly... :/
   But I don't really have the energy to fix that since
   Multi pieces are pretty much impossible to recognize."
  (case dir
	:up (if (som color (getp board (- sy 1) sx))
	      (do (aset board (- sy 1) sx :deleted)
		  (remove-pieces board :up color (- sy 1) sx)))
	:right (if (som color (getp board sy (+ sx 1)))
		 (do (aset board sy (+ sx 1) :deleted)
		     (remove-pieces board :right color sy (+ sx 1))))
	:left (if (som color (getp board sy (- sx 1)))
		(do (aset board sy (- sx 1) :deleted)
		    (remove-pieces board :left color sy (- sx 1))))
	:down (if (som color (getp board (+ sy 1) sx))
		(do (aset board (+ sy 1) sx :deleted)
		    (remove-pieces board :down color (+ sy 1) sx)))))

(defn clear-space [board p1 p2 p3 color sy sx]
  "Clears the proper space around the moved piece."
  (if (> (look board p1 color sy sx) 1)
    (remove-pieces board p1 color sy sx))
  (if (> (+ (look board p2 color sy sx) (look board p3 color sy sx)) 1)
    (do (remove-pieces board p2 color sy sx)
	(remove-pieces board p3 color sy sx))))

(defn simulate-board-move [board move]
  "Simulates a move on the board. Adding in 'unknown' pieces on top.
  Not that pretty... I was getting tired when I wrote this."
  (let [newboard (deepcopy board)
	color (aget newboard (-> move :from :y) (-> move :from :x))]
    (aset newboard (-> move :from :y) (-> move :from :x)
	  (aget newboard (-> move :to :y) (-> move :to :x)))
    (aset newboard (-> move :to :y) (-> move :to :x) :deleted)

    ;clear other pieces
    (case (move :dir)
	  (let [clr (fn [p1 p2 p3]
		      (clear-space newboard p1 p2 p3 color (-> move :to :y) (-> move :to :x)))]
	    :up (clr :up :left :right)
	    :right (clr :right :up :down)
	    :left (clr :left :down :up)
	    :down (clr :down :right :left)))

    (doseq [y (range 0 8) x (range 0 8)]
      ;Go through all pieces from top to bottom and enact gravity
      (if (= (getp newboard y x) :deleted)
	(loop [ny y]
	  (let [upval (getp newboard (- ny 1) x)]
	    (if (nil? upval)
	      (aset newboard ny x (rand))
	      (do
		(aset newboard ny x (getp newboard (- ny 1) x))
		(recur (- ny 1))))))))
    newboard))