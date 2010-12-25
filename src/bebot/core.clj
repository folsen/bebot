(ns bebot.core
  (:import [java.awt Robot Rectangle]
	   [java.awt.image BufferedImage RenderedImage]
	   [java.io File]
	   [javax.imageio ImageIO]
	   [java.awt.event InputEvent]))

(def r (new Robot))
(def cali (ref {:x 192 :y 268}))
(def space 40)
(def board (make-array java.lang.Object 8 8))

(def colors {-5000269 :white -65495 :red -16744198 :blue
	      -203 :yellow -65294 :purple -16729559 :green
	      -47594 :orange -16743941 :blue -16764150 :green
	      -47622 :purple -4934475 :white -5606111 :yellow
	      -16711792 :green -65494 :red -13224394 :white
	      -202 :yellow -16729302 :green -3947581 :white
	      -12255222 :red -47080 :orange -65287 :purple
	      -12910538 :purple})

(defn list-contains? [value list]
  (true? (some #(= value %) list)))

(defn calibrate []
  "Calibrate the window"
  (let [w (new Rectangle 0 0 950 950)
	img (. r createScreenCapture w)
	found (atom false)]
    (doseq [y (range 0 600) x (range 0 600) :while (false? @found)]
      ; Find all the 4 corners
      (if (every?
	   #(contains? colors %)
	   [(. img getRGB x y)
	    (. img getRGB (+ x (* space 7)) y)
	    (. img getRGB x (+ y (* space 7)))
	    (. img getRGB (+ x (* space 7)) (+ y (* space 7)))])
	(do
	  (reset! found true)
	  (dosync (ref-set cali {:x x :y y})))))
    (if found
      (println "Calibrated successfully.")
      (println "Couldn't calibrate. Perhaps you need to change the color values in the code.")))
  @cali)

(defn printboard []
  "Prints the color numbers for the current board.
   Optional argument cnames is true or false.
   If true it prints the names of the colors
   instead of the numbers. (Good for debugging)."
  (doseq [y (range 0 8) x (range 0 8)]
    (print (str (aget board y x) " "))
    (if (= x 7) (print "\n"))))

; TODO Provide example picture and fix this thing up
(defn output-colors [cx cy]
  "Put in the approximate center of the first piece for your screen as x and y
   and you will get a board of the color numbers printed that you can later use."
  (let [w (new Rectangle 0 0 950 950)
	img (. r createScreenCapture w)]
    (doseq [y (range 0 8) x (range 0 8)]
      (aset board y x
	    (. img getRGB (+ cx (* space x))
	       (+ cy (* space y))))))
  (printboard))

(defn scan []
  "Put in the approximate center of the first piece for your screen as x and y
   and you will get a board of the color numbers printed that you can later use."
  (let [w (new Rectangle 0 0 950 950)
	img (. r createScreenCapture w)]
    (doseq [y (range 0 8) x (range 0 8)]
      (let [color (. img getRGB (+ (:x @cali) (* space x))
		     (+ (:y @cali) (* space y)))]
	(aset board y x (if (nil? (colors color))
			  (do
			    ;(println (str "Unknown: " color))
			    ;(Thread/sleep 1500)
			    color)
			  (colors color)))))))

(defn getp [arr y x]
  "This probably exists in some native clojure way but I don't want to try to find it."
  (try
    (aget arr y x)
    (catch java.lang.ArrayIndexOutOfBoundsException e
      nil)))

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
     ; +1 for the moved stone itself
     1)))

(defn som [color unknown]
  "Checks if the color of the unknown is the same or multi.
   som stands for same or multi."
  (or (= color unknown) (= :multi unknown) (= :multi color)))

(defn look [dir color sy sx]
  "Looks in a given direction, calculating the total number of same pieces in that direction."
  (case dir
	:up (if (som color (getp board (- sy 1) sx))
	      (+ 1 (look :up (getp board (- sy 1) sx) (- sy 1) sx))
	      0)
	:right (if (som color (getp board sy (+ sx 1)))
		 (+ 1 (look :right (getp board sy (+ sx 1)) sy (+ sx 1)))
		 0)
	:left (if (som color (getp board sy (- sx 1)))
		(+ 1 (look :left (getp board sy (- sx 1)) sy (- sx 1)))
		0)
	:down (if (som color (getp board (+ sy 1) sx))
		(+ 1 (look :down (getp board (+ sy 1) sx) (+ sy 1) sx))
		0)))

(defn calc-move [dir fy fx ty tx scoringdirs]
  "Calculates the validity and the score of a move for a piece
   with start coordinates fx fy and end-coordinates tx ty.
   Scoring directions are to supplied according to scheme for (score)."
  (let [to (getp board ty tx)
	scores (if (not (nil? to))
		 (map #(look % (getp board fy fx) ty tx) scoringdirs)
		 '(0 0 0))]
    (if (> (score scores) 2)
      {:dir dir :from {:x fx :y fy} :to {:x tx :y ty} :score (score scores)})))

(defn move [dir y x]
  "Returns a 'move hash' if it's a valid move or nil if it's invalid."
  (case dir
   :up    (calc-move :up y x (- y 1) x '(:up :left :right))
   :right (calc-move :right y x y (+ x 1) '(:right :up :down))
   :left  (calc-move :left y x y (- x 1) '(:left :down :up))
   :down  (calc-move :down y x (+ y 1) x '(:down :right :left))))

(defn valid-moves [y x]
  "Returns all valid moves for a position x y"
  (filter #(not (nil? %)) (map #(move % y x) '(:up :right :down :left))))

(defn all-moves []
  "Returns all valid moves for the board"
  (let [moves (ref ())]
    (doseq [y (range 0 8) x (range 0 8)]
      (dosync (alter moves conj (valid-moves y x))))
    (flatten @moves)))

(defn make-move [move]
  "Makes a move given a 'move map'."
  (if (not (nil? move))
    (do
      (. r mouseMove (+ (:x @cali) (* (-> move :from :x) space))
	             (+ (:y @cali) (* (-> move :from :y) space)))
      (. r mousePress InputEvent/BUTTON1_MASK)
      (. r mouseRelease InputEvent/BUTTON1_MASK)
      (. r mouseMove (+ (:x @cali) (* (-> move :to :x) space) 3)
                     (+ (:y @cali) (* (-> move :to :y) space) 3))
      (. r mousePress InputEvent/BUTTON1_MASK)
      (. r mouseRelease InputEvent/BUTTON1_MASK))))

(defn run []
  (dotimes [n 5]
    (println (str (- 5 n) ".."))
    (Thread/sleep 1000))
  (let [start (System/currentTimeMillis)]
    (loop []
      (if (> (- (System/currentTimeMillis) start) 60000)
	"Done"
	(do
	  (scan)
	  (make-move (last (sort-by #(:score %) (all-moves))))
	  (Thread/sleep 100)
	  (recur))))))

(defn -main [&args]
  "Main run loop."
  (calibrate)
  (Thread/sleep 5000)
  (run))