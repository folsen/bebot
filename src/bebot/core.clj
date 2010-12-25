(ns bebot.core
  (:import [java.awt Robot Rectangle]
	   [java.awt.image BufferedImage RenderedImage]
	   [java.io File]
	   [javax.imageio ImageIO]
	   [java.awt.event InputEvent]))

(def r (new Robot))
;(Thread/sleep 5000)
(def cali (ref {:x 0 :y 0}))
(def space 40)
(def start (System/currentTimeMillis))
(def board (make-array Integer/TYPE 8 8))

;(def colors {:white -65794 :red -122558 :blue -15238154 :yellow -66007
;	     :purple -123906 :green -10420589 :orange -65915})

(def colors {:white -65794 :red -65490 :blue -16749580 :yellow -11220
	     :purple -7733121 :green -16711827 :orange -3479})

(defn reverse-map
  "Reverse the keys/values of a map"
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))

(def rcolors (reverse-map colors))

(defn calibrate []
  "Calibrate the window"
  (let [w (new Rectangle 0 0 950 950)
	img (. r createScreenCapture w)
	found (atom false)]
    (doseq [y (range 0 600) x (range 0 600) :while (false? @found)]
      ; Find all the 4 corners
      (if (every?
	   #(contains? rcolors %)
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

(defn printboard [& cnames]
  "Prints the color numbers for the current board.
   Optional argument cnames is true or false.
   If true it prints the names of the colors
   instead of the numbers. (Good for debugging)."
  (doseq [y (range 0 8) x (range 0 8)]
    (if cnames
      (print (str (val (find rcolors (aget board y x))) " "))
      (print (str (aget board y x) " ")))
    (if (= x 7) (print "\n"))))

;(. r mouseMove 0 0 )
;(. timage getRGB 20 20)

(defn testgrid [cx cy]
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
      (aset board y x
	    (. img getRGB (+ (:x @cali) (* space x))
	                  (+ (:y @cali) (* space y)))))))

;(defn valid-moves []
;  "Returns a seq of all valid moves.")

(defn getp [arr x y]
  "This probably exists in some native clojure way but I don't want to try to find it."
  (try
    (aget arr x y)
    (catch java.lang.ArrayIndexOutOfBoundsException e
      nil)))

(defn look [dir color sx sy]
  (case dir
	:up (if (= color (getp board sx (- sy 1)))
	      (+ 1 (look :up color sx (- sy 1)))
	      0)
	:right (if (= color (getp board (+ sx 1) sy))
		 (+ 1 (look :right color (+ sx 1) sy))
		 0)
	:left (if (= color (getp board (- sx 1) sy))
		(+ 1 (look :left color (- sx 1) sy))
		0)
	:down (if (= color (getp board sx (+ sy 1)))
		(+ 1 (look :down color sx (+ sy 1)))
		0)))

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

(defn calc-move [fx fy tx ty scoringdirs]
  "Calculates the validity and the score of a move up for a piece x y"
  (let [to (getp board tx ty)
	scores (if (not (nil? to))
		 (map #(look % (getp board fx fy) tx ty) scoringdirs)
		 '(0 0 0))]
    (if (> (score scores) 2)
      {:from {:x fx :y fy} :to {:x tx :y ty} :score (score scores)})))

(defn move [dir x y]
  "Returns a 'move hash' if it's a valid move or nil if it's invalid."
  (case dir
   :up (calc-move x y x (- y 1) '(:up :left :right))
   :right (calc-move x y (+ x 1) y '(:right :up :down))
   :left (calc-move x y (- x 1) y '(:left :down :up))
   :down (calc-move x y x (+ y 1) '(:down :right :left))))

(defn valid-moves [x y]
  "Returns all valid moves for a position x y"
  (filter #(not (nil? %)) (map #(move % x y) '(:up :right :down :left))))
  

(defn -main [&args]
  "Main run loop."
  (loop []
    (if (> (- (System/currentTimeMillis) start) 85000)
    "Done"
    (do
      ;Something
      ;Scan
      ;Build tree
      ;Sleep
      (recur)))))