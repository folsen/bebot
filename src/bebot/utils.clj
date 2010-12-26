(ns bebot.utils
  (:import [java.awt Robot Rectangle]
	   [java.awt.image BufferedImage RenderedImage]))

; I stashed this parameter in here because it's used like everywhere.
(def space 40)

(defn getp [arr y x]
  "This probably exists in some native clojure way but I don't want to try to find it."
  (try
    (aget arr y x)
    (catch java.lang.ArrayIndexOutOfBoundsException e
      nil)))

(defn deepcopy [board]
  "Making a deep copy of a matrix."
  (let [newboard (make-array java.lang.Object 8)]
    (doseq [y (range 0 8)]
      (aset newboard y (aclone (aget board y))))
    newboard))

(defn som [color unknown]
  "Checks if the color of the unknown is the same or multi.
   'som' stands for same or multi."
  (or (= color unknown) (= :multi unknown) (= :multi color)))

(defn printboard [board & lookup]
  "Prints the color numbers for the current board.
   Optional argument lookup is true or false.
   If true it prints the names of the colors
   instead of the numbers. (Good for debugging)."
  (doseq [y (range 0 8) x (range 0 8)]
    (if lookup
      (print (str (try (val (find bebot.core/colors (aget board y x)))
		       (catch Exception e (aget board y x))) " "))
      (print (str (aget board y x) " ")))
      
    (if (= x 7) (print "\n"))))

(defn output-colors [cx cy]
  "Put in the approximate center of the first piece for your screen as x and y
   and you will get a board of the color numbers printed for the unrecoginized colors
   that you can later use. Sample picture is in the source folder."
  (let [w (new Rectangle 0 0 950 950)
	img (. (new Robot) createScreenCapture w)
	board (make-array java.lang.Object 8 8)]
    (doseq [y (range 0 8) x (range 0 8)]
      (aset board y x
	    (. img getRGB (+ cx (* space x))
	       (+ cy (* space y)))))
    (printboard board true)))