(ns bebot.utils
  (:import [java.awt Robot Rectangle]
	   [java.awt.image BufferedImage RenderedImage]))

; I stashed this parameter in here because it's used like everywhere.
(def space 40)

; This parameter has to be here too or output-colors can't get to it.
(def colors {-5000269 :white -65495 :red -16744198 :blue
	      -203 :yellow -65294 :purple -16729559 :green
	      -47594 :orange -16743941 :blue -16764150 :green
	      -47622 :purple -4934475 :white -5606111 :yellow
	      -16711792 :green -65494 :red -13224394 :white
	      -202 :yellow -16729302 :green -3947581 :white
	      -12255222 :red -47080 :orange -65287 :purple
	     -12910538 :purple -14996940 :blue -2763307 :blue
	     -11185069 :orange})

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
      (print (str (try (val (find colors (aget board y x)))
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