(ns tetris.core
  (:import (java.awt Color Dimension BorderLayout)
    (javax.swing JPanel JFrame JOptionPane JButton JLabel)
    (java.awt.event KeyListener))
  (:use clojure.contrib.import-static deflayout.core
        clojure.contrib.swing-utils)
  (:gen-class))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_DOWN VK_UP VK_SPACE)

(def empty-cell 0)
(def filled-cell 2)
(def moving-cell 1)
(def glass-width 10)
(def glass-height 20)
(def zero-coords [3 0])

(def stick [[0 0 0 0] 
            [1 1 1 1] 
            [0 0 0 0] 
            [0 0 0 0]])

(def square [[1 1]
             [1 1]])

(def tblock [[0 0 0]
             [1 1 1] 
             [0 1 0]])

(def sblock [[0 1 0] 
             [0 1 1] 
             [0 0 1]])

(def zblock [[0 0 1] 
             [0 1 1] 
             [0 1 0]])

(def lblock [[1 1 0] 
             [0 1 0] 
             [0 1 0]])

(def jblock [[0 1 1] 
             [0 1 0] 
             [0 1 0]])

(def figures [stick square tblock sblock zblock lblock jblock])

(def create-vector (comp vec repeat))
  
(defn create-glass[]
  (create-vector glass-height
                 (create-vector glass-width empty-cell)))

(defn pick-cell [figure x y]
  (get-in figure [y x]))

(defn mapmatrix [func matrix]
  (into [] (map-indexed (fn[y vect]
                          (into [] (map-indexed (fn[x el]
                                                  (func el x y))
                                                vect)))
                        matrix)))
           
(defn rotate-figure [fig]
  (let [fsize (count fig)]
    (mapmatrix #(pick-cell fig (- fsize %3 1) %2) fig)))

(defn apply-fig [glass fig [figx figy]]
  (let [fsize (count fig)]
    (mapmatrix (fn[el gx gy]
                 (if (and
                       (<= figx gx (+ figx fsize -1))
                       (<= figy gy (+ figy fsize -1)))
                   (+ el (pick-cell fig (- gx figx) (- gy figy)))
                   el))
      glass)))

(defn destroy-filled [glass]
  (let [clear-glass
        (remove (fn[vect] 
                  (not-any? #(= % empty-cell) vect)) glass)
        destroyed (- glass-height (count clear-glass))]
    [(into (vec (repeat 
                 destroyed
                 (create-vector glass-width empty-cell)))
           (vec clear-glass)) destroyed]))

(defn fix-figure [glass-with-fig]
  (mapmatrix (fn[el & _]
               (if (= el moving-cell)
                  filled-cell
                  el))
    glass-with-fig))

(defn count-cells [glass value]
  (reduce + (map (fn[vect]
                   (count (filter #{value} vect)))
                 glass)))

(defn legal? [glass]
  (= (count-cells glass moving-cell) 4))

(defn move 
  ([glass fig [figx figy] shiftx shifty]
    (let [newx (+ figx shiftx)
          newy (+ figy shifty)
          newglass (apply-fig glass fig [newx newy])]
      (when (legal? newglass) [newx newy])))
  ([glass fig coords direction]
    (cond
      (= direction :down) (move glass fig coords 0 1)
      (= direction :left) (move glass fig coords -1 0)
      (= direction :right) (move glass fig coords 1 0))))

(def score-per-line 10)
 
(defmacro defatoms [& atoms]
  `(do
     ~@(map (fn[a#] `(def ~a# (atom nil))) atoms)))

(defatoms *glass* *fig-coords* *current-fig* *next-fig* *score*)

(defn complete-glass[]
  (apply-fig @*glass* @*current-fig* @*fig-coords*))

(defn done-callback [n]
  (swap! *score* #(+ % (* n score-per-line))))
 
(defn move-to-side [side]
  (let [newcoords
        (move @*glass* @*current-fig* @*fig-coords* side)]
    (if newcoords
      (reset! *fig-coords* newcoords))))

(defn move-down[]
  (let [newcoords
        (move @*glass* @*current-fig* @*fig-coords* :down)]
    (if newcoords
      (reset! *fig-coords* newcoords)
      (let [[newglass d-count] (-> (complete-glass) 
                                   fix-figure
                                   destroy-filled)]
        (reset! *glass* newglass)
        (reset! *fig-coords* zero-coords)
        (reset! *current-fig* @*next-fig*)
        (reset! *next-fig* (rand-nth figures))
        (done-callback d-count)
        (when-not (legal? (complete-glass)) :lose)))))

(defn move-all-down[]
  (move-down)
  (let [newcoords
        (move @*glass* @*current-fig* @*fig-coords* :down)]
    (when newcoords (recur))))
  
(defn rotate-current[]
  (let [rotated (rotate-figure @*current-fig*)]
    (if (legal? (apply-fig @*glass* rotated @*fig-coords*))
      (swap! *current-fig* rotate-figure))))

(defn new-game[]
  (reset! *glass* (create-glass))
  (reset! *fig-coords* zero-coords)
  (reset! *current-fig* (rand-nth figures))
  (reset! *next-fig* (rand-nth figures))
  (reset! *score* 0))

(def cell-size 20)
(def border-size 3)
(def timer-interval 300)
(def game-running (atom false))

(defn fill-point [g [x y] color]
  (.setColor g color)
  (.fillRect g 
    (* x cell-size) (* y cell-size)
    cell-size cell-size)
  (when-not (= color (Color/gray))
    (.setColor g (.brighter color))
    (.fillRect g
      (* x cell-size) (* y cell-size)
      border-size cell-size)
    (.fillRect g
      (* x cell-size) (* y cell-size)
      cell-size border-size)
    (.setColor g (.darker color))
    (.fillRect g
      (- (* (inc x) cell-size) border-size) (* y cell-size)
      border-size cell-size)
    (.fillRect g
      (* x cell-size) (- (* (inc y) cell-size) border-size)
      cell-size border-size)))
  
(defn get-color [cell]
  (cond
    (= cell empty-cell) (Color/gray)
    (= cell filled-cell) (new Color 128 0 0)
    (= cell moving-cell) (new Color 0 128 0)
    :else (Color/black)))

(defn paint-glass [g glass]
  (mapmatrix (fn[cell x y]
               (fill-point g [x y] (get-color cell)))
    glass))
  
(defn game-panel []
  (proxy [JPanel KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doall (paint-glass g (complete-glass))))
    (keyPressed [e]
      (let [keycode (.getKeyCode e)]
        (do (cond
              (= keycode VK_LEFT) (move-to-side :left)
              (= keycode VK_RIGHT) (move-to-side :right)
              (= keycode VK_DOWN) (move-down)
              (= keycode VK_UP) (rotate-current)
              (= keycode VK_SPACE) (move-all-down))
          (.repaint this))))
    (getPreferredSize []
      (Dimension. (* glass-width cell-size)
        (* glass-height cell-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn next-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doall (paint-glass g @*next-fig*)))
    (getPreferredSize []
      (Dimension. (* 4 cell-size)
        (* 4 cell-size)))))

(defn game[]
  (new-game)
  (reset! game-running true)
  (let [gamepanel (game-panel)
        sidepanel (new JPanel)
        nextpanel (next-panel)
        scorelabel (JLabel. "Score: 0")
        exitbutton (JButton. "Exit")
        frame (JFrame. "Tetris")]
    (deflayout
      frame (:border)
      {:WEST gamepanel
       :EAST (deflayout (JPanel.) (:border)
               {:NORTH (deflayout sidepanel (:flow :TRAILING)
                         [nextpanel scorelabel])
                :SOUTH exitbutton})})
    (doto gamepanel
      (.setFocusable true)
      (.addKeyListener gamepanel)
      (.repaint))
    (doto frame
      (.pack)
      (.setVisible true))
    (doto exitbutton
      (add-action-listener (fn[_]
                             (do
                               (.setVisible frame false)
                               (reset! game-running false)))))
    (loop []
      (when @game-running
        (let [res (move-down)]
          (if (= res :lose)
            (JOptionPane/showMessageDialog frame "You lose!" )
            (do               
              (.repaint gamepanel)
              (.repaint nextpanel)
              (.setText scorelabel (str "Score: " @*score*))
              (. Thread sleep timer-interval)
              (recur))))))))

(defn -main [& args]
  (game))
