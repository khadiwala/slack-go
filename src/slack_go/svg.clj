(ns slack-go.svg
  (:require [hiccup.core :refer :all]
            [clojure.string :as string]))

; constants
(def wood "#DCB35C")
(def black "#000000")
(def white "#FFFFFF")

; drawing
(defn line [dir movement length pos]
  (str "m" (string/join "," movement) dir (if pos "" "-") length))

(defn n-lines
  [space dir length n]
  (for [x (range n)]
    (line
     dir
     (if (= dir "h") [0 space] [space 0])
     length
     (= (mod x 2) 0))))

(defn relative-position [[x y] [width height] dim]
  [(* x (quot width (- dim 1)))
   (* y (quot height (- dim 1)))])

(defn translate [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn horizontal-lines
  [[x y] [width height] dim]
  (str "m" x "," y
       (string/join "" (n-lines (quot height (- dim 1)) "h" width (- dim 2)))))

(defn vertical-lines
  [[x y] [width height] dim]
  (str "m" x "," y
       (string/join "" (n-lines (quot width (- dim 1)) "v" height (- dim 2)))))

(defn get-color [color]
  (if (= color :black) black white))

(defn get-opponent-color [color]
  (if (= color :black) white black))

(defn even-multiple [dim length]
  (* (quot length (- dim 1)) (- dim 1)))

(defn view-box
  "The largest box that fits in width/height that is evenly divisible by dim"
  [dim [width height]]
  (map (partial even-multiple dim) [width height]))

(defn draw-board
  "board line elements"
  [[x y] [width height] dim]
  [[:rect
    {:width width
     :height height
     :x x :y y
     :stroke black
     :stroke-width 0.2
     :fill "none"}]
   [:path
    {:stroke black
     :stroke-width 0.2
     :fill "none"
     :d (horizontal-lines [x y] [width height] dim)}]
   [:path
    {:stroke black
     :stroke-width 0.2
     :fill "none"
     :d (vertical-lines [x y] [width height] dim)}]])

(defn stone->pos [origin geom dim stone]
  (-> stone
      (relative-position geom dim)
      (translate origin)))

(defn circle [r color [x y]]
  [:circle {:cx x
            :cy y
            :fill (get-color color)
            :r r}])

(defn circle-marker [r color [x y]]
  [:circle {:cx x
            :cy y
            :stroke (get-opponent-color color)
            :fill "none"
            :r (quot r 2)}])

(defn square-marker [l color [x y]]
  (let [translation (quot l 2)]
  [:rect {:x (- x translation)
          :y (- y translation)
          :width l
          :height l
          :stroke (get-color color)
          :fill (get-color color)}]))

(defn spacing [[width height] dim]
  (min (quot height (- dim 1)) (quot width (- dim 1))))

(defn radius [geom dim]
  (quot (spacing geom dim) 3))

(defn length [geom dim]
  (quot (spacing geom dim) 3))

(defn draw-stones
  "draw stones"
  [origin geom dim {black :black white :white}]
  (concat
   (->> black
        (map #(stone->pos origin geom dim %))
        (map #(circle (radius geom dim) :black %)))
   (->> white
        (map #(stone->pos origin geom dim %))
        (map #(circle (radius geom dim) :white %)))))

(defn draw-special
  [origin geom dim {last-move :last-move :or {last-move nil}}]
  (if-let [[color coord] last-move]
    [(circle-marker (radius geom dim) color (stone->pos origin geom dim coord))]
    []))

(def letters
  (->> (range (int \A) (+ 1 (int \Z)))
       (map char)
       (map str)))

(defn label [x y label]
  [:text
   {:x x :y y :font-size 20 :fill "#4D4D4D" :font-family "Helvetica"}
   label])

(defn draw-labels [side-margin [w h] dim]
  (let [delta (spacing [w h] dim)
        margin (quot side-margin 2)
        letters (take dim letters)]
    (concat

     ;; vertical labels
     (for [i (range dim)]
       (label 15 (+ side-margin (* delta i)) (str (- dim i))))
     (for [i (range dim)]
       (label (+ (* 2 side-margin) w -45) (+ side-margin (* delta i)) (str (- dim i))))

     ;; horizontal labels
     (for [i (range dim)]
       (label (+ side-margin (* delta i) -6) (+ 5 margin) (nth letters i)))
     (for [i (range dim)]
       (label (+ side-margin (* delta i) -6) (+ h (* 2 side-margin) -15) (nth letters i))))))

(defn draw-score [origin geom dim board-state {black :black white :white}]
  (let [black? (set (:black board-state))
        white? (set (:white board-state))]
    (concat (->> black
                 (filter (complement black?))
                 (map #(square-marker (length geom dim) :black (stone->pos origin geom dim %))))
            (->> white
                 (filter (complement white?))
                 (map #(square-marker (length geom dim) :white (stone->pos origin geom dim %)))))))

(defn board

  ([[width height] margin dim board-state]
   (board [width height] margin dim board-state {:black nil :white nil}))

  ([[width height] margin dim board-state score]
   (let [side-margin (/ margin 2)
         origin      [side-margin side-margin]
         geom        [(- width margin) (- height margin)]]
     (html
      [:svg
       {:xmlns "http://www.w3.org/2000/svg"
        :width width
        :height height
        :viewBox (string/join " " [0 0 width height])}
       (concat [[:rect {:width width :height height :fill wood}]]
               (draw-board origin geom dim)
               (draw-stones origin geom dim board-state)
               (draw-special origin geom dim board-state)
               (draw-labels side-margin geom dim)
               (draw-score origin geom dim board-state score))]))))
