(ns slack-go.go (:require [clojure.zip :refer [zipper down]]
                          [clojure.set :refer [union difference]]
                          [clojure.math.numeric-tower :refer [abs]]))

; playing go

(defn sd
  "symmetric difference"
  [x y]
  (abs (- x y)))

(defn neighbor?
  [[x1 y1] [x2 y2]]
  (= 1
     (+ (sd x1 x2) (sd y1 y2))))

(defn neighbors [max [x y]]
  (filter
   (fn [[nx ny]]
     (and (>= nx 0)
          (>= ny 0)
          (< nx max)
          (< ny max)))
   [[(+ x 1) y]
    [(- x 1) y]
    [x (+ y 1)]
    [x (- y 1)]]))

(defn build-group [max stones]
  {:stones stones
   :neighbors (into #{} (mapcat (partial neighbors max) stones))})

(defn merge-groups
  "merge groups that should exclusively overlap on unifying-stone"
  [max unifying-stone groups]
  (reduce
   (fn [cur next]
     {:stones (mapcat :stones [cur next])
      :neighbors (apply union (map :neighbors [cur next]))})
   (build-group max [unifying-stone])
   groups))

(defn add-stone
  "add a stone to existing group if it is connected, otherwise add a new group"
  [max groups stone]
  (let [member? #(boolean (some (partial neighbor? stone) (:stones %)))
        membership (group-by member? groups)
        member-of (or (membership true) [])
        not-member-of (or (membership false) [])]
    (cons (merge-groups max stone member-of) not-member-of)))

(defn to-groups
  "turn list of stones into a list of connected groups"
  [max stones]
  (reduce (partial add-stone max) [] stones))

(defn opponent [color]
  (if (= color :black) :white :black))

(defn occupied [{black :black white :white}]
  (into #{} (concat black white)))

(defn remove-occupied-liberties
  [occupied-set {:keys [stones neighbors]}]
  {:liberties (filter (comp not occupied-set) neighbors)
   :stones stones})

(defn living
  "return only stones that are part of groups that have liberties"
  [groups occupied]
  (->> groups
       (map #(remove-occupied-liberties occupied %))
       (filter (comp seq :liberties)) ; remove groups with no liberties
       (mapcat :stones))) ; convert groups to stones

(defn update-stones
  "return stones for color after killing off any dead groups"
  [{stones color
    dim :dim
    :as board-state}
   color]
  (living (to-groups dim stones) (occupied board-state)))

(defn update-board [color board-state]
  "kill any dead groups on the board"
  (as-> board-state b
        ;; update opp stones first for the suicide rule
        (assoc b (opponent color) (update-stones b (opponent color)))
        (assoc b color (update-stones b color))))

(defn update-turn [color board-state]
  (assoc board-state :turn (opponent color)))

(defn update-prev
  "update pointer to previous state for ko rule enforcment"
  [old-board new-board]
  (assoc new-board :prev (dissoc old-board :prev)))

(defn update-last-move [move board-state]
  (assoc board-state :last-move move))

(defn repeated-state?
  "return true if the old board is the same as the new board"
  [{old-black :black old-white :white}
   {black :black white :white}]
  (and
   (= (set old-black) (set black))
   (= (set old-white) (set white))))

(defn add-move [[color move] {stones color :as board-state}]
  "add the move for color to the board state (doesn't kill off dead groups)"
  (assoc board-state color (cons move stones)))

(defn play-move [board-state move]
  (->> board-state
       (add-move move)
       (update-board (first move))
       (update-turn (first move))
       (update-last-move move)
       (update-prev board-state)))

; move validation

(defn my-turn? [board-state color]
  (= (:turn board-state) color))

(defn occupied? [board-state [color coord]]
  ((occupied board-state) coord))

(defn suicide?
  "Move should not cause any of own color to die"
  [board-state [color :as move]]
  (let [{result color} (play-move board-state move)]
    (<= (count result)
        (count (color board-state)))))

(defn ko-rule-violated?
  "Ensure next move doesn't return to a previous state, currently
  only prevents cycles of length 2"
  [board-state move]
  (if-let [old (:prev board-state)]
    (repeated-state? old (play-move board-state move))
    false))

(defn illegal-move?
  "Return error string if move is illegal"
  [board-state [color :as move]]
  (cond
    ((comp not my-turn?) board-state color) "Not your turn"
    (occupied? board-state move) "Coordinate is already occupied"
    (suicide? board-state move) "Suicide not allowed"
    (ko-rule-violated? board-state move) "Move would return board to previous state"
    :else false))




;; scoring

(defn inverse-coords [dim positions]
  "all positions not in positions"
  (->> (mapcat #(for [y (range dim)] [% y]) (range dim))
       (filter (comp not positions))
       (into #{})))

;; sigh, straight up recursion was much prettier, but caan StackOverflow on bigger boards
#_(defn find-connected-component
    "Return map with all points in the component and whether it touches black/white stones"

    ([dim black? white? pt]
     (find-connected-component dim black? white? {:visited #{}} pt))

    ([dim black? white? {visited :visited :as acc} pt]
     (cond
       (black? pt) (assoc acc :black true)
       (white? pt) (assoc acc :white true)
       (visited pt) acc
       :else (reduce
              (partial find-connected-component dim black? white?)
              (assoc acc :visited (conj visited pt))
              (neighbors dim pt)))))

;; iterative DFS
(defn find-connected-component
  "Return map with all points in the component and whether it touches black/white stones"
  [dim black? white? pt]
  (loop [stack (list pt) acc {:visited #{}}]
    (if-let [next (peek stack)]
      (cond
        (black? next) (recur (pop stack) (assoc acc :black true))
        (white? next) (recur (pop stack) (assoc acc :white true))
        ((:visited acc) next) (recur (pop stack) acc)
        :else (recur
               (apply conj (pop stack) (neighbors dim next))
               (update-in acc [:visited] #(conj % next))))
      acc)))

(defn incorporate
  "Declare the connected component of pt as black, white, or neutral"
  [dim black? white? {b :black w :white n :neutral u :unclaimed :as part-map} pt]
  (let [{:keys [black white visited]}
        (find-connected-component dim black? white? pt)
        claim
        (assoc part-map :unclaimed (difference u visited))]
    (cond
      (and black white) (assoc claim :neutral (union n visited))
      black (assoc claim :black (union b visited))
      white (assoc claim :white (union w visited)))))

(defn part
  "Partition all empty points on board as black, white, or neutral"
  [dim black? white? unclaimed]
  (loop [todo {:black #{} :white #{} :neutral #{} :unclaimed unclaimed}]
    (if (empty? (:unclaimed todo))
      (dissoc todo :unclaimed)
      (recur (incorporate dim black? white? todo (first (:unclaimed todo)))))))

(defn two-liberties
  "return only stones that have at least two liberties"
  [groups occupied]
  (->> groups
       (map #(remove-occupied-liberties occupied %))
       (filter #(> (count (:liberties %)) 0)) ; remove groups with 1 liberty
       (mapcat :stones))) ; convert groups to stones

(defn probably-alive
  [{black :black white :white dim :dim :as board} occupied]
  (assoc board
         :black (two-liberties (to-groups dim black) occupied)
         :white (two-liberties (to-groups dim white) occupied)))

(defn score-board
  "Return board partitioned into black/white/neutral"
  [{dim :dim :as board}]
  (let [occ (occupied board)
        {black :black white :white} (probably-alive board occ)
        partitioned (part dim (set black) (set white) (inverse-coords dim occ))]
    {:black (into (:black partitioned) black)
     :white (into (:white partitioned) white)
     :neutral (:neutral partitioned)}))

(defn score-counts [board]
  (let [{:keys [black white neutral]} (score-board board)]
    {:black (count black) :white (count white) :neutral (count neutral)}))




;; "AI"
(defn all-moves [dim color]
  (map
   (fn [coord] [color coord])
   (inverse-coords dim #{})))

(defn random-move
  "Return a randomly determined legal move. If no move exists, return nil"
  [{dim :dim color :turn :as board}]
  (let [moves (shuffle (all-moves dim color))]
    (->> (all-moves dim color)
         shuffle
         (filter (complement (partial illegal-move? board)))
         first)))
