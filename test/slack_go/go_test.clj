(ns slack-go.go-test
  (:require [clojure.test :refer :all]
            [slack-go.go :refer :all]
            [clojure.set :refer [union difference]]))

(def initial
  {:black []
   :white []
   :dim 9
   :turn :black
   :black-player :black
   :white-player :white})

(defn show [obj] (prn obj) obj)

(defn is-not [pred] (is (not pred)))

(defn coords->moves [color coords]
  (map (fn [coord] [color coord]) coords))

(defn play-all [moves]
  (reduce play-move initial moves))

(defn unoccupied? [board move] (not ((occupied board) move)))

(defn all-neighbors? [home nbrs]
  (every? identity (map (partial neighbor? home) nbrs)))

(defn my-nbrs [moves]
  (as-> moves v
        (map (partial neighbors 9) v)
        (map set v)
        (apply union v)
        (difference v (set moves))))

(defn attempt-capture [board to-capture]
  (->> to-capture
       (map (fn [[_ move]] move))
       my-nbrs
       (map (fn [move] [:white move]))
       (reduce play-move board)))

(deftest neighbor-test

  (testing "interior"
    (let [nbrs (neighbors 9 [3 3])]
      (is (= 4 (count nbrs)))
      (is (all-neighbors? [3 3] nbrs))))

  (testing "corner neighbors"
    (let [nbrs (neighbors 9 [0 0])]
      (is (= 2 (count nbrs)))
      (is (all-neighbors? [0 0] nbrs))))

  (testing "edge neighbors"
    (let [nbrs (neighbors 9 [0 1])]
      (is (= 3 (count nbrs)))
      (is (all-neighbors? [0 1] nbrs)))))

(defn capture-test-fun [to-capture]
  (let [black-moves (map (fn [m] [:black m]) to-capture)
        board (play-all black-moves)
        att (attempt-capture board black-moves)]
    (is (->> to-capture
             (filter (partial unoccupied? att))
             seq))))

(deftest capture-test

  (testing "lone interior stone"
    (capture-test-fun [[3 3]]))

  (testing "pair interior stones"
    (capture-test-fun [[3 3] [3 4]]))

  (testing "corner stone"
    (capture-test-fun [[0 0]]))

  (testing "edge stone"
    (capture-test-fun [[0 1]]))

  (testing "edge/corner pair"
    (capture-test-fun [[0 0] [0 1]]))

  (testing "disjoint stones"
    (capture-test-fun [[3 3] [4 4]])))

(defn suicide-setup [coords]
  (->> coords
       my-nbrs
       (coords->moves :white)
       (concat (coords->moves :black (rest coords)))
       play-all (update-turn :white)))

(deftest move-validation-test

  (testing "occupied"
    (let [setup (play-all [[:black [3 3]]])]
      (is (occupied? setup [:white [3 3]]))
      (is (illegal-move? setup [:white [3 3]]))))

  (testing "suicide rule"

    (testing "single surrounded"
      (let [setup (suicide-setup [[3 3]])]
        (is (suicide? setup [:black [3 3]]))
        (is (illegal-move? setup [:black [3 3]]))))

    (testing "group surrounded"
      (let [setup (suicide-setup [[3 3] [3 4]])]
        (is (suicide? setup [:black [3 3]]))
        (is (illegal-move? setup [:black [3 3]]))))

    (testing "non-suicidal capture"
      (let [moves [[:white [0 1]] [:white [1 0]]
                   [:black [0 2]] [:black [2 0]]
                   [:black [1 1]]]
            setup (update-turn :white (play-all moves))]
        (is-not (suicide? setup [:black [0 0]]))
        (is-not (illegal-move? setup [:black [0 0]])))))

  (testing "turns"
    (is (my-turn? (play-all [[:black [3 3]]]) :white))

    (is (my-turn? (play-all [[:black [3 3]] [:white [3 4]]]) :black))

    (is-not (my-turn? (play-all [[:black [3 3]]]) :black))
    (is (illegal-move? (play-all [[:black [3 3]]]) [:black [4 4]]))

    (is-not (my-turn? (play-all [[:black [3 3]] [:white [3 4]]]) :white))
    (is (illegal-move? (play-all [[:black [3 3]] [:white [3 4]]]) [:white [4 4]])))

  (testing "ko rule"
    (let [moves [[:black [1 0]] [:black [0 1]] [:black [1 2]] [:black [2 1]]
                 [:white [2 0]] [:white [3 1]] [:white [2 2]] [:white [1 1]]]
          setup (play-all moves)]

      (is (ko-rule-violated? setup [:black [2 1]]))
      (is (illegal-move? setup [:black [2 1]])))))


