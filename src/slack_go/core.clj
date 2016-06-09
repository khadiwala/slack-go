(ns slack-go.core
  (:require [slack-go.svg :refer :all]
            [slack-go.go :refer :all]
            [clojure.set :refer [union]]
            [clojure.string :refer [split lower-case]]
            [clojure.math.numeric-tower :refer [abs]]
            [the.parsatron :refer [run]]
            [clj-http.client :as client]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-body, wrap-json-response]]
            [ring.util.response :as response])
  (:import [org.apache.batik.transcoder.image PNGTranscoder JPEGTranscoder]
           [org.apache.batik.transcoder TranscoderInput TranscoderOutput]
           [java.io StringReader FileOutputStream]))

;;; TODO:
;;; scoring
;;; channel + users
;;; make imgur upload async
;;; improve message formatting
;;; switch to slack RTM? (also have channel replay)


;; constants
(def upload-endpoint "https://api.imgur.com/3/image")
(def client-id "Client-Id e1e3dd0dbe53d5a")
(def default-board-size "9")
(def help
  "Supported Commands
  ```/go start <user1> <user2> [board-dimension]```
  Start a game with `user1` playing black and `user2` playing white. `board-dimension` defaults to 9.
  ```/go play <move>```
  Plays a move. Moves look like `d6`
  ```/go show```
  Image of the current board state
  ```/go pass```
  Allow the other player to go
  ```/go end```
  Finish the game, which allows another game to start on the channel or DM
  ```/go help```
  Display this help text")

;; Board rendering

(defn board->svg [board-state]
  (board [900 900] 180 (:dim board-state) board-state))

(defn stone-str [black white move]
  (cond (black move) "X"
        (white move) "O"
        :else "-"))

(defn board->ascii [{bs :black ws :white dim :dim}]
  (let [bset (set bs)
        wset (set ws)]
    (for [y (range dim)]
      (for [x (range dim)]
        (stone-str bset wset [x y])))))

(defn to-png [svg-string output-fn]
  (let [rdr (new StringReader svg-string)
        inp (new TranscoderInput rdr)
        transcoder (new PNGTranscoder)
        ostream (new FileOutputStream output-fn)
        out (new TranscoderOutput ostream)]
    (.transcode transcoder inp out)
    (.flush ostream)
    (.close ostream)))

;; imgur
(def extract-link (comp :link :data :body))
(defn upload [fn]
  (let [resp (client/post upload-endpoint
                          {:headers {:Authorization client-id}
                           :multipart [{:name "image" :content (clojure.java.io/file fn)}]
                           :as :json})]
    (extract-link resp)))

(defn board->link [board]
  (-> board
      board->svg
      (to-png "out.png"))
  (upload "out.png"))

(defn in-channel-response
  "Make slack response public"
  [text]
  {:headers {"Content-Type" "application/json"}
   :body {:text text
          :response_type "in_channel"}})

(defn parse-int [s]
  (. Integer parseInt s))

(defn slack-pos->i
  "translates postion (like a8 to [0 7])"
  [[alpha & numeric] dim]
  [(- (int alpha) (int \a))
   (- dim (parse-int (apply str numeric)))])

(def game-map (ref {}))

(defn alter-and-upload
  "apply functor to game map and return image link"
  [channel f]
  (-> (alter game-map f)
      channel
      board->link))

(defn initial-game [dim black-player white-player]
  {:black []
   :white []
   :dim dim
   :turn :black
   black-player :black
   white-player :white})

(defn start
  "start a game on the channel, responds with imgur link"
  ([channel un1 un2] (start channel un1 un2 default-board-size))
  ([channel un1 un2 dim]
   (let [black (keyword un1)
         white (keyword un2)
         initial (initial-game (parse-int dim) black white)]
     (dosync
      (if-let [existing (channel @game-map)]
        "game already in progress on channel"
        (in-channel-response
         (str "Started game between "
              un1 " and " un2 ":\n"
              (alter-and-upload channel #(assoc % channel initial)))))))))

(defn play
  "Play move, update memory, return imgur link to new board state"
  [channel user-name coord]
  (in-channel-response
   (dosync
    (let [curr (channel @game-map)
          move [(user-name curr) (slack-pos->i coord (:dim curr))]]
      (if-let [err-string (illegal-move? curr move)]
        err-string
        (alter-and-upload channel #(assoc % channel (play-move curr move))))))))

(defn pass
  "player passes"
  [channel user-name]
  (in-channel-response
   (dosync
    (let [curr (channel @game-map)
          color (user-name curr)]
      (if (my-turn? curr color)
        (alter-and-upload channel #(assoc % channel (assoc :turn (opponent color) :last-move nil)))
        "Not your turn")))))

(defn end
  "Removes the game from memory, allows new game to start on channel"
  [channel user-name]
  (in-channel-response
   (dosync
    (if-let [curr (channel @game-map)]
      (do (alter game-map #(dissoc % channel))
          "finished game")
      "No game in progress"))))

(defn show
  "Respond with imgur link of current board state"
  [channel user]
  (in-channel-response
   (if-let [curr (channel @game-map)]
     (board->link curr)
     "No game in progress")))

(defn posted [channel-name user-name text]
  (let [[cmd & args] (split (lower-case text) #"\s+")
        channel-key (keyword channel-name)
        user-key (keyword (str "@" user-name))]
    (prn text)
    (cond
      (= cmd "start") (apply start channel-key args)
      (= cmd "play") (apply play channel-key user-key args)
      (= cmd "pass") (apply pass channel-key user-key args)
      (= cmd "end") (apply end channel-key user-key args)
      (= cmd "show") (show channel-key user-key)
      (= cmd "help") (in-channel-response help))))

(defroutes app-routes
  (POST "/go" [channel_name user_name text]
    (posted channel_name user_name text))
  (route/resources "/")
  (route/not-found "not found"))

(def app
  (-> (wrap-defaults app-routes api-defaults)
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "5000"))]
    (jetty/run-jetty app {:port port})))
