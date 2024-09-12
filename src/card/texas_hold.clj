(ns card.texas-hold
  (:require [clojure.math.combinatorics :as combo]))

(def player ["Mine" "Player1" "Player2" "Player3" "Player4" "Player5" "Player6" "Player7" "Player8"])
(def seat ["SB" "BB" "UTG" "UTG+1" "UTG+2" "LJ" "HJ" "CO" "BTN"])
(def suit ["♣" "♠" "♦" "♥"])
(def rank ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"])
(def rank-map (zipmap rank (range)))
(defn get-deck [] (shuffle (vec (for [a suit b rank] [a b]))))
(defn rotate-seat [seat] (concat (take-last 1 seat) (drop-last 1 seat)))
(defn score [cards]
  (let [is-flush (some #(>= (second %) 5) (frequencies (map first cards)))
        count-map (group-by val (frequencies (sort (map #(+ (get rank-map (second %)) 1) cards))))
        rank-count (apply concat (map #(sort (map first (val %))) count-map))
        number-score (apply + (map #(* %2 (Math/pow (count rank) %1)) (range) rank-count))
        is-straight (and (= (- (apply max rank-count) (apply min rank-count)) 4) (= (count rank-count) 5))]
    (cond
      (and is-flush is-straight) ["Straight Flush" (+ number-score 9000000)] ;Straight Flush
      (and (= (count rank-count) 2) (= (apply max (keys count-map)) 4)) ["Four of a Kind" (+ number-score 8000000)]
      (and (= (count rank-count) 2) (= (apply max (keys count-map)) 3)) ["Full House" (+ number-score 7000000)]
      is-flush ["Flush" (+ number-score 6000000)]
      is-straight ["Straight" (+ number-score 5000000)]
      (and (= (count rank-count) 3) (= (apply max (keys count-map)) 3)) ["Three of a Kind" (+ number-score 4000000)]
      (and (= (count rank-count) 3) (= (apply max (keys count-map)) 2)) ["Two Pair" (+ number-score 3000000)]
      (= (count rank-count) 4) ["One Pair" (+ number-score 2000000)]
      :else ["High Card" (+ number-score 1000000)])))
(defn score-deep [hand public]
  (last (sort-by #(second (second %)) (map #(vector % (score %)) (combo/combinations (concat hand public) 5)))))
(defn start [deck ro-seat]
  (let [seat-player (apply hash-map (interleave ro-seat player))
        hands (apply hash-map (interleave seat (partition 2 (take (* 2 (count player)) deck))))
        public (take 5 (drop (* 2 (count player)) deck))
        scores (map #(vector % (seat-player %) (hands %) (score-deep (hands %) public)) seat)]
    (println public)
    (doseq [score_ scores] (println score_))
    (println "WINNER IS" (last (sort-by #(last (flatten %)) scores))))
  (let [continue (read-line)]
    (if (and continue (not= "exit" continue))
      (start (get-deck) (rotate-seat ro-seat))
      (println "EXIT GAME"))))

(defn start-game []
  (start (get-deck) seat))


;(score [["♠" "10"]["♠" "J"]["♠" "Q"]["♠" "K"]["♠" "A"]])