(ns aoc2024.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;(def file (->                                               ;(io/resource "day05.txt")
;            ;io/reader
;            ;slurp
;            "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\r\n\r\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"
;            (str/split #"\r\n\r\n")))

(def file (-> (io/resource "day05.txt")
              io/reader
              slurp
              (str/split #"\r\n\r\n")))

(def p1 (first file))
(def p2 (second file))

(defn parse-line [line]
  (->> (str/split line #",")
       (mapv parse-long)))

(defn check-rule [[r1 r2] update-line]
  ;(prn update-line)
  (if (and (.contains update-line r1)
           (.contains update-line r2))                      ;;aa
    (let [indices (zipmap update-line (range))]
      (> (get indices r2)
         (get indices r1)))
    true))

(defn every-rule-true? [rules update-line]
  (->> rules
       (mapv #(check-rule % update-line))
       (every? true?)))

(defn mid-number [xs]
  (let [len (count xs)]
    (nth xs (int (/ len 2)))))


(defn part1 []
  (let [rules (->> (str/split-lines p1)
                   (mapv #(mapv parse-long (str/split % #"\|"))))
        updates (->> (str/split-lines p2)
                     (mapv parse-line))]
    ; (prn rules)
    ; (prn updates)
    (->> updates
         (filter #(every-rule-true? rules %))
         (map mid-number)
         (reduce +))))

;(part1)
;6034