(ns aoc2024.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn parse-line [line]
  (->> (str/split line #" ")
       (mapv parse-long)))

;(def file (->> "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
;               str/split-lines
;               (mapv parse-line)))

(def file (->> (io/resource "day02.txt")
               io/reader
               line-seq
               (mapv parse-line)))

;(def file* (->> (str/split-lines file)
;                (mapv parse-line)))

(defn adjacent-safe? [row]
  (->> (partition 2 1 row)
       (map (fn [[a b]] (>= 3 (abs (- a b)) 1)))
       (every? true?)))

(defn safe? [row]
  (and (or (apply < row)
           (apply > row))
       (adjacent-safe? row)))

(defn part1 []
  (->> file
       (filter safe?)
       count))

;341
;(part1)

(defn row-variants [row]
  (mapv #(let [[l r] (split-at % row)]
           (concat l (rest r)))
        (range (count row))))

(defn has-safe-variant? [row]
  (->> (row-variants row)
       (some safe?)))

(defn part2 []
  (->> file
       (filter #(or (safe? %)
                    (has-safe-variant? %)))
       count))

;404
;(part2)

(defn check []
  (and (= (part1) 341)
       (= (part2) 404)))

(check)

