(ns aoc2024.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;(defn file []
;  (str/split-lines "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"))

(def file
  (-> (io/resource "day01.txt")
      io/reader
      line-seq))

(defn transpose [mx]
  (apply map vector mx))

(defn parse-line [line]
  (->> (str/split line #"\s+")
       (map parse-long)))

(defn part1 []
  (->> file
       (map parse-line)
       transpose
       (map sort)
       (apply map -)
       (map abs)
       (reduce +)))

;1580061

(defn part2 []
  (let [[l r] (->> file
                   (map parse-line)
                   transpose)
        fr (frequencies r)]
    (reduce (fn [acc v] (+ acc (* v (get fr v 0))))
            0 l)))

;23046913

(defn check []
  (and (= (part1) 1580061)
       (= (part2) 23046913)))

(check)