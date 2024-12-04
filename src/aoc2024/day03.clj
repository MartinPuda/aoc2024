(ns aoc2024.day03
  (:require [clojure.java.io :as io])
  (:gen-class))


(def file* "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def file** "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def file (->> (io/resource "day03.txt")
               io/reader
               slurp))

(defn part1 []
  (->> (re-seq #"mul\((\d+),(\d+)\)" file)
       (mapv (fn [[_ a b]] (* (parse-long a)
                              (parse-long b))))
       (reduce +)))

;(part1)
;178538786

(defn parse-token [s]
  (let [[_ n m] (re-find #"mul\((\d+),(\d+)\)" s)]
    (* (parse-long n)
       (parse-long m))))

(defn part2 []
  (->> file
       (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)")
       (reduce (fn [acc token]
                 (case token
                   "do()" (assoc acc :active true)
                   "don't()" (assoc acc :active false)
                   (cond-> acc
                           (:active acc)
                           (update :result + (parse-token token)))))
               {:active true :result 0})
       :result))

;(part2)
;102467299

(defn check []
  (and (= (part1) 178538786)
       (= (part2) 102467299)))

(check)