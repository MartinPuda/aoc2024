(ns aoc2024.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def file (str/split-lines "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"))

(def file (-> (io/resource "day04.txt")
              io/reader
              slurp
              str/split-lines))

(defn search-line [line c*]
  (keep-indexed (fn [i c] (when (= c c*) i)) line))

(defn char-pos [file c]
  (->> file
       (keep-indexed (fn [i line] (let [positions (search-line line c)]
                                    (when positions
                                      (mapv #(vector i %) positions)))))
       (apply concat)))

(def paths [[-1 -1] [-1 0] [-1 1]
            [0 -1] [0 1]
            [1 -1] [1 0] [1 1]])

(def len (count (first file)))

(def file-lines (count file))

(defn valid-path? [path]
  (->> path
       (apply concat)
       (every? #(>= (dec len) % 0))))

(defn paths-for-cord [cord]
  (->> paths
       (mapv (fn [path] (take 4 (iterate (fn [res] (mapv + res path)) cord))))
       (filter valid-path?)))

(defn path-to-word [path]
  (->> path
       (mapv (fn [cord] (get-in file cord)))
       (apply str)))

(defn follow-paths [paths]
  (->> paths
       (mapv path-to-word)
       (filter #{"XMAS"})
       count))

(defn part1 []
  (->> (char-pos file \X)
       (mapv paths-for-cord)
       (apply concat)
       follow-paths))

;(part1)
;2370

(defn valid-a-pos? [[y x]]
  (and (pos? x)
       (pos? y)
       (> len x)
       (> file-lines y)))

(def m? #{\M})
(def s? #{\S})

(defn mas-pos? [cord]
  (let [left-top (get-in file (mapv + [-1 -1] cord))
        left-bot (get-in file (mapv + [1 -1] cord))
        right-top (get-in file (mapv + [-1 1] cord))
        right-bot (get-in file (mapv + [1 1] cord))]
    (or (and (m? left-top)
             (m? right-top)
             (s? left-bot)
             (s? right-bot))
        (and (m? left-top)
             (s? right-top)
             (m? left-bot)
             (s? right-bot))
        (and (s? left-top)
             (m? right-top)
             (s? left-bot)
             (m? right-bot))
        (and (s? left-top)
             (s? right-top)
             (m? left-bot)
             (m? right-bot)))))

(defn part2 []
  (->> (char-pos file \A)
       (filter valid-a-pos?)
       (filter mas-pos?)
       count))

;(part2)

;1908

(defn check []
  (and (= (part1) 2370)
       (= (part2) 1908)))

(check)