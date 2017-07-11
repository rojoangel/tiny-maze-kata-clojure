(ns tiny-maze.solver-test
  (:require [clojure.test :refer :all]
            [tiny-maze.solver :refer :all]))

(deftest test-solve-maze
  (comment (testing "can find way to exit with 3x3 maze"
             (let [maze [[:S 0 1]
                         [1 0 1]
                         [1 0 :E]]
                   sol [[:x :x 1]
                        [1 :x 1]
                        [1 :x :x]]]
               (is (= sol (solve-maze maze))))))

  (comment (testing "can find way to exit with 4x4 maze"
             (let [maze [[:S 0 0 1]
                         [1 1 0 0]
                         [1 0 0 1]
                         [1 1 0 :E]]
                   sol [[:x :x :x 1]
                        [1 1 :x 0]
                        [1 0 :x 1]
                        [1 1 :x :x]]]
               (is (= sol (solve-maze maze)))))))

(defn first-step [maze]
  (map (partial replace {:S :y}) maze))

(deftest test-first-step
  (testing "first step replaces start with :x"
    (let [maze [[:S 0 1]
                [1 0 1]
                [1 0 :E]]
          sol-step1 [[:y 0 1]
                     [1 0 1]
                     [1 0 :E]]]
      (is (= sol-step1 (first-step maze))))))

(defn possible-mazes [maze]
  [[[:x :y 1]
    [1 0 1]
    [1 0 :E]]])

(deftest test-possible-mazes
  (testing "returns all possible mazes"
    (let [maze [[:y 0 1]
                [1 0 1]
                [1 0 :E]]
          sol [[[:x :y 1]
                [1 0 1]
                [1 0 :E]]]]
      (is (= sol (possible-mazes maze)))))
  (testing "returns all possible mazes"
    (let [maze [[:y 0 1]
                [0 0 1]
                [1 0 :E]]
          sol [[[:x :y 1]
                [0 0 1]
                [1 0 :E]]
               [[:x 0 1]
                [:y 0 1]
                [1 0 :E]]]]
      (is (= sol (possible-mazes maze))))))

(defn same-row? [num-cols idx y]
  (= (quot y num-cols) (quot idx num-cols)) )

(defn down [num-col idx] (+ idx num-col))

(defn up [num-col idx] (- idx num-col))

(def right inc)

(def left dec)

(defn neighbors-idxs [size num-colums idx]
  (let [ud (filter #(<= 0 %1 (dec size))
                   ((juxt (partial up num-colums) (partial down num-colums))
                     idx))
        lr (filter #(and (pos? %)
                     (same-row? num-colums idx %))
                   ((juxt left right) idx))]
    (concat ud lr)))






(comment
  (def maze [[:y 0 1]
             [1 0 1]
             [1 0 :E]])
  )