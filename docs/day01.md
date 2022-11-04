# Day 1: Sonar Sweep

Link to [problem](https://adventofcode.com/2021/day/1).

Code: [Clojure](../clojure/aoc/src/aoc/day01.clj), [Scala](../scala/aoc/src/main/scala/adventofcode/Day1/Day1.scala)

## Introduction

In part 1, we are given a series of depth measurements and asked to 
calculate the number of times a measurement increases from the previous 
value.

The second part extends this idea slightly, and we are asked to consider 
three-measurement sliding windows. The task is now to count the number of times
the sum of a three-measurement window increases.

## Solution

We can solve both parts using the same approach. In particular, consider the 
situation for part 1: an increase only occurs if the element at index `i + 
1` is greater than the previous element (index `i`). Now consider, the 
second part, an increase of the sum of a three-measurement window only 
occurs if the element at index `i + 3` is greater than the element at index 
`i`. To see why, consider the illustration below:

```
i   i+1 i+2 i+3
---------------
a   b   c  
    b   c   d  
```

In any consecutive three-measurement windows, two elements overlap and 
increase occurs only if `d > a`. As such, the solution for both parts only 
differs by which elements we are comparing. This approach is implemented below,

```clojure
(defn num-increases
  "Number of increases of the sums of n-element sliding windows"
  [n coll]
  (->> (map > (drop n coll) coll )
       (filter true?)
       count))
```

For part 1, we compare directly with the next element:

```clojure
(num-increases 1 coll)
```

For part 2, we must compare with the element 3 indices away:

```clojure
(num-increases 3 coll)
```
