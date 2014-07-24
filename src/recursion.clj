(ns recursion)

(defn product [coll]
  (cond
   (empty? coll) 1
   :else (* (first coll) (product (rest coll)))))

;(first #{4 1 2 3})

(defn singleton? [coll]
  (if (and
       (empty? (rest coll))
       (not (empty? coll)))
    true
    false))

;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false
;(singleton? [nil])
;(seq? [])

(defn my-last [coll]
  (cond
   (singleton? coll) (first coll)
   (empty? coll) nil
   :else (my-last (rest coll))))

;(my-last [])      ;=> nil
;(my-last [1 2 3]) ;=> 3
;(my-last [2 5])   ;=> 5

(defn max-element-recur [acc a-seq comparator]
  (let [first-elem (first a-seq)]
    (cond
     (nil? first-elem) acc
     (comparator first-elem acc) (max-element-recur first-elem (rest a-seq) comparator)
     :else (max-element-recur acc (rest a-seq) comparator))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   :else (max-element-recur (first a-seq) a-seq >)))

;(max-element [2 4 1 4]) ;=> 4
;(max-element [2])       ;=> 2
;(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (if (> len-1 len-2)
      seq-1
      seq-2)))

;(seq-max [1] [1 2])   ;=> [1 2]
;(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   :else (max-element-recur [] a-seq seq-max)))

;(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;(longest-sequence [[1 2]])            ;=> [1 2]
;(longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   :else (let [first-elem (first a-seq)]
           (cond
            (pred? first-elem) (cons first-elem (my-filter pred? (rest a-seq)))
            :else (my-filter pred? (rest a-seq))))))

;(my-filter odd? [1 2 3 4]) ;=> (1 3)
;(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;(my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   :else (let [first-elem (first a-seq)]
           (cond
            (= first-elem elem) true
            :else (sequence-contains? elem (rest a-seq))))))

;(sequence-contains? 3 [1 2 3]) ;=> true
;(sequence-contains? 3 [4 7 9]) ;=> false
;(sequence-contains? :pony [:horse :pony])  ;=> false

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [first-elem (first a-seq)]
      (if (pred? first-elem)
        (cons first-elem (my-take-while pred? (rest a-seq)))
        '()))))

;(assoc [1 2 3] 2 4)
;(my-take-while odd? [1 2 3 4])  ;=> (1)
;(my-take-while odd? [1 3 4 5])  ;=> (1 3)
;(my-take-while even? [1 3 4 5]) ;=> ()
;(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [first-elem (first a-seq)]
      (if (pred? first-elem)
        (my-drop-while pred? (rest a-seq))
        (seq a-seq)))))

;(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;(my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (let [a-first (first a-seq)
               b-first (first b-seq)]
           (cond
            (= a-first b-first) (seq= (rest a-seq) (rest b-seq))
            :else false))))

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (let [first-1 (first seq-1)
               first-2 (first seq-2)]
           (cons (f first-1 first-2) (my-map f (rest seq-1) (rest seq-2))))))

;(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;(my-map + [1 2 3] [])        ;=> ()

(defn indexed [a-seq]
  (let [indices (range 0 (count a-seq))]
    (map vector indices a-seq)))
(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

;(consecutives [1 2 3])
;(indexed [1 2 3])

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

;(fib 10)

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


;(my-repeat 2 :a)    ;=> (:a :a)
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;(my-repeat -1 :a)   ;=> ()

(defn my-range-from [low high]
  (if (>= low high)
    '()
    (cons low (my-range-from (inc low) high))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

;(my-range 0)  ;=> ()
;(my-range 1)  ;=> (0)
;(my-range 2)  ;=> (1 0)
;(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

;(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

;(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn rotations [a-seq]
  (let [indices (range 0 (count a-seq))]
    (map (fn [sequence rotation-index]
             (concat
              (drop rotation-index sequence)
              (take rotation-index sequence)))
         (repeat (count a-seq) a-seq)
         indices)))

;;;;;;;;;;;;; TODO:
;(rotations []) ;=> (())
;;;;;;;;;;;;;;;;;;;;;;



;(repeat (count []) [])
;(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
;(rotations [:a :b])   ;=> ((:b :a) (:a :b))
;(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;(count (rotations [6 5 8 9 2])) ;=> 5

;(take 0 '(1 2 3))
;(drop 0 '(1 2 3))
;(merge '(3) '(1 2))

;(defn my-frequencies-helper [freqs a-seq]
;  [:-])

(defn my-frequencies [a-seq]
  (reduce
   (fn [freq-map key]
     (assoc freq-map key (inc (freq-map key 0))))
   {}
   a-seq))

;({:a 1 :b 2} :c 0)
;(my-frequencies []) ;=> {}
;(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]
  (reduce
   (fn [a-seq a-key]
     (let [count-key (a-map a-key)]
       (concat a-seq (repeat count-key a-key))))
   '()
   (keys a-map)))

;(keys {:a 1})
;(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (cond
   (or (empty? coll) (zero? n)) '()
   :else (cons (first coll)
               (my-take (dec n) (rest coll)))))

;(my-take 2 [1 2 3 4]) ;=> (1 2)
;(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (cond
   (or (empty? coll) (zero? n)) coll
   :else (my-drop (dec n) (rest coll))))

;(my-drop 2 [1 2 3 4]) ;=> (3 4)
;(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [half-count (int (/ (count a-seq) 2))
        first-half (take half-count a-seq)
        second-half (drop half-count a-seq)]
    [first-half second-half]))

;(halve [1 2 3 4])
;(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;(halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a-first (first a-seq)
               b-first (first b-seq)]
           (cond
            (< a-first b-first) (cons a-first (seq-merge (rest a-seq) b-seq))
            :else (cons b-first (seq-merge a-seq (rest b-seq)))))))

;(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq)) a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

;(merge-sort [1 -2 5 3])
;(merge-sort [])

(defn monotonicity [first second]
  (if (>= second first)
    >=
    <))

(defn take-while-monotonic-helper [prev a-seq order-func]
  (if (empty? a-seq)
    [prev]
    (let [next-elem (first a-seq)
          rest-seq (rest a-seq)]
      (if (order-func next-elem prev)
        (cons prev (take-while-monotonic-helper next-elem rest-seq order-func))
        [prev]))))

(defn take-while-monotonic [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[first-elem next-elem] a-seq
          order-func (monotonicity first-elem next-elem)]
      (take-while-monotonic-helper first-elem (rest a-seq) order-func))))

;(take-while-monotonic [-9 0 -1])

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [first-monotonic (take-while-monotonic a-seq)]
      (cons first-monotonic (split-into-monotonics (drop (count first-monotonic) a-seq))))))

;(split-into-monotonics [1 2 -1 4 -7 -8 -9])
;(map list [1 2] [3 4])
;(get [1 2 3] 0)
;(cons 0 (concat [[1 2]] [3 4]))
;(take 1 [1 2 3])
;(empty? [1])
;(get 0 '(1 2))
;(range 1 (inc (count #{1 2})))
;(get [1 2 3] 0)
;(range 1 0)
;(repeat 2 [1 2])
;(concat '() '(1 2))
;(map #(cons 2 %) [[1 2] [3 4]])
;(concat [1] '(2 3))
;(map #(cons 3 %) '(((1 2)) ((2 1))))
;(reduce (fn [a-list a-list-of-list]
;          (concat a-list a-list-of-list))
;        []
;        [[[1 2] [2 3]] [[4 5] [6 7]]])
;(conj [1 2] 3)

(defn permutations [a-set]
  (let [a-vec (vec a-set)
        indices (range 0 (count a-vec))]
    (if (or (empty? a-vec) (singleton? a-vec))
      [a-vec]
      (apply concat
             (map (fn [index a-vec]
                    (let [the-elem (get a-vec index)
                          first-half (take index a-vec)
                          second-half (drop (inc index) a-vec)
                          rest-permutations (permutations (concat first-half second-half))]
                      (map #(cons the-elem %) rest-permutations)))
                  indices
                  (repeat (count indices) a-vec))))))

(defn my-list? [a-list]
  (or (vector? a-list) (list? a-list) (seq? a-list)))

(defn my-flatten [a-list]
  (if (empty? a-list)
    a-list
    (let [first-elem (first a-list)]
      (if (my-list? first-elem)
        (let [flattened-first-list (my-flatten first-elem)]
          (concat flattened-first-list (my-flatten (rest a-list))))
        (cons first-elem (my-flatten (rest a-list)))))))

(my-flatten '(1 2 '(3 4 '(5 6))))
(my-flatten [1 2 [4 5 [6 7]]])

;(permutations #{1 2 3 4})
(cons 1 #{2 3})

(defn cons-all [an-elem a-list-of-list]
  (map #(cons an-elem %) a-list-of-list))

(cons-all 2 [[3 4] [6 7] #{4 5}])

(defn powerset [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (set [a-set '()])
    (let [the-elem (first a-set)
          rest-set (rest a-set)
          powersets-of-rest (powerset rest-set)
          powersets-containing-the-elem (cons-all the-elem powersets-of-rest)]
      (concat powersets-of-rest powersets-containing-the-elem))))

(count (powerset #{2 3 4 5}))
