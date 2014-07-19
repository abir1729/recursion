(ns recursion)

(defn product [coll]
  (cond
   (empty? coll) 1
   :else (* (first coll) (product (rest coll)))))

;(first #{4 1 2 3})

(defn singleton? [coll]
  (if (and
       (empty? (rest coll))
       (not (nil? (first coll))))
    true
    false))

;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false

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
  :-)

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

