(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   1
                   (if (== n 1)
                     acc
                     (recur (* acc base) (dec n)))))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last a-seq]
                 (if (empty? a-seq)
                   last
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (if (and (empty? seq1) (empty? seq2))
                   true
                   (if (or (empty? seq1) (empty? seq2))
                     false
                     (if (not (== (first seq1) (first seq2)))
                       false
                       (recur (rest seq1) (rest seq2))))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         seq a-seq]
    (if (empty? seq)
      nil
      (if (pred (first seq))
        idx
        (recur (inc idx) (rest seq))))))

(defn avg [a-seq]
  (loop [n 0
         tot 0
         seq a-seq]
    (if (empty? seq)
      (/ tot n)
      (recur (inc n) (+ tot (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? (set a-set) elem)
    (disj (set a-set) elem)
    (conj (set a-set) elem)))

(defn parity [a-seq]
  (loop [src a-seq
         dst '()]
    (if (empty? src)
      dst
      (recur (rest src) (toggle dst (first src))))))

(defn fast-fibo [n]
  (loop [c 1
         fnn 1
         fn1 0]
    (if (zero? n)
      0
      (if (= c n)
        fnn
        (recur (inc c) (+ fnn fn1) fnn)))))

(defn my-add [a-set elem]
  (if (not (some #(= elem %) a-set))
    (conj a-set elem)))

(defn cut-at-repetition [a-seq]
  (loop [src a-seq
         dst '()]
    (if (or (empty? src) (some #(= (first src) %) dst))
      (reverse dst)
      (recur (rest src) (my-add dst (first src))))))
