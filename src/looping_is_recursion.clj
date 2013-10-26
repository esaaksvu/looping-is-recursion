(ns looping-is-recursion)

(defn power [n k]
  (let [helper (fn[pow res]
       (cond
         (= n 0) 0
         (= k 0) 1
         (= pow 1) res
         :else (recur (dec pow) (* n res))))]
    (helper k n)))

(defn last-element [a-seq]
   (let [help (fn [a]
        (if (empty? (rest a))
          (first a)
            (recur (rest a))))]
     (help a-seq)))


(defn seq= [seq1 seq2]
  (let [help (fn [a b]
        (cond
          (and (empty? a) (empty? b)) true
          (not (= (count a) (count b))) false
          (not (= (first a) (first b))) false
          :else (recur (rest a) (rest b))))]
    (help seq1 seq2)))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq)
    nil
      (loop [indeksi 0
             sequenc a-seq]
        (cond
          (pred (first sequenc)) indeksi
          (empty? (rest sequenc)) nil
          :else (recur (inc indeksi) (rest sequenc))))))


(defn avg [a-seq]
  (loop [i 1
         s a-seq
         sum 0]
    (cond
      (empty? (rest s)) (/ (+ (first s) sum) i)
      :else (recur (inc i) (rest s) (+ (first s) sum)))))


(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
      (conj a-set elem)))


(defn parity [a-seq]
  (loop [odds (set ())
         s a-seq]
    (cond
      (empty? s) odds
      :else (recur (toggle odds (first s)) (rest s)) )))


(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

