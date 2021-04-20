(ns markov-toy
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.java.io :as jio]))

(defn n-grams [n s]
  (->> (string/split s #"[\s|\n]")
       (partition n 1)))

(defn word? [s]
  (re-matches #"\w+([.,]|)" s))

(defn word-chain [word-transitions]
  (reduce (fn [acc [a b c]]
            (if (and (word? a) (word? b) (word? c))
              (merge-with set/union acc {[a b] #{c}})
              acc))
          {}
          word-transitions))

(defn text->word-chain [s]
  (->> s
       (n-grams 3)
       (word-chain)))

(defn chain->text [chain]
  (string/join " " (concat [(string/capitalize (first chain))] (rest chain))))

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-ZåäöÅÄÖ]+" text))
        result-text (if (or (empty? trimmed-to-last-punct)
                            (< (count trimmed-to-last-punct) 10))
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (clojure.string/replace result-text #"[,| ]$" ".")]
    (clojure.string/replace cleaned-text #"\"" "'")))

(defn over-char-limit [s]
  (< 140 (count s)))

(defn walk-chain
  ([prefix chain]
   (walk-chain prefix chain prefix))
  ([prefix chain result]
   (if (empty? (chain prefix))
     result
     (let [suffix (first (shuffle (chain prefix)))
           new-prefix [(last prefix) suffix]
           new-result (conj result suffix)]
       (if (over-char-limit (chain->text new-result))
         result
         (recur new-prefix chain new-result))))))

(defn generate-text [start-phrase chain]
  (-> start-phrase
      (string/split #" ")
      (walk-chain chain)
      (chain->text)))

(defn process-file [fname]
  (->> fname
       (jio/resource)
       (slurp)
       (text->word-chain)))

(defn load-chain [fname]
  (->> fname
       (jio/resource)
       (clojure.java.io/input-stream)
       (java.util.zip.GZIPInputStream.)
       (clojure.java.io/reader)
       (java.io.PushbackReader.)
       (clojure.edn/read)))

(defn generate-tweet [prefix-list chain]
  (-> prefix-list
      (shuffle)
      (first)
      (generate-text chain)
      (end-at-last-punctuation)))
