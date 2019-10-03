(ns book.demos.bookmarks.textgen)


(defn map-vals
  "Transform a map by applying f to each value"
  [f hmap]
  (zipmap (keys hmap)
          (map f (vals hmap))))

(defn reductions-vals
  "Transform a map by running (reductions f ...) onto the values
   and returning an array-map"
  [f hmap]
  (apply array-map
    (interleave
      (keys hmap)
      (reductions f (vals hmap)))))


;Creating N-Gram & Word Frequency Distributions

(defn file->words
  "Create a sequence of words from a string specifying the filename
   containing the source text"
  [file]
  (-> file
      slurp
      (clojure.string/split #" ")))

(defn make-ngrams
  "Transform a sequence of words to a sequence of vectors of all
   n-length consecutive words, looping at the end to the start"
  [words n]
  (letfn [(rotate [[head & tail]]
            (concat tail [head]))]
    (->> words
         (iterate rotate)
         (take n)
         (apply map vector))))

(defn cumulative-frequencies
  "Take a list and convert to an array-map of cumulative frequencies"
  [xs]
  (->> xs
       frequencies
       (reductions-vals +)))

(defn words->freq-distribution
  "Create an associative of all n-1 length ngram vectors to the
   cumulative frequency distributions of the following word"
  [words n]
  (let [ngrams   (make-ngrams words n)
        n-1gram  (comp vec drop-last)
        grouped  (group-by n-1gram ngrams)]
    (->> grouped
         (map-vals #(map last %))
         (map-vals cumulative-frequencies))))


;Generating Infinite Markov Chains

(defn next-word
  "Choose the next word based on an associative with vectors of n-grams
   and the cumulative frequency distribution of the words following"
  [starting-words freq-distribution]
  (let [cum-freq  (get freq-distribution starting-words)
        total     (second (last cum-freq))
        i         (rand-int total)
        pair-at-i (first
                    (filter #(< i (second %)) cum-freq))
        word-at-i (first pair-at-i)]
    word-at-i))

(defn
  markov-chain
  "Create an infinite sequence of words by repeatedly choosing the next
   word based on the previous n-1 words"
  [starting-words freq-distribution]
  (letfn [(f [words]
             (conj (vec (rest words))
                   (next-word words freq-distribution)))]
    (->> starting-words
         (iterate f)
         (map first))))


;Putting it all together

(defn combine-words
  "Take a sequence of words and convert to a string with spaces between words"
  [words]
  (->> words
       (interpose " ")
       (apply str)))

(defn random-text
  "Generate random text of 'length', starting with 'words', using
   word distributions based on the text in 'file'"
  [words length file]
  (let [n                 (inc (count words))
        freq-distribution (-> file
                              file->words
                              (words->freq-distribution n))]
    (if (contains? freq-distribution words)
      (->> (markov-chain words freq-distribution)
           (take length)
           combine-words))))

(defn random-text-partial
  "Generate random text of 'length', starting with 'words', using
   word distributions based on the text in 'file'"
  [words file]
  (let [n                 (inc (count words))
        freq-distribution (-> file
                              file->words
                              (words->freq-distribution n))]
     freq-distribution))

(defn random-text-partial2
  [words file]
  (let [freq-distribution (random-text-partial words file)]
    (if (contains? freq-distribution words)
      words
      )
    )
  )

(comment
  "Usage example:"
  (random-text ["Physik"] 10 "/tmp/QPL_Quantenprogrammiersprache.txt")

  (random-text ["I" "have"] 10 "/tmp/corpus.trump")


  (-> "/tmp/corpus.trump"
      file->words
      (words->freq-distribution 10)
      )
  )