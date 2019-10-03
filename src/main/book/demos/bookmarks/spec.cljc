(ns book.demos.bookmarks.spec
  (:require
    [clojure.test.check.generators :as gen]
    [miner.strgen :as sg]
    [clojure.spec.gen.alpha :as sgen]
    [clojure.spec.alpha :as s]))


(def uri-regex2 #?(:clj #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)?(\?([^#]*))?(#(.*))?$"
                  :cljs #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)?(\?([^#]*))?(#(.*))?$"))

(defn title-generator
  [size]
  (gen/sample-seq (gen/return (book.demos.bookmarks.textgen/random-text ["I" "am"] size "/tmp/corpus.trump")))
  )


(s/def ::id string?)
(s/def ::title (s/spec string?
                       :gen #(title-generator 15)))
(s/def ::dateAdded pos-int?)
(s/def ::tags string?)
(s/def ::uri (let [re uri-regex2]
               (s/spec (s/and string? #(re-matches re %))
                       :gen #(sg/string-generator re)
                       )))
(s/def ::children (s/* :chrome.bookmarks/bookmark))

(s/def :chrome.bookmarks/bookmark
  (s/keys :req-un [::id ::uri ::title ::dateAdded]
          :opt-un [::tags ::children]
          ))

(comment

  (s/conform :chrome.bookmarks/bookmark
             {:id "0"
              :title "some title"
              :uri "http://www.web.de"
              :dateAdded 1221321
              }
             )

  (sg/string-generator uri-regex2)

  (binding [s/*recursion-limit* 3]
    (sgen/generate (s/gen :chrome.bookmarks/bookmark)))

  )
