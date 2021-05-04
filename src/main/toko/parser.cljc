(ns toko.parser
  #?(:cljs (:require-macros [toko.macros :refer [interlace]]))
  (:require
   [toko.tokens :as tokens]
   [clojure.walk :refer [walk postwalk]]
   [toko.utils :refer [seq-tokens]]
   #?(:clj [toko.macros :refer [interlace]])
   [instaparse.core :as insta]
   #?@(:cljs [[ cljs.reader :refer [read-string]]])))


(def token-parser
  (insta/parser
   "sentence = token*
    <token> = string | whitespace | number | float | keyword | symbol | open-container | close-container | quote | newline
    string = #'\"(?:.)*\"'
    quote = !string #'\\''
    newline = !string #'[\\n]'
    whitespace = !string #'[ ,]+'
    number = digit+
    keyword = #':[^\n\\s,\\(\\[\\{\\)\\}\\]]*'
    symbol = !string #'[^:\n\\s,\\(\\[\\{\\)\\}\\]0-9][^\n\\s,\\(\\[\\{\\)\\}\\]]*'
    open-container = #'[\\(\\[\\{]'
    close-container = #'[\\)\\}\\]]'
    float = #'[0-9]+[.][0-9]*'
    <digit> = #'[0-9]+'
   "))

(defn parse-string
  [s]
  (next (token-parser s)))

(defn read-keyword [s]
  (read-string s))

(defn read-symbol [s]
  (read-string s))

(defn read-number [s]
  (read-string s))

(defn update! [trans k f]
  (assoc! trans k (f (get trans k))))

(defn- popn [coll n]
  (if (zero? n)
    coll
    (recur (pop coll) (dec n))))

(defn parse-tokens
  ([s]
   (parse-tokens s (iterate dec -1) nil {}))
  ([s start-ids]
   (parse-tokens s start-ids nil {}))
  ([s start-ids end-ids] (parse-tokens s start-ids end-ids {}))
  ([s start-ids end-ids base]
   (let [tokens (parse-string s)
         ids (vec (take (count tokens) (concat start-ids (iterate dec -1))))
         ids (if end-ids
               (into (popn ids (count end-ids)) end-ids)
               ids)]
     (interlace (for [[[type value] id] (map list tokens ids)
                      token (cond-> [(assoc base
                                            :token/type type
                                            :token/value value
                                            :db/id id)]
                              #_(= type :newline) #_(conj (assoc base
                                                             :token/type :empty
                                                             :token/value ""
                                                             :db/id (- id (count ids)))))]
                  token)
                -1 prev (assoc :token/prev-token (:db/id prev))
                1  nxt  (assoc :token/next-token (:db/id nxt))))))

(comment

  (parse-string "\n")

  (parse-string "\"\" ")


  )
