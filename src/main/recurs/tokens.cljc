(ns recurs.tokens
  (:refer-clojure :exclude [newline keyword symbol]))

(defn whitespace [value]
  {:token/type :whitespace
   :token/value value})

(defn newline []
  {:token/type :newline
   :token/value :newline})

(defn number [n]
  {:token/type :number
   :token/value n})

(defn keyword [key]
  {:token/type :keyword
   :tokne/value key})

(defn string [s]
  {:token/type :keyword
   :token/value s})

(defn symbol [sym]
  {:token/type :symbol
   :token/value sym})

(defn quote []
  {:token/type :quote
   :token/value 'quote})

(defn open-container [s]
  {:token/type :open-container
   :token/value s})

(defn close-container [s]
  {:token/type :close-container
   :token/value s})
