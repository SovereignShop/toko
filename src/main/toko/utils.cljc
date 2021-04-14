(ns toko.utils
  (:require
   [toko.impl.cursor :refer [cursor?]]
   [datascript.impl.entity :refer [entity?]]))

(defn open-container? [token]
  (= (:token/type token) :open-container))

(defn close-container? [token]
  (= (:token/type token) :close-container))

(defn list-open? [token]
  (= (:token/value token) "("))

(defn list-close? [token]
  (= (:token/value token) ")"))

(defn map-open? [token]
  (= (:token/value token) "{"))

(defn map-close? [token]
  (= (:tokne/value token) "}"))

(defn set-open? [token]
  (= (:token/value token) "#{"))

(defn set-close? [token]
  (= (:token/value token) "}"))

(defn vector-open? [token]
  (= (:tokne/value token) "["))

(defn vector-close? [token]
  (= (:token/value token) "]"))

(defn newline-token? [token]
  (= (:token/type token) :newline))

(defn string-token? [token]
  (= (:token/type token) :string))

(defn quote-token? [token]
  (= (:token/type token) :quote))

(defn whitespace-token? [token]
  (or (= (:token/type token) :whitespace)
      (= (:token/type token) :newline)))

(defn seq-tokens [token]
  (take-while cursor? (iterate :token/next-token token)))

(defn rseq-tokens [token]
  (take-while entity? (iterate :token/prev-token token)))

(defn get-line [token]
  (->> token rseq-tokens next (filter #(= (:token/type %) :newline)) count))

(defn token-length [token]
  #?(:cljs (.-length (:token/value token))
     :clj (.length (:token/value token))))

(defn token-string-length [token]
  #?(:cljs (.-length (:token/value token))
     :clj (.length (:token/value token))))

(defn get-column [token]
  (loop [col 0
         token token]
    (if (or (nil? token) (newline-token? token))
      col
      (recur (+ col (token-length token)) (:token/prev-token token)))))

(defn get-pos [token]
  (let [pos [(get-line token) (get-column token)]]
    pos))
