(ns toko.reader
  "Token level reader."
  (:require
   [sci.core :as sci]
   [toko.utils
    :refer [whitespace-token?]])
  #?(:cljs
     (:import [goog.string StringBuffer])))

(defn read-token [ctx token]
  (sci/parse-string ctx (:token/value token)))

(defn token->string-with-meta [token]
  (str "^" {:token-id (:db/id token)}
       " " (:token/value token)))

(defn meta-type? [token]
  (contains? #{:open-container :symbol} (:token/type token)))

(defn tokens->string
  ([root-token]
   (tokens->string 0 (StringBuffer.) root-token {:meta? false}))
  ([root-token opts]
   (tokens->string 0 (StringBuffer.) root-token opts))
  ([stack-depth sb token {:keys [meta?] :as opts}]
   (if (and (pos? #?(:cljs (.getLength sb)
                     :clj (.length sb)))
            (zero? stack-depth))
     [token (str sb)]
     (case (:token/type token)
       :close-container
       (recur (dec stack-depth)
              (.append sb (:token/value token))
              (:token/next-token token)
              opts)

       :open-container
       (recur (inc stack-depth)
              (.append sb
                       (if (and meta? (meta-type? token))
                         (token->string-with-meta token)
                         (:token/value token)))
              (:token/next-token token)
              opts)

       (recur stack-depth
              (.append sb (if (and meta? (meta-type? token))
                            (token->string-with-meta token)
                            (:token/value token)))
              (:token/next-token token)
              opts)))))

(defn- read-next
  ([token]
   (read-next (sci/init {}) token))
  ([ctx token]
   (let [[token s] (tokens->string token)]
     [token (sci/parse-string ctx s)])))

(defn read-first
  ([token]
   (read-first (sci/init {}) token))
  ([ctx token]
   (second (read-next ctx token))))

(defn strip-whitespace
  [token]
  (if (whitespace-token? token)
    (recur (:token/next-token token))
    token))

(defn read-all
  ([token]
   (read-all (sci/init {}) token))
  ([ctx token]
   (lazy-seq
    (let [[nxt-token form] (read-next ctx token)
          nxt (strip-whitespace nxt-token)]
      (cons form (when nxt (read-all ctx nxt)))))))

(defn read-tokens
  "Takes a sequences of tokens and returns a form."
  ([token]
   (read-tokens (sci/init {}) token))
  ([ctx token]
   (sci/parse-string ctx (tokens->string token {:meta? true}))))

(defn eval-all
  "Evals tokens until eof is reached."
  ([token]
   (eval-all (sci/init {}) token))
  ([ctx token]
   (for [form (read-all ctx token)]
     (sci/eval-form ctx form))))
