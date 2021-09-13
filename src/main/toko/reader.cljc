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

(defn get-tagged-container [token]
  (loop [prev (:token/prev-token token)
         s ""]
    (cond (= (:token/type prev) :tag) [token (str (:token/value prev) s)]
          (= (:token/type prev) :whitespace) (recur prev (:token/value prev))
          :else false)))

(defn reverse-tokens->string
  ([root-token]
   (reverse-tokens->string 0 (list) root-token {:meta? false}))
  ([root-token opts]
   (reverse-tokens->string 0 (list) root-token opts))
  ([stack-depth sb token {:keys [meta?] :as opts}]
   (if (and (pos? (count sb))
            (zero? stack-depth))
     (if-let [[token s] (get-tagged-container token)]
       [token (apply str s sb)]
       [token (apply str sb)])
     (case (:token/type token)
       :open-container
       (recur (dec stack-depth)
              (cons (:token/value token) sb)
              (:token/prev-token token)
              opts)

       :close-container
       (recur (inc stack-depth)
              (cons (if (and meta? (meta-type? token))
                      (token->string-with-meta token)
                      (:token/value token))
                    sb)
              (:token/prev-token token)
              opts)

       (recur stack-depth
              (cons (if (and meta? (meta-type? token))
                      (token->string-with-meta token)
                      (:token/value token))
                    sb)
              (:token/prev-token token)
              opts)))))

(defn read-next
  ([token]
   (read-next (sci/init {}) token))
  ([ctx token]
   (let [[token s] (tokens->string token)]
     [token (sci/parse-string ctx s)])))

(defn read-last
  ([token]
   (read-last (sci/init {}) token))
  ([ctx token]
   (let [[token s] (reverse-tokens->string token)]
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

(defn rstrip-whitespace
  [token]
  (if (whitespace-token? token)
    (recur (:token/prev-token token))
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
   (sci/parse-string ctx (second (tokens->string token {:meta? true})))))

(defn read-tokens-reverse
  "Takes a sequences of tokens and returns a form."
  ([token]
   (read-tokens-reverse (sci/init {}) token))
  ([ctx token]
   (sci/parse-string ctx (second (reverse-tokens->string token {:meta? true})))))

(defn eval-next
  ([token]
   (let [ctx (sci/init {})]
     (sci/eval-form ctx (second (read-next ctx token)))))
  ([ctx token]
   (sci/eval-form ctx (second (read-next ctx token)))))

(defn eval-last
  ([token]
   (let [ctx (sci/init {})]
     (sci/eval-form ctx (read-tokens-reverse ctx token))))
  ([ctx token]
   (sci/eval-form ctx (read-tokens-reverse ctx token))))

(defn eval-token
  ([cursor]
   (eval-token (sci/init {}) cursor))
  ([ctx {:keys [cursor/token]}]
   (let [token (rstrip-whitespace token)]
     (case (:token/type token)
       :close-container
       (eval-last ctx token)
       (eval-next ctx token)))))

(defn eval-all
  "Evals tokens until eof is reached."
  ([token]
   (eval-all (sci/init {}) token))
  ([ctx token]
   (for [form (read-all ctx token)]
     (sci/eval-form ctx form))))
