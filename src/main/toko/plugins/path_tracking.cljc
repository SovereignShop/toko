(ns toko.plugins.path-tracking
  "Plugin to track the path to the current cursor token."
  (:require
   [toko.utils :as util]))

(derive ::ctx ::list)
(derive ::ctx ::vector)
(derive ::ctx ::map)
(derive ::ctx ::set)

(defn in-context? [path]
  (isa? ::ctx (peek path)))

(defn token->context [token]
  (case (:token/value token)
    "[" ::vector
    "{" ::map
    "(" ::list))

(defn next-seq-idx
  [cursor]
  (let [path (:cursor/path cursor)]
    (cond-> cursor
      (in-context? path) (update :cursor/path pop)
      true               (update :cursor/path conj (:cursor/index cursor)))))

(defn next-map-key
  [cursor token]
  (let [path (:cursor/path cursor)]
    (cond-> cursor
      (in-context? path) (update :cursor/path pop)
      true               (update :cursor/path conj (:token/value token)))))

(defn next-token
  [{:keys [cursor/token cursor/path] :as cursor}]
  (let [ctx (peek path)]
    (cond (util/open-container? token)   (update cursor :cursor/path conj (token->context token))
          (util/close-container? token)  (update cursor :cursor/path pop)
          (util/whitespace-token? token) token
          (= ctx ::map)                  (if (zero? (rem (:cursor/index cursor) 2))
                                           (next-map-key cursor token)
                                           cursor)
          (or (= ctx ::list)
              (= ctx ::vector))          (next-seq-idx cursor))))
