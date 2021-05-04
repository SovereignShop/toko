(ns toko.middleware.reader
  "A middleware that reads tokens into EDN forms."
  (:import [toko.impl.cursor Cursor])
  (:require
   [toko.impl.cursor :as impl]
   [toko.reader :refer [read-tokens read-tokens-reverse]]))

(defprotocol ICursorReader
  (read-form [cursor ctx] [cursor] "Read the current cursor form."))

(defn- read-form*
  ([cursor]
   (let [token (:cursor/token cursor)]
     (case (:token/type token)
       :close-container
       (read-tokens-reverse token)
       (read-tokens token))))
  ([cursor ctx]
   (let [token (:cursor/token cursor)]
     (case (:token/type token)
       :close-container
       (read-tokens-reverse ctx token)
       (read-tokens ctx token)))))

(extend-type Cursor
  ICursorReader
  (read-form [cursor ctx] (read-form* cursor ctx))
  (read-form [cursor] (read-form* cursor)))
