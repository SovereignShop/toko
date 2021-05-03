(ns toko.middleware.reader
  "A middleware that reads tokens into EDN forms."
  (:import [toko.impl.cursor Cursor])
  (:require
   [toko.middleware.protocols :as proto]
   [toko.impl.cursor :as impl]
   [toko.reader :refer [read-tokens]]))

(defprotocol ICursorReader
  (read-form [cursor] "Read the current cursor form."))

(extend-type Cursor
  ICursorReader
  (read-form [cursor] (read-tokens (:cursor/token cursor))))
