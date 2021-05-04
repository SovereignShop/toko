(ns toko.middleware.eval
  (:import [toko.impl.cursor Cursor])
  (:require
   [toko.impl.cursor :as impl]
   [toko.reader :refer [eval-token]]))

(defprotocol IEvalForm
  (eval-form [cursor ctx] [cursor] "Evaluate form in `ctx` or empty context."))

(extend-type Cursor
  IEvalForm
  (eval-form [cursor ctx] (eval-token ctx cursor))
  (eval-form [cursor] (eval-token cursor)))
