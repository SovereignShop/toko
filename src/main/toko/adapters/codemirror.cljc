(ns toko.adapters.codemirror
  "Adapter to use with Codemirror editor."
  (:require
   [clojure.string :as s]
   [toko.core :as curs]))

(defn on-change
  "Apply a change to the model."
  [cursor inserted-text removed-text finish-pos from]

  (cond-> cursor
    (pos? (count inserted-text))
    (curs/insert-text inserted-text
                      from
                      finish-pos)

    (pos? (count removed-text))
    (curs/delete-text from
                      finish-pos
                      (count removed-text))))

(defn on-move
  "Move the cursor to a new pose"
  [cursor pos]
  (curs/move cursor pos))

(defn on-eval-file
  "Evaluate a file."
  [ctx cursor])
