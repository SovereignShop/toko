(ns recurs.adapters.codemirror
  "Adapter to use with Codemirror editor."
  (:require
   [recurs.cursor :as curs]))

(defn on-change
  "Apply a change to the model."
  [cursor inserted-text removed-text start-pos finish-pos]
  (cond-> cursor
    (pos? (count inserted-text))
    (curs/insert-text inserted-text
                      start-pos
                      finish-pos)

    (pos? (count removed-text))
    (curs/delete-text start-pos
                      finish-pos
                      (count removed-text))))

(defn on-move
  "Move the cursor to a new pose"
  [cursor pos]
  (curs/move cursor pos))
