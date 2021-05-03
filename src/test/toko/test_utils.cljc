(ns toko.test-utils
  (:require
   #?@(:clj [[clojure.test :as t]
             [datascript.core :as d]
             [toko.schema :refer [schema]]
             [toko.impl.cursor :as ic]
             [toko.utils :as ut]
             [toko.core :as curs]
             [toko.parser :as parser]]
       :cljs [[cljs.test :as t :include-macros true]
              [datascript.core :as d]
              [toko.schema :refer [schema]]
              [toko.utils :as ut]
              [toko.impl.cursor :as ic]
              [toko.core :as curs]
              [toko.parser :as parser]])))

(defn token-values
  [curs]
  (->> curs :cursor/token (ut/seq-tokens) (mapv :token/value)))

(defn token-types
  [s]
  (->> s parser/parse-tokens (mapv :token/type)))

(defn make-cursor [cursor-id token-id]
  {:db/id                cursor-id
   :editor/id            "na"
   :cursor/stack         [{}]
   :cursor/line          0
   :cursor/column        0
   :cursor/column-offset 0
   :cursor/line-offset   0
   :cursor/token          token-id})

(defn string->cursor [s]
  (let [cursor-id -1
        {:keys [db-after tempids]} (d/with (d/empty-db schema)
                                           (curs/from-text {} s cursor-id))]
    (curs/move (ic/cursor (d/entity db-after (tempids cursor-id))) [0 0])))

#?(:clj
   (defmacro assert-props
     [cursor & kvs]
     (let [args (into [] (partition-all 2) kvs)
           test-fn (if (:ns &env) 'cljs.test/is 'clojure.test/is)]
       `(do
          (doseq [[path# v#] ~args]
            (~test-fn
             (= (get-in ~cursor path#) v#)
             (str " " v# " != " (get-in ~cursor path#)  " at path: " path#)))
          ~cursor))))

(defn insert-text-at
  ([cursor text start-pos]
   (insert-text-at cursor text start-pos [0 0]))
  ([cursor text start-pos end-pos]
   (-> cursor
       (ic/transform (comp vector curs/move) start-pos)
       (ic/transform curs/insert-text cursor text start-pos end-pos))))

(defn cursor-move-tx
  [cursor pos]
  (-> cursor
      (ic/transform cursor curs/move pos)))
