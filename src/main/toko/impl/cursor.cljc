(ns ^:no-doc toko.impl.cursor
  "Extend the Entity namespace to support a customized Cursor entity that supports
  Apply tranctions to the cursor and accumulating transaction data results.

  The goal is to have functions from Cursor -> Cursor that accumulates facts sort of
  monadically. The trouble is we can't extend the Entity type to do this. We also
  can't define our own that that datascript treats as a Entity. So we can't have a thing
  that is an entity to Datascript. Maybe extend it to IAssociatve? Isn't working out.k"
  (:refer-clojure :exclude [flush])
  (:require [#?(:cljs cljs.core :clj clojure.core) :as c]
            [datascript.impl.entity :as ie]
            [datascript.core :as d])
  #?(:clj (:import
           [datascript.impl.entity Entity])))

(declare cursor? join-tokens parse-tokens)

(defprotocol ICursor
  (to-facts [this] "Returns accumulated facts")
  (flush [this] "Applies accumulated facts")
  (transact [this tx] "Apply arbitrary transaction"))

(defn lookup
  ([id entity cache k]
   (lookup id entity cache k nil))
  ([id entity cache k not-found]
   (or (some-> cache (get id) (get k))
       #?(:clj (.valAt entity k)

          :cljs (-lookup entity k))
       not-found)))

(defn- join-tokens
  "Join left of token `b` to right of token `a`. When joining, token `b` might unify
  with token `a`, resulting one fewer tokens in the resulting token sequence.
  For example, joining a whitespace token with a whiteapce token results in
  a single whitespace token."
  [a b]
  (let [[l r] (parse-tokens (str (:token/value a)
                                 (:token/value b))
                            (list (:db/id a) (:db/id b)))
        nxt-token (:token/next-token b)]
    (if (and nxt-token (nil? r))
      [[:db/retractEntity (:db/id b)]
       (assoc l :token/next-token (:db/id nxt-token))
       [:db/add (:db/id nxt-token) :token/prev-token-test-test (:db/id l)]]
      [[:db/add (:db/id a) :token/next-token (:db/id b)]
       [:db/add (:db/id b) :token/prev-token-test-test (:db/id a)]])))

(deftype Cursor [id entity cache facts id-gen]
  #?@(:clj
      [Object
       (toString [e] (str (-> (.-entity (flush e)) ((juxt :cursor/line :cursor/column (comp :token/value :cursor/token))))))
       (hashCode [e] (.hashCode entity)) ; db?
       (equals [e o]
               (if-let [x (cache id)]
                 (.equals (into x entity) o)
                 (.equals entity o)))

       clojure.lang.ILookup
       (valAt [e k]
              (when-let [x (lookup id entity cache k)]
                (if (ie/entity? x)
                  (Cursor. (:db/id x) x cache facts id-gen)
                  x)))
       (valAt [e k not-found]
              (if-let [x (.valAt entity k)]
                (if (ie/entity? x)
                  (Cursor. (:db/id x) x cache facts id-gen)
                  x)
                not-found))

       clojure.lang.Seqable
       (seq [e] (seq (.-entity (flush e))))

       clojure.lang.Associative
       (equiv [e o]       (or (-> cache (get id) (.equiv o)) (.equiv entity o)))
       (containsKey [e k] (or (-> cache (get id) (contains? k)) (.containsKey entity k)))
       (entryAt [e k]     (when-let [x (lookup id entity cache k)]
                            (if (ie/entity? x)
                              (Cursor. (:db/id x) x cache facts id-gen)
                              x)))
       (empty [e]         (throw (UnsupportedOperationException.)))
       (assoc [e k v]     (Cursor. id entity (update cache (:db/id entity) assoc k v) facts id-gen))
       (cons  [e [k v]]   (Cursor. id entity (update cache (:db/id entity) assoc k v) facts id-gen))
       (count [e]         (throw (UnsupportedOperationException.)))]
      :cljs
      [Object
       (toString [_] ^js (.toString entity))
       (equiv [_ other]  ^js (.equiv entity other)) ;; TODO: fix
       (keys [_] ^js (.keys entity))
       (entries [_] ^js (.entries entity))
       (values [_] ^js (.values entity))
       (has [_ attr] ^js (.has entity attr))
       (get [_ attr] ^js (.get entity attr))
       (forEach [_ f] ^js (.forEach entity f))
       (forEach [_ f use-as-this] ^js (.forEach entity f use-as-this))
       (key_set   [_] ^js (.key_set entity))
       (entry_set [_] ^js (.entry_set entity))
       (value_set [_] ^js (.value_set entity))

       ISeqable
       (-seq [this] (-seq (flush this)))

       ILookup
       (-lookup [e k]
                (when-let [x (lookup id entity cache k)]
                  (if (ie/entity? x)
                    (Cursor. (:db/id x) x cache facts id-gen)
                    x)))
       (-lookup [e k not-found]
                (if-let [x (-lookup entity k)]
                  (if (ie/entity? x)
                    (Cursor. (:db/id x) x cache facts id-gen)
                    x)
                  not-found))
       IAssociative
       (-assoc [e k v] (Cursor. id entity (update cache (:db/id entity) assoc k v) facts id-gen))
       (-contains-key? [e k] (or (-> cache (get id) (contains? k)) (contains? entity k)))])

  ICursor
  (to-facts [this]
    (into facts
          (for [[id ent] cache]
            (into {:db/id id}
                  (for [[k v] ent]
                    (if (cursor? v)
                      [k (.-id v)]
                      [k v]))))))
  (flush [this]
    (let [{:keys [db-after tx-data]} (d/with (d/entity-db entity) (into facts (to-facts this)))]
      (Cursor. id (d/entity db-after id) {} (into facts tx-data) id-gen)))
  (transact [this tx]
    (let [fcts                       (into facts (into (to-facts this) tx))
          {:keys [db-after tx-data]} (d/with (d/entity-db entity) fcts)]
      (Cursor. id (d/entity db-after id) {} (into facts tx-data) id-gen))))

(defn transform
  ([cursor f]
   (transact cursor (f cursor)))
  ([cursor f a]
   (transact cursor (f cursor a)))
  ([cursor f a b]
   (transact cursor (f a b)))
  ([cursor f a b & args]
   (transact cursor (apply f cursor a b args))))


(defn cursor
  ([entity]
   (Cursor. (:db/id entity) entity {} [] (iterate dec -1))))

(defn cursor?
  ([x]
   (instance? Cursor x)))
