(ns recurs.core
  "Cursor-related functions. The cursor points at a token, and
  tracks information about forms as it is moved. It essentially constintutes
  zipper over clojure forms.

  Constraints:
  - Cursor is either: (1) at

  "
  (:require
   [recurs.impl.cursor :as ic]
   [recurs.utils :refer [newline-token? token-length]]
   [recurs.reader :as rdr]
   [recurs.parser :refer [parse-tokens]]))

(defn past-current-ch?
  "True if cursor token is past `col`"
  [{:keys [cursor/column cursor/token]} col]
  (>= (- col column) (token-length token)))

(defn before-current-ch?
  "True if cursor is before `col`"
  [{:keys [cursor/column]} col]
  (neg? (- col column)))

(defn at-file-beginning?
  "True if the cursor points at the beginning of the file."
  [{:keys [cursor/token]}]
  (nil? (:token/prev-token token)))

(defn line-length
  "Returns the number of characters from the start of the given token to the
beginning of the line."
  [token]
  (loop [ret   (token-length token)
         token (:token/prev-token token)]
    (if (or (nil? token) (newline-token? token)) ret
        (recur (+ ret (token-length token))
               (:token/prev-token token)))))


(defn token-value [cursor]
  (-> cursor :cursor/token :token/value))

(defn next-token-newline
  "Steps the cursor over a newline form."
  [cursor]
  (-> cursor
      (update :cursor/line inc)
      (assoc :cursor/column -1)))

(defn prev-token-newline
  "Steps the cursor over a newline token."
  [cursor]
  (-> cursor
      (update :cursor/line dec)
      (assoc :cursor/column (dec (line-length (:cursor/token cursor))))))

(defn next-token
  "Moves the cursor to the next token."
  [{:keys [cursor/token] :as cursor}]
  (when-let [nxt-token (:token/next-token token)]
    (let [delta       (token-length token)]
      (cond-> cursor
        (newline-token? token)      (next-token-newline)
        true                        (assoc :cursor/token nxt-token)
        true                        (update :cursor/column + delta)))))

(defn prev-token
  "Moves the cursor to the previous token."
  [{:keys [cursor/token] :as cursor}]
  (when-let [prv-token (:token/prev-token token)]
    (let [delta (token-length prv-token)]
      (cond-> cursor
        true                       (assoc :cursor/token prv-token)
        true                       (update :cursor/column - delta)
        (newline-token? prv-token) (prev-token-newline)))))

(defn walk-file [cursor]
  (take-while (complement nil?) (iterate next-token cursor)))

(defn prev-line
  [{:keys [cursor/token] :as cursor}]
  (if (newline-token? token)
    cursor
    (recur (prev-token cursor))))

(defn next-line
  [{:keys [cursor/token] :as cursor}]
  (if (newline-token? token)
    cursor
    (recur (next-token cursor))))

(defn search-line-forward
  "Move the cursor forward, stoppping when it minimizes the line offset
  or get to end of file."
  [{:keys [cursor/line] :as cursor} target-line]
  (let [line-offset (- target-line line)]
    (if (zero? line-offset)
      cursor
      (if-let [nxt (next-token cursor)]
        (recur nxt target-line)
        cursor))))

(defn goto-line-beginning
  [cursor]
  (let [nxt (prev-token cursor)]
    (if (or (nil? nxt) (newline-token? (:cursor/token nxt)))
      cursor
      (recur nxt))))

(defn search-line-backward
  "Move the cursor backward, stopping when it minimizes the line offset
  or gets to beginning of file."
  [{:keys [cursor/line] :as cursor} target-line]
  (let [line-offset (- target-line line)]
    (if (zero? line-offset)
      (goto-line-beginning cursor)
      (if-let [prev (prev-token cursor)]
        (recur prev target-line)
        cursor))))

(defn search-ch-forward
  "Move cursor forward until a token is reached that minimizes the column/line offset"
  ([cursor column]
   (when cursor
     (if (past-current-ch? cursor column)
       (let [nxt (next-token cursor)]
         (if (newline-token? (:cursor/token nxt))
           nxt
           (recur nxt column)))
       cursor))))

(defn search-ch-backward
  "Move cursor backward until at `ch` is reached."
  ([cursor column]
   (when cursor
     (if (before-current-ch? cursor column)
       (let [prev (prev-token cursor)]
         (if (newline-token? (:cursor/token prev))
           cursor
           (recur prev column)))
       cursor))))

(defn cursor-position [cursor]
  (+ (:cursor/column cursor) (:cursor/column-offset cursor)))

(defn goto-nearest-token
  "Search backward/forward, stoping at token nearest `column` on line."
  [cursor column]
  (let [curs (if (past-current-ch? cursor column)
               (or (search-ch-forward cursor column) cursor)
               (or (search-ch-backward cursor column) cursor))]
    (assoc curs :cursor/column-offset (- column (:cursor/column curs)))))

(defn goto-line
  "move cursor to line nearest `target-line`."
  [{:keys [cursor/line] :as cursor} target-line]
  (let [line-offset (- target-line line)]
    (cond (zero? line-offset) cursor
          (pos? line-offset)  (search-line-forward cursor target-line)
          :else               (search-line-backward cursor target-line))))

(defn goto-file-beginning
  "Move cursor to start of file."
  [cursor]
  (if (at-file-beginning? cursor)
    cursor
    (recur (prev-token cursor))))

(defn- splice [s n x]
  #?(:cljs (str (.slice s 0 n) x (.slice s n))
     :clj (str (.substring s 0 n) x (.substring s n))))

(defn move-relative
  [{:keys [cursor/line cursor/column cursor/column-offset] :as cursor} line-delta col-delta]
  (let [target-line (+ line line-delta)
        target-col (+ column column-offset col-delta)]
    (-> cursor
        (goto-line target-line)
        (goto-nearest-token target-col))))

(defn move-prev-char
  [{:keys [cursor/column] :as cursor}]
  (if (zero? column)
    (move-relative cursor -1 0)
    (move-relative cursor 0 -1)))

(defn move
  "Move cursor to `line`, `ch`."
  [cursor [line ch]]
  (-> cursor
      (goto-line line)
      (goto-nearest-token ch)))

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
       [:db/add (:db/id nxt-token) :token/prev-token (:db/id l)]]
      [[:db/add (:db/id a) :token/next-token (:db/id b)]
       [:db/add (:db/id b) :token/prev-token (:db/id a)]])))

(defn token [cursor]
  (:cursor/token cursor))

(declare from-text)

(defn insert-text
  "Insert `text` starting at `start-pos`, then move the cursor to `finish-pos`."
  [cursor text start-pos finish-pos]
  (if-not (:cursor/token cursor)
    (-> cursor
        (ic/transact (from-text {} text (:db/id cursor)))
        (move finish-pos))
    (let [curs        (move cursor start-pos)
          offset      (:cursor/column-offset curs)
          tok         (token curs)
          prv-token   (:token/prev-token tok)
          start-token (if (zero? offset)
                        (or prv-token tok)
                        tok)
          end-tokens  (if (zero? offset)
                        [tok]
                        (if-let [nxt (:token/next-token tok)]
                          [nxt]
                          []))
          insert-idx  (if (and (zero? offset) prv-token)
                        (+ offset (token-length prv-token))
                        offset)
          new-text    (splice (str (str (:token/value start-token))
                                   (apply str (map :token/value end-tokens)))
                              insert-idx
                              text)
          tokens      (parse-tokens new-text
                                    [(:db/id start-token)]
                                    (map :db/id end-tokens))]
      (move (ic/transact (move-prev-char curs) tokens) finish-pos))))

(defn delete-text
  "Delete `n-chars starting at `start`, then move to `finish-pos`."
  [cursor start finish-pos n-chars]
  (let [cursor       (move cursor start)
        start-offset (:cursor/column-offset cursor)
        start-token  (:cursor/token cursor)
        start-id     (:db/id start-token)
        start-slice  (.substring (:token/value start-token) 0 start-offset)]
    (loop [{:keys [cursor/token] :as cursor} cursor
           length                            (- (token-length token) start-offset)
           retractions                       []]
      (if (>= length n-chars)
        (let [end-offset   (- (token-length token) (- length n-chars))
              end-token    (:cursor/token cursor)
              end-id       (:db/id end-token)
              end-slice    (.substring (:token/value end-token) end-offset)
              before-first (:token/prev-token start-token)
              after-last   (:token/next-token end-token)
              tokens       (parse-tokens (str start-slice end-slice)
                                         (list start-id end-id))]
           (-> cursor
              (move start)
              (move-prev-char)
              (ic/transact (if (empty? tokens)
                             (cond-> (conj retractions [:db/retractEntity end-id])
                               (and before-first after-last) (into (join-tokens before-first after-last)))
                             (let [first-token (first tokens)
                                   last-token  (last tokens)]
                               (cond-> (into [] (remove (comp (set [(:db/id first-token) (:db/id last-token)]) second)) retractions)
                                 before-first (conj (assoc first-token :token/prev-token (:db/id before-first)))
                                 after-last   (conj (assoc last-token :token/next-token (:db/id after-last)))
                                 true         (into (map #(dissoc % :token/value) tokens))))))
              (move finish-pos)))
        (let [nxt (next-token cursor)]
          (recur nxt
                 (+ length (token-length (:cursor/token nxt)))
                 (conj retractions [:db/retractEntity (:db/id token)])))))))

(defn from-text
  "Returns the tx-data to create a cursor from `text`."
  ([text]
   (from-text {} text))
  ([base-token text]
   (from-text base-token text -1))
  ([base-token text cursor-id]
   (let [start-token-id -2
         end-token-id -3
         tokens      (conj (into [{:db/id start-token-id
                                   :token/value "\n"
                                   :token/type :newline}]
                                 (parse-tokens text (remove #{cursor-id} (iterate dec -4)) nil base-token))
                           {:db/id end-token-id
                            :token/value "\n"
                            :token/type :newline})
         last-idx (-> tokens count dec)]
     (-> tokens
         (update 0 assoc :token/next-token (-> tokens (nth 1) :db/id))
         (update 1 assoc :token/prev-token start-token-id)
         (update (dec last-idx) assoc :token/next-token end-token-id)
         (update last-idx assoc :token/prev-token (-> tokens (nth (dec last-idx)) :db/id))
         (conj {:db/id                cursor-id
                :editor/id            "na"
                :cursor/stack         []
                :cursor/index         0
                :cursor/line          -1
                :cursor/column        0
                :cursor/column-offset 0
                :cursor/line-offset   0
                :cursor/token         start-token-id})))))

(defn to-text
  "Concatenate token strings together starting form the current cursor position."
  [curs]
  (-> curs :cursor/token (rdr/tokens->string {:meta? false}) second))
