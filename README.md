# Toko

Incremental tokenizer, reader, and evaluator for Clojure(Script).
It maintains a set of tokens under arbitrary cursor movement, as well as under
insertion and deletion of text. Tokens are stored as Datascript facts and form a
complete partition of the text (whitespace included), making toko suitable for
implementing automatic, incremental persistence of textual documents as Datascript facts.
It uses sci to read and evaluate token strings.

It can potentially form the basis of a CRDT system, as the document is maintained as a doubly
linked list of "ropes", allowing concurrent updates with minimal collisions. Although not implemented,
it should be possible to efficiently map Datascript tx-data to document changes. You'd likely want each
client to have a cursor for every other client

It also supports middleware, which can be added to track additional information
under cursor movement (depending on your parser). See the protocol specification
for more information.

# Status

Pre-alpha. Usable, but still a work in progress.

# Connecting to an editor

Your entry point will usually be via. an adapter. Currently there's just one adapter to plug into CodeMirror. For example, you could do this to connect CodeMirror using re-posh.


``` clojure
(re-posh/reg-event-ds
 :app.events.editor/change
 (fn reg-editor-change [db [_ cursor-id cm change]]
   (let [cm-cursor     (.getCursor cm)
         inserted-text (s/join \newline (.-text change))
         removed-text  (s/join \newline (.-removed change))
         from          (.-from change)
         facts         (to-facts (cm-adapter/on-change (cursor (d/entity db cursor-id))
                                                       inserted-text
                                                       removed-text
                                                       [(.-line cm-cursor) (.-ch cm-cursor)]
                                                       [(.-line from) (.-ch from)]))]
     facts)))

(re-posh/reg-event-ds
 :app.events.editor/move-cursor
 (fn reg-move [db [_ cursor-id pos]]
   (to-facts (cm-adapter/on-move (cursor (d/entity db cursor-id))
                                 [(.-line pos) (.-ch pos)]))))

(defn add-listeners! [cm-editor cursor-id]
  (doto cm-editor
    (.on "cursorActivity"
         (fn [cm]
           (re-posh/dispatch [:app.events.editor/move-cursor cursor-id (.getCursor cm)])))

    (.on "change"
         (fn [cm change]
           (let [pos  (.getCursor cm)]
             (re-posh/dispatch-sync [:app.events.editor/change cursor-id cm change pos]))))))
```

A full example application will be published soon.
