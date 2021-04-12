
Incremental tokenizer, reader, and evaluator for Clojure(Script). It maintains a
set of tokens under arbitrary cursor movement, as well as under insertion and
deletion of text. Tokens are stored as Datascript facts, making it suitable for
implementing automatic persistence of textual documents as facts. Currently it
only provides a tokenizer for Clojure/Clojurescript, but it can also be extended to
other by implementing a token grammar using instaparse.

It also supports middleware, which can be added to track additional information under
cursor movement.
