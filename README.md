Incremental and isomorphic tokenizer, reader, and evaluator for Clojure(Script).
It maintains a set of tokens under arbitrary cursor movement, as well as under
insertion and deletion of text. Tokens are stored as Datascript facts and form a
complete partition of the text (whitespace included), making toko suitable for
implementing automatic persistence of textual documents as facts. Currently it
only provides a tokenizer for Clojure/Clojurescript, but it can also be extended
to other languages by implementing a token grammar using
[instaparse](https://github.com/engelberg/instaparse).

Toko does not necessary have to be used as a semantic parser. For example,
you could have one token type of just a fixed-size string and think of tokens
as simply ropes. 

It also supports middleware, which can be added to track additional information
under cursor movement (depending on your parser). See the protocol specification
for more information.
