(ns toko.middleware.protocols)

(defprotocol ICursorMiddleware
  (next-token [this token]
    "Called each time the cursor steps forward onto a new token.")
  (prev-token [this token]
    "Called each time the cursor steps backward onto a new token."))
