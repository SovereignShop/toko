(ns recurs.schema)

(def schema
  {:token/first-child     {:db/type :db.type/ref}
   :token/next-token      {:db/type :db.type/ref}
   :token/prev-token      {:db/type :db.type/ref}
   :cursor/token          {:db/type :db.type/ref}})
