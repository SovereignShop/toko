(ns toko.schema)

(def schema
  {:token/first-child     {:db/type :db.type/ref}
   :token/parent          {:db/type :db.type/ref}
   :token/prev-line       {:db/type :db.type/ref}
   :token/next-line       {:db/type :db.type/ref}
   :token/next-token      {:db/type :db.type/ref}
   :token/prev-token      {:db/type :db.type/ref}
   :token/container-token {:db/type :db.type/ref}
   :cursor/token          {:db/type :db.type/ref}})
