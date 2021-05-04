(ns toko.middleware.reader-test
  (:require
   [toko.test-utils :as tu]
   [toko.core :as curs]
   [toko.middleware.reader :as rdr]
   [toko.middleware.eval :as ev]
   #?@(:clj [[clojure.test :as t]]
       :cljs [[cljs.test :as t]])))

(t/deftest test-read-form
  (t/is (= 1
           (-> "1"
               (tu/string->cursor)
               (rdr/read-form))))
  (t/is (= '(+ 1 2)
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (rdr/read-form))))
  (t/is (= '(+ 1 2)
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (rdr/read-form))))
  (t/is (= '{:a 1 b 3 6 10.0}
           (-> "{:a 1 b 3 6 10.0}"
               (tu/string->cursor)
               (rdr/read-form))))

  (t/is (= "a b"
           (-> "\"a b\""
               (tu/string->cursor)
               (rdr/read-form))))

  #_(t/is (= :string                    ; TODO: Fix strings
             (-> "\"asdabd \n\""
                 (tu/string->cursor)
                 (:cursor/token)
                 (:token/value)))))

(t/deftest test-reverse-read
  (t/is (= '(+ 1 2)
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (curs/move [0 6])
               (rdr/read-form)))))

(t/deftest test-eval-token
  (t/is (= 3
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (curs/move [0 0])
               (ev/eval-form))))

  (t/is (= 3
           (-> "(+ 1 2)  "
               (tu/string->cursor)
               (curs/move [0 8])
               (ev/eval-form)))))

(t/deftest test-read-form-meta
  (t/is (number? (-> "{:a 1 b 3}"
                     (tu/string->cursor)
                     (rdr/read-form)
                     (meta)
                     (:token-id)))))
