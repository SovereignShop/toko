(ns toko.middleware.reader-test
  (:require
   [toko.test-utils :as tu]
   [toko.middleware.reader :as rdr]
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
               (rdr/read-form)))))

(t/deftest test-read-form-meta
  (t/is (number? (-> "{:a 1 b 3}"
                     (tu/string->cursor)
                     (rdr/read-form)
                     (meta)
                     (:token-id)))))
