(ns toko.reader-test
  (:require
   #?@(:clj [[clojure.test :as t]
             [toko.reader :as r]
             [toko.test-utils :as tu]]
       :cljs [[cljs.test :as t :include-macros true]
              [toko.test-utils :as tu]
              [toko.reader :as r]])))

(t/deftest test-read-tokens
  (t/is (= '(+ 1 2)
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (:cursor/token)
               (r/read-first))))
  (t/is (= 1
           (-> "1"
               (tu/string->cursor)
               (:cursor/token)
               (r/read-first))))
  (t/is (= '(+ 1 2)
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (:cursor/token)
               (r/read-first))))
  (t/is (= (list 1 2 3)
           (-> "1 2   3"
               (tu/string->cursor)
               (:cursor/token)
               (r/read-all))))
  (t/is (= '(1 2 (3 4))
           (-> "1 2 (3 4)"
               (tu/string->cursor)
               (:cursor/token)
               (r/read-all)))))

(t/deftest test-eval-tokens
  (t/is (= '(1 2 55)
           (-> "1 2 (transduce (map inc) + (range 10))"
               (tu/string->cursor)
               (:cursor/token)
               (r/eval-all)))))
