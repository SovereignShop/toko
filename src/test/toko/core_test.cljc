(ns toko.core-test
  (:require
   #?@(:clj [[clojure.test :as t]
             [toko.core :as curs]
             [toko.impl.cursor :as ic]
             [toko.utils :as ut]
             [toko.test-utils :as tu]]
       :cljs [[cljs.test :as t :include-macros true]
              [toko.core :as curs]
              [toko.impl.cursor :as ic]
              [toko.utils :as ut]
              [toko.test-utils :as tu :include-macros true]])))

(t/deftest test-cursor-next-and-prev-token
  (t/is (= "1" (-> "1 1"
                   (tu/string->cursor)
                   (curs/next-token)
                   (curs/next-token)
                   (:cursor/token)
                   (:token/value))))
  (t/is (= "1"
           (-> "1  2 3"
               (tu/string->cursor)
               (curs/next-token)
               (curs/prev-token)
               (curs/to-text))))

  (t/is (ic/cursor? (-> "1 2" (tu/string->cursor))))

  (t/is (zero? (-> "1  23\n  12"
                   (tu/string->cursor)
                   (:cursor/column-offset)))))

(t/deftest test-move-prev-char
  (t/is (= ["1" 0]
           (-> "(+ 1 2)"
               (tu/string->cursor)
               (curs/move [0 5])
               (curs/move-prev-char)
               (curs/move-prev-char)
               ((juxt curs/token-value :cursor/column-offset)))))

  (t/is (= ["(" 0]
           (-> "(+ 1\n 2)"
               (tu/string->cursor)
               (curs/move [0 0])
               (curs/move [1 1])
               (curs/move-prev-char)
               (curs/move-prev-char)
               ((juxt curs/token-value :cursor/column-offset))))))

(t/deftest test-cursor-move

  (t/is (zero? (-> "1 3 3 :a 3 2"
                   (tu/string->cursor)
                   (tu/assert-props [:cursor/token :token/value] "1")
                   (curs/move [0 11])
                   (tu/assert-props [:cursor/token :token/value] "2"
                                    [:cursor/column-offset] 0)
                   (curs/move [0 0])
                   (tu/assert-props [:cursor/token :token/value] "1"
                                    [:cursor/column-offset] 0)
                   (curs/move [0 2])
                   (:cursor/column-offset))))

  (t/is (zero? (-> "ab cd ef gh ij kl"
                   (tu/string->cursor)
                   (curs/move [0 12])
                   (curs/move [0 11])
                   (curs/move [0 10])
                   (curs/move [0 9])
                   (curs/move [0 10])
                   (curs/move [0 11])
                   (curs/move [0 12])
                   (:cursor/column-offset))))

  (t/is (= 1
           (-> "\n"
               (tu/string->cursor)
               (curs/move [1 0])
               (:cursor/line))))

  (t/is (= [2 0]
           (-> "\n\n\n\n\n"
               (tu/string->cursor)
               (tu/assert-props
                [:cursor/column] 0
                [:cursor/line] 0)
               (curs/move [2 0])
               (tu/assert-props
                [:cursor/column] 0
                [:cursor/line] 2)
               (curs/move [4 0])
               (tu/assert-props
                [:cursor/column] 0
                [:cursor/line] 4)
               (curs/move [2 0])
               ((juxt :cursor/line :cursor/column)))))

  (-> "ab\n"
      (tu/string->cursor)
      (curs/move [0 1])
      (tu/assert-props [:cursor/column] 0
                       [:cursor/column-offset] 1)
      (curs/move [1 0])
      (tu/assert-props [:cursor/line] 1
                       [:cursor/column] 0
                       [:cursor/column-offset] 0
                       [:cursor/token :token/value] "\n"))

  (-> "ab\ncd"
      (tu/string->cursor)
      (curs/move [0 1])
      (tu/assert-props [:cursor/column] 0
                       [:cursor/column-offset] 1)
      (curs/move [1 1])
      (tu/assert-props [:cursor/column] 0
                       [:cursor/column-offset] 1
                       [:cursor/token :token/value] "cd"))

  (t/is (= [1 0 1 "hello"]
           (-> "\nhello\nword"
               (tu/string->cursor)
               (curs/move [1 1])
               ((juxt :cursor/line :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= [0 4]
           (-> "   abc\n\n"
               (tu/string->cursor)
               (curs/move [2 0])
               (curs/move [0 4])
               ((juxt :cursor/line curs/cursor-position)))))

  (t/is (= [0 0]
           (-> "abc\nde\nf\n"
               (tu/string->cursor)
               (curs/move [0 1])
               (curs/move [1 2])
               (curs/move [0 2])
               (curs/move [1 1])
               (curs/move [0 0])
               ((juxt curs/cursor-position :cursor/line)))))

  (t/is (= [0 11 4 "swig.three"]
           (-> "(require '[swig.three :as swig3]\n         '[swig.core :as swig])"
               (tu/string->cursor)
               (curs/move [0 15])
               (curs/move [1 15])
               (curs/move [0 15])
               ((juxt :cursor/line :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= [0 3 "\n"] ;; TODO: fix
           (-> "hello\n\n\n\n"
               (tu/string->cursor)
               (curs/move [3 0])
               ((juxt :cursor/column :cursor/line curs/token-value)))))

  (t/is (= 0
           (-> "abc\n"
               (tu/string->cursor)
               (curs/move [1 0])
               (:cursor/column)))))

(t/deftest test-cursor-move-prev-char
  (t/is (= [0 0 0]
           (-> "\n\n"
               (tu/string->cursor)
               (curs/move [1 0])
               (curs/move-prev-char)
               ((juxt :cursor/column :cursor/column-offset :cursor/line))))))

(t/deftest test-cursor-insert-text
  (t/is (= ["3" " " "1" " " "2" "\n"]
           (as-> "1 2" $
             (tu/string->cursor $)
             (curs/insert-text $ "3 " [0 0] [0 0])
             (curs/move $ [0 0])
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (-> "a"
      (tu/string->cursor)
      (curs/insert-text " " [0 0] [0 0])
      (tu/assert-props [:cursor/token :token/value] " "))

  (t/is (= "cd"
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "\n" [0 3] [1 0])
               (curs/insert-text " " [1 0] [1 1])
               (curs/token-value))))

  (t/is (= [0 0]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "a" [0 1] [0 0])
               ((juxt :cursor/column :cursor/column-offset)))))


  (t/is (= [2 1 "  "]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text " " [0 2] [0 3])
               ((juxt :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= [4 0 "cd"]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text " " [0 3] [0 4])
               ((juxt :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= [6 0 "123"]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text " 123" [0 5] [0 6])
               ((juxt :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= [0 0 "aab"]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "a" [0 0] [0 0])
               ((juxt :cursor/column :cursor/column-offset curs/token-value)))))

  (t/is (= "xy"
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "\n" [0 5] [1 0])
               (curs/insert-text "xy" [1 0] [1 0])
               (curs/token-value))))

  (t/is (= [0 0]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "a" [0 3] [0 0])
               ((juxt :cursor/column :cursor/column-offset)))))

  (t/is (= ["\n" "ab" " " "cd" "\n"]
           (-> "ab cd"
               (tu/string->cursor)
               (curs/insert-text "\n"  [0 0] [1 0])
               (curs/move [0 0])
               (tu/token-values))))

  (t/is (= ["\n"]
           (as-> "" $
             (tu/string->cursor $)
             (curs/insert-text $ "\n" [0 0] [1 0])
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (t/is (= ["1" "\n" "\n"]
           (as-> "1" $
             (tu/string->cursor $)
             (curs/insert-text $ "\n" [0 1] [1 0])
             (curs/move $ [0 0])
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (t/is (= ["1" "\n" "\n"]
           (as-> "1" $
             (tu/string->cursor $)
             (curs/insert-text $ "\n" [0 1] [1 0])
             (curs/move $ [0 0])
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (t/is (= "[\n\n\n\n]"
           (-> ""
               (tu/string->cursor)
               (curs/insert-text "[\n\n\n\n]" [0 0] [0 0])
               (curs/to-text))))

  (t/is (= "1"
           (-> ""
               (tu/string->cursor)
               (curs/insert-text "1" [0 0] [0 0])
               (curs/to-text))))

  (t/is (= "(+ 1\n   2 3 4)"
           (-> "(+ 1 2 3 4)"
               (tu/string->cursor)
               (curs/insert-text "\n  " [0 4] [0 0])
               (curs/to-text))))
  (t/is (= ["ab" "\n" " " "c" "\n"]
           (-> "abc"
               (tu/string->cursor)
               (curs/insert-text "\n" [0 2] [1 0])
               (curs/insert-text " " [1 0] [0 1])
               (curs/move [0 0])
               (tu/token-values))))

  (-> "(require '[swig.three :as swig3]\n        '[swig.core :as swig])"
      (tu/string->cursor)
      (curs/insert-text "\n" [0 25] [1 11])
      (curs/insert-text "           " [1 0] [1 11])
      (curs/delete-text [1 10] [1 11] 1)
      (curs/move [0 0])
      (curs/to-text)
      #_((juxt :cursor/column-offset :cursor/line-offset)))

  (t/is (= [0 0 9 "'"]
           (-> "(require '[swig.three :as swig3])"
               (tu/string->cursor)
               (curs/move [0 13])
               (ic/transact [])
               (curs/move [0 9])
               ((juxt :cursor/column-offset :cursor/line-offset :cursor/column curs/token-value)))))

  (t/is (= "(require '[   swig.three :as swig3])"
           (as-> "(require '[swig.three :as swig3])" $
             (tu/string->cursor $)
             (curs/move $ [0 9])
             (tu/assert-props $
                              [:cursor/column-offset] 0
                              [:cursor/token :token/value] "'")
             (curs/insert-text $ "   " [0 11] [0 14])
             (tu/assert-props $
                              [:cursor/column-offset] 0
                              [:cursor/token :token/value] "swig.three")
             (curs/move $ [0 0])
             (curs/to-text $)))))

(t/deftest test-delete-text
  (t/is (= ["1" "\n"]
           (-> "1 2"
               (tu/string->cursor)
               (curs/delete-text [0 1] [0 0] 2)
               (tu/token-values))))

  (t/is (= ["2" "\n"]
           (-> "1 2"
               (tu/string->cursor)
               (curs/delete-text [0 0] [0 0] 2)
               (tu/token-values))))

  (t/is (= ["12" "  " "78" "\n"]
           (as-> "12 34 \n 56 78" $
             (tu/string->cursor $)
             (curs/delete-text $ [0 3] [0 0] (count "34 \n 56"))
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (t/is (= ["12" " " "6" " " "78" "\n"]
           (as-> "12 34 \n 56 78" $
             (tu/string->cursor $)
             (curs/delete-text $ [0 3] [0 0] (count "34 \n 5"))
             (:cursor/token $)
             (ut/seq-tokens $)
             (mapv :token/value $))))

  (t/is (= "  "
           (-> "ab \n cd"
               (tu/string->cursor)
               (curs/delete-text [0 3] [0 3] 1)
               (curs/token-value))))

  (t/is (= "ac"
           (-> "abc"
               (tu/string->cursor)
               (curs/delete-text [0 1] [0 0] 1)
               (:cursor/token)
               (:token/value)))))
