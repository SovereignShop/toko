(ns recurs.macros)

(defmacro interlace
  "Transform a vector, binding to neighbors of each element. Takes a
  collection, followed by args satisfying (offset[int],binding[sym],expr[expr])+.

  Offset binding forms only execute if they are within the bounds of the input
  collection.

  ex.
  (interlace [1 2 3]
             -1 prev (+ prev)
             1  nxt (+ nxt))
  => [3 8 11]

  ex.
  (interlace [{:id 1} {:id 2} {:id 3}]
            -1 prev (assoc :prev (:id prev))
            1  nxt (assoc :next (:id nxt)))
  => [{:id 1, :next 2} {:id 2, :prev 1, :next 3} {:id 3, :prev 2}]"
  [coll & exprs]
  (let [ret    (gensym "ret-")
        idx    (gensym "idx-")
        length (gensym "length-")
        exprs  (for [[offset binding expr] (partition 3 exprs)]
                 `(assoc! ~idx
                          (let [i#       (+ ~offset ~idx)
                                ~binding (get ~ret i#)]
                            (if (and (>= i# 0) (< i# ~length))
                              (-> (get ~ret ~idx) ~expr)
                              (get ~ret ~idx)))))]
    `(let [~ret    (transient (vec ~coll))
           ~length (int (count ~ret)) ]
       (loop [~ret ~ret
              ~idx (int 0)]
         (if (>= ~idx ~length)
           (persistent! ~ret)
           (recur (-> ~ret ~@exprs)
                  (unchecked-inc-int ~idx)))))))

(defmacro ex [s]
  (if (:ns &env)
    `(js/Error. ~s)
    `(Exception. ~s)))
