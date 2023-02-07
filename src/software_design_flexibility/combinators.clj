(ns software-design-flexibility.combinators)

(defn add-one [x] (+ x 1))

(defn exp [x n]
  (->> x
       (repeat n)
       (reduce *)))

(defn arity
  "Returns the maximum arity of defined and anonymous functions
  Returns `nil` if the function is variadic."
  [f]
  (if-let [arity (:arity (meta f))]
    arity
    (let [methods (->> f
                       class
                       .getDeclaredMethods
                       (map #(vector (.getName %)
                                     (count (.getParameterTypes %)))))
          var-args? (some #(-> %
                               first
                               #{"getRequiredArity"})
                          methods)]
      (if var-args?
        nil
        (->> methods
             (filter (comp #{"invoke"} first))
             (sort-by second)
             last
             second)))))

(defn compose [f g]
  (fn [& args] (->> args
                   (apply g)
                   f)))

(defn my-iterate
  "A hof which recurs a function n times"
  [n]
  (fn [f] (if (= n 0)
           identity
           (compose f ((my-iterate (- n 1)) f)))))

(defn parallel-combine
  "A hof which combines the result of calling f and g"
  [combinator f g]
  (fn [& args] (combinator (apply f args) (apply g args))))

(defn spread-combine
  "A hof which splits args between f and g according to their arities and combines the results"
  [combinator f g]
  (let [arity-f     (arity f)
        arity-g     (arity g)
        total-arity (+ arity-f arity-g)]
    (with-meta
      (fn [& args]
                 (assert (= (count args) total-arity))
                 (print arity-f arity-g total-arity)
                 (combinator
                  (apply f (take arity-f args))
                  (apply g (take arity-g (drop arity-f args)))))
      {:arity total-arity})))
