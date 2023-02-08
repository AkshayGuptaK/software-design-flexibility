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

(defn with-arity
  "Wraps a function with its arity metadata"
  [f arity]
  (with-meta f {:arity arity}))

(defn compose [f g]
  (let [arity-f (arity f)
        arity-g (arity g)]
    (assert (= arity-f 1))
    (with-arity
      (fn [& args]
        (assert (= (count args) arity-g))
        (->> args
             (apply g)
             f))
      arity-g)))

(defn my-iterate
  "A hof which recurs a function n times"
  [n]
  (fn [f] (if (= n 0)
           identity
           (compose f ((my-iterate (- n 1)) f)))))

(defn parallel-combine
  "A hof which combines the result of calling f and g"
  [combinator f g]
  (let [arity-f (arity f)
        arity-g (arity g)
        arity-c (arity combinator)]
    (assert (= arity-f arity-g))
    (assert (= arity-c 2))
    (with-arity
      (fn [& args]
        (assert (= (count args) arity-f))
        (combinator (apply f args) (apply g args)))
      arity-f)))

(defn spread-combine
  "A hof which splits args between f and g according to their arities and combines the results"
  [combinator f g]
  (let [arity-f     (arity f)
        arity-g     (arity g)
        total-arity (+ arity-f arity-g)
        arity-c     (arity combinator)]
    (assert (= arity-c 2))
    (with-arity
      (fn [& args]
                 (assert (= (count args) total-arity))
                 (combinator
                  (apply f (take arity-f args))
                  (apply g (take arity-g (drop arity-f args)))))
      total-arity)))
