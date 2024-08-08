(ns crinklywrappr.aoc.geom)

(defprotocol Point
  (x [_])
  (y [_]))

(defprotocol LineSegment
  (from [_])
  (to [_])
  (min-x [_])
  (min-y [_])
  (max-x [_])
  (max-y [_])
  (xlen [_])
  (ylen [_]))

(extend-protocol Point
  clojure.lang.PersistentVector
  (x [[x]] x)
  (y [[_ y]] y))

(extend-protocol LineSegment
  clojure.lang.PersistentVector
  (from [[a]] a)
  (to [[_ b]] b)
  (min-x [[a b]] (min (x a) (x b)))
  (min-y [[a b]] (min (y a) (y b)))
  (max-x [[a b]] (max (x a) (x b)))
  (max-y [[a b]] (max (y a) (y b)))
  (xlen [[a b]] (- (x a) (x b)))
  (ylen [[a b]] (- (y a) (y b))))

(defn orientation [p q r]
  (let [delta (- (* (- (y q) (y p)) (- (x r) (x q)))
                 (* (- (x q) (x p)) (- (y r) (y q))))]
    (cond
      (zero? delta) :collinear
      (pos? delta) :clockwise
      :else :counterclockwise)))

(defn on-segment? [l p]
  (and (<= (min-x l) (x p) (max-x l))
       (<= (min-y l) (y p) (max-y l))))

(defn intersects? [a b]
  (let [collinear? (fn [k] (= k :collinear))
        orientation1 (orientation (from a) (to a) (from b))
        orientation2 (orientation (from a) (to a) (to b))
        orientation3 (orientation (from b) (to b) (from a))
        orientation4 (orientation (from b) (to b) (to a))]
    (or (and (not= orientation1 orientation2)
             (not= orientation3 orientation4))
        (and (collinear? orientation1) (on-segment? a (from b)))
        (and (collinear? orientation2) (on-segment? a (to b)))
        (and (collinear? orientation3) (on-segment? b (from a)))
        (and (collinear? orientation4) (on-segment? b (to a))))))

(defn intersection [a b]
  (let [nx (ylen a)
        ny (- (xlen a))
        n (+ (* nx (- (x (from b)) (x (from a))))
             (* ny (- (y (from b)) (y (from a)))))
        d (+ (* (- nx) (- (x (to b)) (x (from b))))
             (* (- ny) (- (y (to b)) (y (from b)))))]
    (if (zero? d)
      -1
      (let [t (/ n d)]
        [(+ (x (from b)) (* t (- (x (to b)) (x (from b)))))
         (+ (y (from b)) (* t (- (y (to b)) (y (from b)))))]))))
