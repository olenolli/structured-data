(ns structured-data)

(defn do-a-thing
  [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff
  "takes a vector and returns the sum of the first and third elements of the vector."
  [v]
  (+ (get v 0) (get v 2)))

(defn cutify
  "takes a vector as a parameter and adds \"<3\" to its end."
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  "destructures its parameter"
  [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn width
  "return the width of the given rectangle."
  [rectangle]
  (let [[[bl-x bl-y][tr-x tr-y]] rectangle]
    (- tr-x bl-x)))

(defn height
  [rectangle]
  ;  [bottom-left top-right]
  "return the height of the given rectangle."

  (let [[[bl-x bl-y][tr-x tr-y]] rectangle]
    (- tr-y bl-y)))

(defn square?
  [rectangle]
  ;  [bottom-left top-right]
  "returns true if rectangle is a square and otherwise false."

  (if (= (height rectangle) (width rectangle)) true false))

(defn area
  [rectangle]
  ;  [bottom-left top-right]
  "returns the area of the given rectangle"

  (* (height rectangle) (width rectangle)))

(defn contains-point?
  [rectangle point]
  ;  rectangle [bottom-left top-right]
  ;  point [x y]
  "returns true if rectangle contains point and otherwise false."

  (let [ [ [bl-x bl-y] [tr-x tr-y] ] rectangle
         [x y] point ]

    (if (and (<= bl-x x tr-x)
             (<= bl-y y tr-y)) true false)))

(defn contains-rectangle?
  [outer inner]
  ;  rectangle [bottom-left top-right]

  "returns true if the rectangle inner is inside the rectangle outer and otherwise false."

  (let [ [ [obl-x obl-y] [otr-x otr-y] ] outer
         [ [ibl-x ibl-y] [itr-x itr-y] ] inner ]

    (if (and (<= obl-x ibl-x itr-x otr-x)
             (<= obl-y ibl-y itr-y otr-y)) true false)))


(defn title-length [book]
  "counts the length of the book’s title."
  (count (:title book)))

(defn author-count [book]
  "returns the amount of authors that book has."
  (count (:authors book)))

(defn multiple-authors? [book]
  "returns true if book has multiple authors, otherwise false."
  (< 1 (author-count book)))

(defn add-author [book new-author]
  "takes a book and an author as a parameter and adds author to books authors."

  (let [updated-authors (conj (get book :authors) new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  "returns true if the author is alive, otherwise false."
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  "returns the lengths of every item in collection."
  (map count collection))

(defn second-elements [collection]
  "takes a vector of vectors and returns a sequence of the second elements."

  (let [get-second-element (fn [col] (get col 1))]
        (map get-second-element collection)))

(defn titles [books]
  "takes a collection of books and returns their titles."
  (map :title books))

(defn monotonic? [a-seq]
  "returns true if a-seq is monotonic and otherwise false."
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  "returns a string with n aterisks *"
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  "returns true if sequence contains some element multiple times. Otherwise it returns false."
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
