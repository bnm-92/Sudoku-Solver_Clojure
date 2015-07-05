(ns midterm2.core
  (:gen-class))

(use 'clojure.set)

(def sudoku-board 
    [[5 3 0 0 7 0 0 0 0]
    [6 0 0 1 9 5 0 0 0]
    [0 9 8 0 0 0 0 6 0]
    [8 0 0 0 6 0 0 0 3]
    [4 0 0 8 0 3 0 0 1]
    [7 0 0 0 2 0 0 0 6]
    [0 6 0 0 0 0 2 8 0]
    [0 0 0 4 1 9 0 0 5]
    [0 0 0 0 8 0 0 7 9]])

(def solved-board
    [[5 3 4 6 7 8 9 1 2]
    [6 7 2 1 9 5 3 4 8]
    [1 9 8 3 4 2 5 6 7]
    [8 5 9 7 6 1 4 2 3]
    [4 2 6 8 5 3 7 9 1]
    [7 1 3 9 2 4 8 5 6]
    [9 6 1 5 3 7 2 8 4]
    [2 8 7 4 1 9 6 3 5]
    [3 4 5 2 8 6 1 7 9]])

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn row-values-cal [sudoku-board coordinates]
	(clojure.set/union #{} (if (>= (coordinates 1) 9)
		(do #{})
		(do (clojure.set/union (conj #{} (get-in sudoku-board coordinates)) 
			(clojure.set/union #{} (row-values-cal sudoku-board [(coordinates 0) (inc(coordinates 1))]))
			))
		))
	)

(defn row-values [sudoku-board coordinates]
	(row-values-cal sudoku-board [(coordinates 0) 0])
	)

(defn col-values-cal [sudoku-board coordinates]
	(clojure.set/union #{} (if (>= (coordinates 0) 9)
		(do #{})
		(do (clojure.set/union (conj #{} (get-in sudoku-board coordinates)) 
			(clojure.set/union #{} (col-values-cal sudoku-board [(inc(coordinates 0)) (coordinates 1)]))
			))
		))
	)

(defn col-values [sudoku-board coordinates]
	(col-values-cal sudoku-board [0 (coordinates 1)])
	)

(defn mod-row [number]
	(if (< number 3)
		3
		(if (< number 6)
			6
			9)))

(defn mod-col [number]
	(if (< number 3)
		3
		(if (< number 6)
			6
			9)))

(defn top-left [coordinates]
	[(- (mod-row (coordinates 0))3)  (- (mod-col (coordinates 1))3)]) 

(defn all-three [coordinates]
	(into [] (for [number [0 1 2]]
		[(coordinates 0) (+ (coordinates 1) number)]
		)))

(defn all-nine [coordinates]
	(into [] (concat (all-three [(coordinates 0) (coordinates 1)]) 
		(all-three [(inc(coordinates 0)) (coordinates 1)])
		(all-three [(inc(inc(coordinates 0))) (coordinates 1)])
		)))

(defn convert-set [vect]
	(if (= (count vect) 0)
		(do #{})
		(do (clojure.set/union (conj #{} (first vect)) 
			(clojure.set/union #{} (convert-set (rest vect)))
			))
		))

(defn block-values [sudoku-board coordinates]
		(convert-set (into [] (for [cor-vec (all-nine (top-left coordinates))]
		(get-in sudoku-board cor-vec)))))

(defn valid-values-for [sudoku-board coordinates]
	(if (= (get-in sudoku-board coordinates) 0)
		(do (clojure.set/difference all-values (clojure.set/union (row-values sudoku-board coordinates) (col-values sudoku-board coordinates) 
			(block-values sudoku-board coordinates))))
		#{}
	)
)

(defn find-empty-point [sudoku-board]
	(first (into [] (remove #(= nil %) (into [] (for [row [0 1 2 3 4 5 6 7 8]
		col [0 1 2 3 4 5 6 7 8]]
		(if (= (get-in sudoku-board [row col]) 0)
			[row col]))))))
	)

(defn filled? [sudoku-board]
	(if (= (find-empty-point sudoku-board) nil)
		true false))

(defn get-all-rows [sudoku-board coordinates]
	(remove #(= #{} %) (if (>= (coordinates 0) 9)
		(do (conj [] #{}))
		(do (conj  (get-all-rows sudoku-board [(inc (coordinates 0)) (coordinates 1)])
		 (row-values sudoku-board coordinates)))
		))
	)

(defn rows [sudoku-board]
	(into [] (get-all-rows sudoku-board [0 0]))
	)

(defn get-all-cols [sudoku-board coordinates]
	(remove #(= #{} %) (if (>= (coordinates 1) 9)
		(do (conj [] #{}))
		(do (conj  (get-all-cols sudoku-board [(coordinates 0) (inc(coordinates 1))])
		 (col-values sudoku-board coordinates)))
		))
	)

(defn cols [sudoku-board]
	(into [] (get-all-cols sudoku-board [0 0]))
	)

(defn get-all-blocks [sudoku-board coordinates]
	(remove #(= #{} %) (if (>= (coordinates 0) 9)
		(do (conj [] #{}))
		(do (conj  (get-all-blocks sudoku-board [(+ 3 (coordinates 0)) (coordinates 1)])
		 (block-values sudoku-board coordinates)))
		))
	)

(defn blocks [sudoku-board]
	(into [](concat (get-all-blocks sudoku-board [0 0]) (get-all-blocks sudoku-board [0 3])
		(get-all-blocks sudoku-board [0 6]))
	))

(defn valid-rows? [sudoku-board]
	(if (= (count (remove #(= #{} %)(mapv #(clojure.set/difference all-values %) (rows sudoku-board))))
		0) true false)
	)

(defn valid-cols? [sudoku-board]
	(if (= (count (remove #(= #{} %)(mapv #(clojure.set/difference all-values %) (cols sudoku-board))))
		0) true false)
	)
(defn valid-blocks? [sudoku-board]
	(if (= (count (remove #(= #{} %)(mapv #(clojure.set/difference all-values %) (blocks sudoku-board))))
		0) true false)
	)

(defn valid-solution? [sudoku-board]
	(if (= (valid-blocks? sudoku-board) (valid-cols? sudoku-board) (valid-rows? sudoku-board))
	true false)
	)

(defn solve [sudoku-board]
	(if (and (filled? sudoku-board) (valid-solution? sudoku-board))
		sudoku-board
		(if (filled? sudoku-board) 
			nil
			(do (let [empty-spot (find-empty-point sudoku-board) 
				possible-vals (valid-values-for sudoku-board empty-spot)]
				(for [elem possible-vals solution (solve (assoc-in sudoku-board empty-spot elem))] 
					solution
				))
			))
		)
	)

(defn -main
  [& args]
  (println "everything done and working"))
