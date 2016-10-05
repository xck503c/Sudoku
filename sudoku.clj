(defn near? [i j]
	"是否在同一行，同一列，同一九宫格内"
	(or 
		(= (quot i 9) (quot j 9))    ;同一行
		(= (rem i 9) (rem j 9))  ;同一列
		(and 
			(=
				(quot i 27)
				(quot j 27))
			(= 
				(quot (rem i 9) 3)
				(quot (rem j 9) 3)))))

(defn guess [numbers index value]
	(map-indexed #(cond 
		(= index %1) #{value}
		(near? index %1) (disj %2 value)
                      	:else %2)
               numbers))
 
(defn bfs [numbers index]
	"先判断索引是否小于81和集合是否只有一个，是就递归，否则遍历sudoku中下标为index的set，然后一个个递归"
	(if  (and 
		(= (count (nth numbers index)) 1)
		(< index 81))
		(bfs numbers (inc index))
		(some 
			(fn [value]
				(let [table (guess numbers index value)]
					(or 
						(and (every? #(= (count %) 1) table) table)
						(and (not-any? empty? table) (< index 81) (bfs table (inc index))))))
			(nth numbers index))))

(def read-data
	"构建一个1~81的惰性序列，然后将输入的数据放入空vector中"
	(reduce (fn [numbers number] (conj numbers (read)))
		[]
		(range 1 81)))

(defn init
	"将传进来的value初始化为set(1-9)，
	遍历numbers和value比较是否near，将value中不可能的数字排除，
	返回最后可能的set"
	[numbers n-index n-value]
	(let [n-value (set (range 1 10))]
		(loop [x 0 n-value n-value]
			(if (= (count numbers) x)
				n-value
				(cond 
					(and (not (zero? (nth numbers x)))
						(near? x n-index)) (recur (inc x) (disj n-value (nth numbers x)))
					:else (recur (inc x) n-value))))))

(defn map-init [numbers]
	"遍历numbers将0初始化为set
	将不是0的数直接替换为set"
	(map-indexed #(cond (zero? %2) (init numbers %1 %2)
				:else #{%2}) 
		numbers))


(def result (time (bfs  (map-init read-data) 0)))

(println result)


;The test data --- 8 0 0 0 0 0 0 0 0 0 0 3 6 0 0 0 0 0 0 7 0 0 9 0 2 0 0 0 5 0 0 0 7 0 0 0 0 0 0 0 4 5 7 0 0 0 0 0 1 0 0 0 3 0 0 0 1 0 0 0 0 6 8 0 0 8 5 0 0 0 1 0 0 9 0 0 0 0 4 0 0
