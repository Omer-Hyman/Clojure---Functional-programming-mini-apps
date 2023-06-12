(ns assignment.core 
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.string :as str]))

;; Map of ascii characters and their morse code equivalents. 
(def morseCodeMap {:a ".-" :b "-..." :c "-.-." :d "-.." :e "." :f "..-." :g "--." :h "...." :i ".." :j ".---"
                   :k "-.-" :l ".-.." :m "--" :n "-." :o "---" :p ".--." :q "--.-" :r ".-." :s "..." :t "-"
                   :u "..-" :v "...-" :w ".--" :x "-..-" :y "-.--" :z "--.." :1 ".----" :2 "..---" :3 "...--"
                   :4 "....-" :5 "....." :6 "-...." :7 "--..." :8 "---.." :9 "----." :0 "-----"})

(def months {:1 "January" :2 "February" :3 "March" :4 "April" :5 "May" :6 "June" :7 "July" :8 "August" :9 "September" :10 "October" :11 "November" :12 "December"})

;; Return keyword (without colon) of the morseCodeMap value passed in.
(defn getAsciiFromMap [a]
  (try 
    (subs (str (nth (keys morseCodeMap) (.indexOf (vals morseCodeMap) a))) 1)
    (catch Exception ex
      (println (str "Exception: " (.toString ex)))
      (println "Character" a "not found")))
)

;; Regex pattern taken from https://stackoverflow.com/questions/17197887/java-regexp-match-morse-code and modified slightly.
(def validateMorseRegex #"[.-]{1,5}(?>   [.-]{1,5})*(?>       [.-]{1,5}(?>   [.-]{1,5})*)*")
(s/def ::morseInput #(re-matches validateMorseRegex %))

;; Question 1 - Morse to Ascii conversion.
(defn morseToAscii []
  (println "What morse code would you like to convert to ascii text?")
  (let [input (read-line)
        results (if (s/valid? ::morseInput input)
                  (loop [characters (str/split input #"   ")
                         result []]
                    (if (empty? characters)
                      result
                      (if (= (first characters) "")
                        (recur (rest (rest characters)) (conj result " " (getAsciiFromMap (subs (first (rest characters)) 1))))
                        (recur (rest characters) (conj result (getAsciiFromMap (first characters))))))) 
                  "Invalid morse input!")]
    (println "Converting:" input)
    (apply str results)))

;; (morseToAscii)
;; ....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -.. = hello world

(def validateAsciiRegex #"([0-9]*[a-z]*[ ]*)*")
(s/def ::asciiInput #(re-matches validateAsciiRegex %))

;; Question 1 - Ascii to Morse conversion.
(defn asciiToMorse []
  (println "What ASCII text would you like to convert to morse code? (Only lower case letters and numbers are valid!)")
  (let [input (read-line)
        results (if (s/valid? ::asciiInput input) 
                  (loop [characters (str/split input #"")
                         result []]
                    (if (empty? characters)
                      result
                      (if (= (first characters) " ")
                        (recur (rest characters) (conj result "    "))
                        (recur (rest characters) (if (= (count characters) 1)
                                                   (conj result (get morseCodeMap (keyword (first characters))))
                                                   (conj result (get morseCodeMap (keyword (first characters))) "   "))))))
                    "Invalid ascii string!")]
    (println "Input =" input)
    (apply str results)))

;; (asciiToMorse)

(def keyz [:year :day :1 :2 :3 :4 :5 :6 :7 :8 :9 :10 :11 :12])

;; Creates a map out of the keyz vector above and the words vector passed in to the function.
(defn returnMap [words]
  (zipmap keyz words))

;; Old function:
;; I later changed this to calculateRowAverage

;; (defn averageOfOneRow [data]
;;   (let [result (loop
;;                 [sumOfRow 0
;;                  row (vals (dissoc data :day :year))
;;                  legitValuesCount 0
;;                  overallCount 0]
;;                  (if (empty? row)
;;                    (/ sumOfRow legitValuesCount)
;;                    (recur
;;                     (if (not (= (first row) "-999")) (+ sumOfRow (Float/parseFloat (first row))) sumOfRow)
;;                     (rest row)
;;                     (if (not (= (first row) "-999")) (+ legitValuesCount 1) legitValuesCount)
;;                     (+ overallCount 1))))]
;;     (float result))
;;   )

(defn calculateRowAverage [row]
  (let [updatedRow (filter #(not (= -999.0 %)) (for [data (dissoc row :day :year)]
                                                 (Float/parseFloat (val data))))]
    (Float/parseFloat (format "%.3f" (/ (reduce + updatedRow) (count updatedRow))))
    )
  )

(defn returnVectorOfMaps []
  (let [src "weatherData.dat"
        words (str/split (slurp src) #"\s+")
        results (loop [i 15
                       result []]
                  (if (and (integer? (/ (- i 1) 14)) (<= i (count words)))
                    (recur (+ i 14) (conj result (returnMap (subvec words (- i 14) i))))
                    result))
        ]
    (loop [rowsWithAverages []
           rows results]
      (if (empty? rows)
        rowsWithAverages
        (recur
         (conj rowsWithAverages (assoc (first rows) :average (calculateRowAverage (first rows))))
         (rest rows))))))

;; (returnVectorOfMaps)

(defn warmestDayOfMonth [month]
  (let [warmestDay (last (sort-by month (returnVectorOfMaps)))]
    (println "Day" (get warmestDay :day) "of" (get warmestDay :year) "was the warmest January day with a temperature of" (/ (Float/parseFloat (get warmestDay month)) 10))))

;; (warmestDayOfMonth :1)

(defn sumOfAveragesYearly [data]
  (let [myMap (apply merge-with + (for [x data] (select-keys x [:average])))]
    (get myMap :average)))

(defn yearlyAverages []
  (let [result (loop [rows (returnVectorOfMaps)
                      sums []]
                 (if (empty? rows)
                   sums
                   (recur (drop 31 rows) (conj sums {:average (/ (sumOfAveragesYearly (take 31 rows)) 31) :year (:year (first rows))}))))]
    result))

(defn warmestYear []
  (last (sort-by :average (yearlyAverages))))

(defn coldestYear []
  (first (sort-by :average (yearlyAverages))))

;; (println "Warmest year:" (warmestYear))
;; (println "Coldest year:" (coldestYear))

(defn allValuesForSingleMonth [month]
  (sort (filter #(not (= -999.0 (first %))) (for [row (returnVectorOfMaps)]
                                              [(Float/parseFloat (get row (keyword month))) (get row (keyword :year)) (get row (keyword :day))]))))

(defn questionThree []
  (println "What month would you like to find the average for? Input the number corresponding to each month.")
  (println "E.g.\n\t1) Jan\n\t2) Feb\n\t3) Mar\n\t4) Apr\n\t5) May\n\t6) June\n\t7) Jul\n\t8) Aug\n\t9) Sep\n\t10) Oct\n\t11) Nov\n\t12) Dec")
  (let [input (read-line)]
    (try
      (let [data (for [x (returnVectorOfMaps)] (Float/parseFloat (get x (keyword input))))
            average (format "%.2f" (float (/ (apply + data) (* (count data) 10))))
            list (allValuesForSingleMonth (keyword input))]
        (println "For the month of" (str (get months (keyword input)) ":"))
        (println "Average temperature:" average)
        (println "Coldest temperature:" (str (last (first list)) "/" input "/" (second (first list))) "=" (/ (first (first list)) 10))
        (println "Warmest temperature:" (str (last (last list)) "/" input "/" (second (last list))) "=" (/ (first (last list)) 10)))
      (catch Exception ex
        (println ex "Please input a valid month!")))))

;; (questionThree)
