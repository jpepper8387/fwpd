(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
        (clojure.string/split string #"\r\n")))

(def all-records (parse (slurp filename)))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
        (reduce (fn [row-map [vamp-key value]]
          (assoc row-map vamp-key (convert vamp-key value)))
          {}
          (map vector vamp-keys unmapped-row)))
        rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn list-names
  [records]
  (seq (map :name records)))



(def validations {:name identity
                :glitter-index str->int})

(defn validate
  [record]
  (and (contains? record :name) (contains? record :glitter-index)))

(defn append
  [curr record]
  (if (validate record)
    (conj curr [(:name record) (:glitter-index record)])
    curr))

(defn conv-to-csv-string
  [records]
  (clojure.string/join "\n" (map #(clojure.string/join ","
  [(first %) (second %)]) records)))

  (defn two-comp
    [f g]
    (fn [& args]
      (f (apply g args))))
