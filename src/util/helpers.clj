(ns util.helpers)

(defn get-lines
  "Lazy line reader that closes when it reaches the end."
  [file]
  (let [rdr (clojure.java.io/reader file)
        f (fn line-stream [lines]
            (if-let [next-line (first lines)]
              (cons next-line (lazy-seq (line-stream (next lines))))
              (do
                (.close rdr)
                nil)))]
    (f (line-seq rdr))))
