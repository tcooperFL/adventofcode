(ns util.trace)

(def level (atom 0))
(defn- indent [i] (apply str (repeat i "  ")))

(defn enter
  "Simple indenting trace function. Suggested usage: {:pre [(enter <fnname> <arg>...)]}"
  [name & args]
  (do
    (println
      (apply str
             `(~(indent (dec (swap! level inc)))
                "->" ~name " " ~@args)))
    @level))

(defn leave
  "Simple indenting trace exit function. Suggested usage: {:post [(leave <fnname> %)]}"
  [name & args]
  (do
    (println
      (apply str
             `(~(indent (swap! level dec))
                "<-" ~name " " ~@args)))
    @level))

