(defproject adventofcode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.490"]
                 [incanter "1.9.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev     {:global-vars {*assert* true *print-length* 20 *print-level* 6}}})
