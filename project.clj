(defproject minesweeper "0.1.0-SNAPSHOT"
  :description "A game of minesweeper (exercise in clojure)"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cljfmt "0.5.1"]]
  :main ^:skip-aot minesweeper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
