(defproject katas "0.1.0-SNAPSHOT"
  :description "A project containing code katas"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.trace "0.7.11"]
                 [djblue/portal "0.34.2"]]
  :plugins [[lein-cljfmt "0.6.6"]
            [lein-ancient "0.6.15"]]
  :repl-options {:init-ns katas.core})
