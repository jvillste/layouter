(defproject layouter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.stuartsierra/frequencies "0.1.0"]
                 [dev.weavejester/medley "1.7.0"]
                 [flow-gl "4"]
                 [metosin/jsonista "0.3.9"]
                 [fi.evident.raudikko/raudikko "0.1.4"]
                 [camel-snake-kebab "0.4.3"]
                 [clj-http "3.13.1"]
                 [org.languagetool/language-en "6.6"]
                 [com.google.guava/guava "21.0"]]
  :repl-options {:init-ns layouter.core}

  :jvm-opts ["-Xmx2g" "-Djdk.attach.allowAttachSelf" ;; for clj-async-profiler
             "-XX:-OmitStackTraceInFastThrow"])
