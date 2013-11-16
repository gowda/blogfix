(ns blogfix.core
  (:use [blogfix.fix :only [fix-article]]))

(defn -main [& args]
  (if (< (count args) 2)
    (println "Usage: <program-invocation> wikipedia-article local-file-path")
    (let [[article file-path] args]
      (println "Wikipedia-article:" article)
      (println "Local file path:" file-path)
      (spit file-path (fix-article article)))))
