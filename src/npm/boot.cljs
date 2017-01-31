(ns npm.boot
  (:require [clojure.string :as str]
            [cljs.analyzer.api :as analy]
            [npm.mvn :as mvn]))

(def PACKAGE (js->clj (js/require (str js/process.env.PWD "/package.json"))))
(declare *classpath*)

(defn file-seq [root]
  (tree-seq #(and (js/fs.existsSync %)
                  (-> % js/fs.statSync .isDirectory))
            (fn [dir]
              (->> dir js/fs.readdirSync
                   js->clj
                   (map #(str dir "/" %))))
            root))

(defn ^:export lumo [& opts]
  (let [process        (atom nil)
        source-paths   (get-in PACKAGE ["cljs" "source-paths"])
        source-pattern (re-pattern (str \( (str/join "|" source-paths) \) "/?"))
        cache-path     (or (when (some #{"-K"} opts)
                             ".lumo_cache")
                           (let [idx (inc (.indexOf opts "-k"))]
                             (when (pos? idx)
                               (nth opts idx))))
        opts           (clj->js (concat ["-c"
                                         (str/join ":" (concat source-paths *classpath*))]
                                        opts))
        invalid-cache! (fn []
                         (when cache-path
                           (let [files  (->> source-paths
                                             (mapcat file-seq)
                                             (remove #(re-find #"\.#" %))
                                             (filter #(re-find #"\.clj[sc]$" %)))
                                 caches (->> files
                                             (map #(str/replace % source-pattern ""))
                                             (map #(str/replace % #"\.clj[sc]" ".js"))
                                             (map #(str cache-path \/ (munge %))))]
                             (dorun
                              (map (fn [file cache]
                                     (when (and (js/fs.existsSync file)
                                                (js/fs.existsSync cache)
                                                (> (.-mtime (js/fs.statSync file))
                                                   (.-mtime (js/fs.statSync cache))))
                                       (println "invalid cache..." file)
                                       (js/fs.unlinkSync cache)
                                       (js/fs.unlinkSync (str/replace cache #"js$" "cache.json"))))
                                   files
                                   caches)))))]
    (fn [_]
      (fn []
        (invalid-cache!)
        (some-> @process .kill)
        (reset! process
                (js/child_process.spawn "lumo" opts #js {:stdio "inherit"}))))))

(defn ^:export watch [t file]
  (fn [next-handler]
    (fn []
      (next-handler)
      (js/fs.watch file #js {:recursive (boolean (#{"-d" "--dir"} t))}
                   (fn [_ file]
                     (when-not (re-find #"\.#" file)
                       (println "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ watch task ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
                       (next-handler)))))))

(defn run-task [task]
  ((task identity)))

(def NS-EXPORTS
  (reduce #(aget %1 (munge %2))
          js/global
          (-> (ns-name *ns*)
              (str/split #"\."))))

(defn argv->task [argv]
  (let [task-specs (->> argv
                        (partition-by #(.hasOwnProperty NS-EXPORTS (munge %)))
                        (partition 2))]
    (apply comp (for [[[t] args] task-specs]
                  (do
                    (println "compile" (pr-str (cons t args)) "...")
                    (apply (aget NS-EXPORTS (munge t)) args))))))

(defn -main [& argv]
  (mvn/async-map mvn/download-dep (map mvn/parse-dep (get-in PACKAGE ["cljs" "dependencies"]))
                 (fn [paths]
                   (set! *classpath* paths)
                   (run-task (argv->task argv)))))


