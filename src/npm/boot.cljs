(ns npm.boot
  (:require [clojure.string :as str]
            [cljs.analyzer.api :as analy]
            [npm.mvn :as mvn]))

(def Telnet (js/require "telnet-client"))

(def PACKAGE
  (if (js/fs.existsSync (str js/process.env.PWD "/package.json"))
    (js->clj (js/require (str js/process.env.PWD "/package.json")))
    {"cljs" {"source-paths" ["src"]}}))

(declare reload)

(let [processers (atom nil)]
  (defn spawnx [key cmd opts conf]
    (let [p    (atom nil)
          opts (clj->js (vec opts))
          conf (clj->js conf)]
      (some-> @p (get key) .kill)
      (swap! processers assoc key
             (js/child_process.spawn cmd opts conf)))))

(defn file-seq [root]
  (tree-seq #(and (js/fs.existsSync %)
                  (-> % js/fs.statSync .isDirectory))
            (fn [dir]
              (->> dir js/fs.readdirSync
                   js->clj
                   (map #(str dir "/" %))))
            root))

(defn ^:export lumo [& opts]
  (let [source-paths   (get-in PACKAGE ["cljs" "source-paths"])
        source-pattern (re-pattern (str \( (str/join "|" source-paths) \) "/?"))
        cache-path     (or (when (some #{"-K"} opts)
                             ".lumo_cache")
                           (let [idx (inc (.indexOf opts "-k"))]
                             (when (pos? idx)
                               (nth opts idx))))
        invalid-cache! (fn []
                         (when cache-path
                           (let [files (->> source-paths
                                            (mapcat file-seq)
                                            (remove #(re-find #"\.#" %))
                                            (filter #(re-find #"\.clj[sc]$" %)))]
                             (doall
                              (keep (fn [file]
                                      (let [resource (str/replace file source-pattern "")
                                            cache    (-> resource
                                                         (str/replace #"\.clj[sc]" ".js")
                                                         munge
                                                         (->> (str cache-path \/)))]
                                        (when (and (js/fs.existsSync file)
                                                   (js/fs.existsSync cache)
                                                   (> (.-mtime (js/fs.statSync file))
                                                      (.-mtime (js/fs.statSync cache))))
                                          (println "invalid cache..." file)
                                          (js/fs.unlinkSync cache)
                                          (js/fs.unlinkSync (str/replace cache #"js$" "cache.json"))
                                          resource)))
                                    files)))))]
    (fn [next-handler]
      (fn [fileset]
        (let [repl (let [idx (inc (.indexOf opts "-n"))]
                     (when (pos? idx)
                       (let [[host port] (str/split (nth opts idx) #":")]
                         {:host host :port (js/parseInt port)})))
              opts (clj->js (concat
                             (when (and (not repl) (some #{reload} (:tasks fileset)))
                               ["-n" "127.0.0.1:5555"])
                             ["-c" (str/join ":" (concat source-paths (:classpath fileset)))]
                             opts))]
          (invalid-cache!)
          (spawnx ::lumo "lumo" opts #js {:stdio "inherit"})
          (next-handler (assoc fileset
                               :lumo/invalid-cache! invalid-cache!
                               :lumo/repl (merge {:host "127.0.0.1" :port 5555} repl))))))))

(defn ^:export watch [t file]
  (fn [next-handler]
    (fn [fileset]
      (next-handler fileset)
      (js/fs.watch file #js {:recursive (boolean (#{"-d" "--dir"} t))}
                   (fn [_ file]
                     (when-not (re-find #"\.#" file)
                       (println "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ watch task ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
                       (next-handler (assoc fileset :reload file))))))))

(defn file->ns [file]
  (-> file
      (str/replace #"\.clj[sc]?" "")
      (str/replace #"/" ".")
      (str/replace #"_" "-")
      symbol))

(defn ^:export reload []
  (fn [next-handler]
    (let [conn (atom nil)]
      (fn reload* [{:keys [lumo/repl] :as fileset}]
        (if-let [conn @conn]
          (when-let [file (:reload fileset)]
            (when-let [resources (seq ((:lumo/invalid-cache! fileset)))]
              (.send conn
                     (str (pr-str `(require '~@(map file->ns resources) :reload))
                          \newline)
                     (fn [err res]))))
          (let [c (Telnet.)]
            (doto c
              (.on "ready" (fn [prompt]
                             (reset! conn c)
                             (reload* fileset)))
              (.on "data" #(when-let [s (not-empty (str %))]
                             (when-not (str/starts-with? (str/trim s) "cljs.user=>")
                               (print s))))
              (.on "error" (fn [prompt]
                             (js/setTimeout #(reload* fileset) 500)))
              (.connect #js {:host        (:host repl)
                             :port        (:port repl)
                             :shellPrompt "cljs.user=> "
                             :timeout     1500}))))))))

(defn ^:export exec [cmd & opts]
  (fn [next-handler]
    (fn [fileset]
      (spawnx (str "exec-" cmd) cmd (clj->js opts) #js {:stdio "inherit"}))))

(def ^:export node (partial exec "node"))

(defn run-task [task fileset]
  ((task (constantly :nothing)) fileset))

(def NS-EXPORTS
  (reduce #(aget %1 (munge %2))
          js/global
          (-> (ns-name *ns*)
              (str/split #"\."))))

(defn argv->task [argv]
  (let [task-specs (->> argv
                        (partition-by #(.hasOwnProperty NS-EXPORTS (munge %)))
                        (partition-all 2))]
    (apply comp (for [[[t] args] task-specs]
                  (do
                    (println "compile" (pr-str (cons (symbol t) args)) "...")
                    (apply (aget NS-EXPORTS (munge t)) args))))))

(defn -main [& argv]
  (mvn/async-map mvn/download-dep (map mvn/parse-dep (get-in PACKAGE ["cljs" "dependencies"]))
                 (fn [paths]
                   (run-task (argv->task argv)
                             (assoc PACKAGE :classpath paths
                                    :tasks (keep #(aget NS-EXPORTS (munge %)) argv))))))


