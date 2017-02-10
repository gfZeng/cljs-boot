(ns npm.mvn
  (:require-macros [npm.macros :refer [defn-async let-async]])
  (:require [clojure.string :as str]))

(defn parse-dep [[dep version]]
  (let [[group artifact] (if (str/includes? dep "/")
                           (str/split dep #"/")
                           [dep dep])
        dep-dir          (-> group (str/replace #"\." "/")
                             (str "/" artifact "/" version))]
    {:group    group
     :artifact artifact
     :dep-dir  dep-dir
     :version  version}))

(defn hiccup->xml [x]
  (cond
    (vector? x)
      (let [tag              (first x)
            [attrs children] (if (map? (second x))
                               [(second x) (drop 2 x)]
                               [nil (rest x)])
            attrs-str        (if-not attrs
                               ""
                               (str " " (str/join
                                         " "
                                         (for [[k v] attrs]
                                           (str k "=" \" v \")))))]
        (str "<" (name tag) attrs-str ">"
             (apply str (map hiccup->xml children))
             "</" (name tag) ">"))

    (seq? x)
      (do
        (apply str (map hiccup->xml x)))

    :else (str x)))

(defn emit-pom-xml [deps]
  (hiccup->xml
   [:project
    [:repositories
     [:repository
      [:id "clojars.org"]
      [:url "http://clojars.org/repo"]]]
    [:dependencies
     (for [d deps]
       [:dependency
        [:groupId (:group d)]
        [:artifactId (:artifact d)]
        [:version (:version d)]])]]))

(defn install-dep [dep]
  (js/child_process.spawn "mvn" #js [(str '-DrepoUrl= "https://clojars.org/repo/")
                                     (str '-DgroupId= (:group dep))
                                     (str '-DartifactId= (:artifact dep))
                                     (str '-Dversion= (:version dep))
                                     "dependency:get"]
                          #js {:stdio "inherit"}))

(defn ensure-dir! [dir]
  ((fn [dir subs]
     (if (js/fs.existsSync dir)
       (reduce (fn [dir sub]
                 (doto (str dir \/ sub)
                   (js/fs.mkdirSync)))
               dir
               subs)
       (when-let [[_ dir sub] (re-find #"(.*)/([^/]+$)" dir)]
         (recur dir (cons sub subs)))))
   (str/replace dir #"^(?!/)" "./") ()))

(defn-async http-copy [url path]
  (if (js/fs.existsSync path)
    (&async path)
    (let [file-stream (js/fs.createWriteStream path)]
      (println "downloading..." url)
      (js/https.get url #(.pipe % file-stream))
      (.on file-stream "finish"
           (fn []
             (.close file-stream)
             (&async path))))))

(defn download-dep [{:keys [dep-dir artifact version] :as dep} done]
  (ensure-dir! (str js/process.env.HOME "/.m2/repository/" dep-dir))
  (let-async [dep-path  (str dep-dir \/ artifact "-" version ".jar")

              file-path (str js/process.env.HOME "/.m2/repository/" dep-path)
              file-url  (str "https://clojars.org/repo/" dep-path)

              sha1-path (str file-path ".sha1")
              sha1-url  (str file-url ".sha1")

              sha1-path ^:async (http-copy sha1-url sha1-path)
              file-path ^:async (http-copy file-url file-path)

              shasum    (-> (str "shasum " file-path)
                            (str " | awk '{print $1}'")
                            js/child_process.execSync
                            str
                            str/trim)]
    (if (= shasum (str/trim (str (js/fs.readFileSync sha1-path))))
      (done file-path)
      (js/fs.unlink file-path (fn [e]
                                (download-dep dep done))))))

(defn async-map [f xs done]
  ((fn step [[x & xs] acc]
     (if-not x
       (done acc)
       (f x (fn [ret]
              (step xs (conj acc ret))))))
   xs []))

(defn async-pmap [f xs done]
  (let [cnt  (atom (count xs))
        rets (atom (vec (repeat @cnt nil)))]
    (reduce-kv (fn [_ i x]
                 (f x (fn [ret]
                        (swap! rets assoc i ret)
                        (when (zero? (swap! cnt dec))
                          (done @rets)))))
               nil
               (vec xs))))


(comment
  (async-pmap (fn [x done]
                (done (inc x)))
              (range 10)
              (fn [xs] (println "Done:" xs)))
  )
