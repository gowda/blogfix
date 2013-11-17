(ns blogfix.fix
  (:refer-clojure :exclude [replace])
  (:use [clojure.string :only [split trim replace]])
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

(defn get-article-name [s]
  (if (re-find #"^http" s)
    (->> (re-find #"^http[s]?://[^/]+/wiki/(.+)" s) second)
    s))

(defn create-url [s]
  (str "https://en.wikipedia.org/w/index.php?title=" (get-article-name s)
       "&action=render"))

(defn fetch-article
  [article]
  (client/get (create-url article) {:headers {"User-Agent" "blogfix-clj"}}))

(defn domain-name [url]
  (->> (re-find #"http[s]?://([^/]+)/.*" url) second))

(defn link-attr [node]
  (or (and (html/attr-values node :href) :href)
      (and (html/attr-values node :src) :src)))

(defn link-dest [node]
  (->> (or (html/attr-values node :href)
           (html/attr-values node :src))
       first))

(def media-base-url "https://upload.wikimedia.org/wikipedia/commons")

(def stable-magnify-url (str media-base-url "/6/6b/Magnify-clip.png"))
(defn stabilize-magnify-url [node]
  ((html/set-attr :src stable-magnify-url) node))

(def stable-videoplayer-url (str media-base-url
                                 "/9/96/Crystal_Project_Player_play.png"))
(defn stabilize-videoplayer-url [node]
  ((html/set-attr :src stable-videoplayer-url) node))

(def style-fixes {:.center "text-align: center;"
                  :.thumb.tright (str "text-align: center;"
                                      "border:1px solid #ccc;"
                                      "margin:2px;"
                                      "float:right;"
                                      "clear:right;"
                                      "margin:0.5em 0 0.8em 1.4em;")
                  :.thumb.tleft (str "text-align: center;"
                                     "border: 1px solid #ccc;"
                                     "margin: 2px;"
                                     "float: left;"
                                     "clear: left;"
                                     "margin: 0.5em 1.4em 0.8em 0;")
                  :.thumbinner (str "padding: 3px !important;"
                                    "border: 1px solid rgb(204, 204, 204);"
                                    "text-align: center;"
                                    "overflow: hidden;"
                                    "font-size: 94%;"
                                    "background-color: white;"
                                    "display: block;"
                                    "margin-left: auto;"
                                    "margin-right: auto;")
                  :.thumbimage "border:1px solid #ccc;"
                  :.thumbcaption (str "border: none;"
                                      "text-align: left;"
                                      "line-height: 1.4em;"
                                      "padding: 3px !important;"
                                      "font-size: 94%;")
                  :.magnify (str "float: right;"
                                 "border: none !important;"
                                 "background: none !important;")
                  :.gallery (str "margin: 2px;"
                                 "padding: 2px;"
                                 "display: block;")
                  :.gallerybox (str "vertical-align: top;"
                                    "display: -moz-inline-box;"
                                    "display: inline-block;")
                  :.gallerytext (str "overflow: hidden;"
                                     "font-size: 94%;"
                                     "padding: 2px 4px;"
                                     "word-wrap: break-word;")
                  :.thumb (str "text-align: center;"
                               "border: 1px solid #ccc;"
                               "margin: 2px;")
                  :.sc2 "color: #009900;"
                  :.kw2 "color: #000000; font-weight: bold;"
                  :.kw3 "color: #000066;"
                  :.sy0 "color: #66cc66;"
                  :.st0 "color: #ff0000;"})

(defn classes-to-str-seq [cls]
  (->> (split (name cls) #"\.") (remove #(= 0 (count %)))))

(defn create-class-matcher [node]
  (fn [[cls style]]
    (let [classes (->> (classes-to-str-seq cls) sort)]
      (= (->> (html/attr-values node :class) sort) classes))))

(defn remove-class [cls]
  (let [classes (classes-to-str-seq cls)]
    (apply html/remove-class classes)))

(defn fix-style [node]
  (let [[[cls style] & _] (filter (create-class-matcher node) style-fixes)]
    ((html/do->
      (remove-class cls)
      (html/set-attr :style style)) node)))

(defn do-fix-link [domain s]
  (cond
   (re-find #"^/wiki/" s) (str "https://" domain s)
   (re-find #"^//" s) (str "https:" s)
   (re-find #"/wiki/File" s) (replace s domain "commons.wikimedia.org")
   :else s))

(defn create-fix-link [url]
  (let [domain (domain-name url)]
    (fn [node]
      ((html/set-attr (link-attr node)
                      (do-fix-link domain (link-dest node))) node))))

(defn fix-srcset [node]
  (let [url-size-pairs (-> node :attrs :srcset (split #","))
        fixed-url-size-pairs (map #(str "https:" (trim %))
                                  url-size-pairs)]
    ((html/set-attr :srcset (->> (interpose "," fixed-url-size-pairs)
                                 (apply str))) node)))

(defn transform [article]
  (let [body (->> (fetch-article article) :body)
        fix-link (create-fix-link (create-url article))]
    (html/at (html/html-snippet body)
             [#{(html/attr? :href) (html/attr? :src)}] fix-link
             [(html/attr? :srcset)] fix-srcset
             [[:.external #{[:.text :.free :.autonumber]}]]
             (html/do->
              (html/remove-class "external"
                                 "text"
                                 "free"
                                 "autonumber")
              (html/remove-attr :rel))
             [(->> (keys style-fixes) (into #{}))] fix-style
             [[:img
               (html/attr-ends :src "skins/common/images/magnify-clip.png")]]
             stabilize-magnify-url
             [[:img
               (html/attr-ends :src "extensions/OggHandler/play.png")]]
             stabilize-videoplayer-url)))

(defn fix-article [article]
  (->> (transform article) html/emit* (apply str)))
