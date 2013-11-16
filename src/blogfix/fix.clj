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

(defn create-article-link-externalizer [url]
  (let [domain (domain-name url)]
    (fn [node]
      (let [href (->> (html/attr-values node :href) first)]
        ((html/set-attr :href (str "https://" domain href)) node)))))

(defn create-fd-link-externalizer [url]
  (let [domain (domain-name url)]
    (fn [node]
      (let [href (->> (html/attr-values node :href) first)]
        ((html/set-attr :href (replace href domain "commons.wikimedia.org"))
         node)))))

(defn externalize-srcset-url [node]
  (let [url-size-pairs (-> node :attrs :srcset (split #","))
        externalize-url-size-pairs (map #(str "https:" (trim %))
                                        url-size-pairs)]
    ((html/set-attr :srcset (->> (interpose "," externalize-url-size-pairs)
                                 (apply str))) node)))

(defn link-attr [node]
  (or (and (html/attr-values node :href) :href)
      (and (html/attr-values node :src) :src)))

(defn link-dest [node]
  (->> (or (html/attr-values node :href)
           (html/attr-values node :src))
       first))

(defn externalize-url [node]
  ((html/set-attr (link-attr node) (str "https:" (link-dest node))) node))

(defn add-center-style [node]
  ((html/do->
    (html/set-attr :style "text-align: center;")
    (html/remove-class "center")) node))

(defn add-thumbnail-right-style [node]
  ((html/do->
    (html/set-attr :style (str "text-align: center;"
                               "border:1px solid #ccc;"
                               "margin:2px;"
                               "float:right;"
                               "clear:right;"
                               "margin:0.5em 0 0.8em 1.4em;"))
    (html/remove-class "thumb" "tright")) node))

(defn add-thumbnail-left-style [node]
  ((html/do->
    (html/set-attr :style (str "text-align: center;"
                               "border: 1px solid #ccc;"
                               "margin: 2px;"
                               "float: left;"
                               "clear: left;"
                               "margin: 0.5em 1.4em 0.8em 0;"))
    (html/remove-class "thumb" "tleft")) node))

(defn add-thumbinner-style [node]
  ((html/do->
    (html/set-attr :style (str "padding: 3px !important;"
                               "border: 1px solid rgb(204, 204, 204);"
                               "text-align: center;"
                               "overflow: hidden;"
                               "font-size: 94%;"
                               "background-color: white;"
                               "display: block;"
                               "margin-left: auto;"
                               "margin-right: auto;"))
    (html/remove-class "thumbinner")) node))

(defn add-thumbimage-style [node]
  ((html/do->
    (html/set-attr :style "border:1px solid #ccc;")
    (html/remove-class "thumbimage")) node))

(defn add-thumbcaption-style [node]
  ((html/do->
    (html/set-attr :style (str "border: none;"
                               "text-align: left;"
                               "line-height: 1.4em;"
                               "padding: 3px !important;"
                               "font-size: 94%;"))
    (html/remove-class "thumbcaption")) node))

(defn add-magnify-style [node]
  ((html/do->
    (html/set-attr :style (str "float: right;"
                               "border: none !important;"
                               "background: none !important;"))
    (html/remove-class "magnify")) node))

(defn add-gallery-style [node]
  ((html/do->
    (html/set-attr :style (str "margin: 2px;"
                               "padding: 2px;"
                               "display: block;"))
    (html/remove-class "gallery")) node))

(defn add-gallerybox-style [node]
  ((html/do->
    (html/set-attr :style (str "vertical-align: top;"
                               "display: -moz-inline-box;"
                               "display: inline-block;"))
    (html/remove-class "gallerybox")) node))

(defn add-gallerytext-style [node]
  ((html/do->
    (html/set-attr :style (str "overflow: hidden;"
                               "font-size: 94%;"
                               "padding: 2px 4px;"
                               "word-wrap: break-word;"))
    (html/remove-class "gallerytext")) node))

(defn add-thumb-style [node]
  ((html/do->
    (html/set-attr :style (str "text-align: center;"
                               "border: 1px solid #ccc;"
                               "margin: 2px;"))
    (html/remove-class "thumb")) node))

(defn replace-class-by-style [node cls style]
  ((html/do->
    (html/set-attr :style style)
    (html/remove-class cls))
   node))

(defn add-sc2-style [node]
  (replace-class-by-style node "sc2" "color: #009900;"))

(defn add-kw2-style [node]
  (replace-class-by-style node "kw2" "color: #000000; font-weight: bold;"))

(defn add-kw3-style [node]
  (replace-class-by-style node "kw3" "color: #000066;"))

(defn add-sy0-style [node]
  (replace-class-by-style node "sy0" "color: #66cc66;"))

(defn add-st0-style [node]
  (replace-class-by-style node "st0" "color: #ff0000;"))

(def media-base-url "https://upload.wikimedia.org/wikipedia/commons")

(def stable-magnify-url (str media-base-url "/6/6b/Magnify-clip.png"))
(defn stabilize-magnify-url [node]
  ((html/set-attr :src stable-magnify-url) node))

(def stable-videoplayer-url (str media-base-url
                                 "/9/96/Crystal_Project_Player_play.png"))
(defn stabilize-videoplayer-url [node]
  ((html/set-attr :src stable-videoplayer-url) node))

(defn transform [article]
  (let [body (->> (fetch-article article) :body)
        externalize-article-link (create-article-link-externalizer
                                  (create-url article))
        externalize-fd-link (create-fd-link-externalizer
                             (create-url article))]
    (html/at (html/html-snippet body)
             [[:a (html/attr-starts :href "/wiki")]] externalize-article-link
             [[:.external #{[:.text :.free :.autonumber]}
               (html/attr-has :rel "nofollow")]]
             (html/do->
              (html/remove-class "external"
                                 "text"
                                 "free"
                                 "autonumber")
              (html/remove-attr :rel))
             [[:.external #{[:.text :.free :.autonumber]}]]
             (html/do->
              (html/remove-class "external"
                                 "text"
                                 "free"
                                 "autonumber"))
             [:.center] add-center-style
             [[:.thumb :.tright]] add-thumbnail-right-style
             [[:.thumb :.tleft]] add-thumbnail-left-style
             [:.thumbinner] add-thumbinner-style
             [:.thumbimage] add-thumbimage-style
             [:.thumbcaption] add-thumbcaption-style
             [:.magnify] add-magnify-style
             [:.gallery] add-gallery-style
             [:.gallerybox] add-gallerybox-style
             [:.gallerytext] add-gallerytext-style
             [:.thumb] add-thumb-style
             [:.sc2] add-sc2-style
             [:.kw2] add-kw2-style
             [:.kw3] add-kw3-style
             [:.sy0] add-sy0-style
             [:.st0] add-st0-style
             [[:img
               (html/attr-ends :src "skins/common/images/magnify-clip.png")]]
             stabilize-magnify-url
             [[:img
               (html/attr-ends :src "extensions/OggHandler/play.png")]]
             stabilize-videoplayer-url
             [[(html/attr-starts :href "//")]] externalize-url
             [[(html/attr-starts :src "//")]] externalize-url
             [[(html/attr-starts :srcset "//")]] externalize-srcset-url
             [[(html/attr-contains :href "/wiki/File:")]] externalize-fd-link)))

(defn fix-article [article]
  (->> (transform article) html/emit* (apply str)))
