(ns book.demos.root
  (:require
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :refer [defmutation set-string!]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.networking.http-remote :as http]
    [com.fulcrologic.fulcro.algorithms.react-interop :as react-interop]
    [com.wsscode.pathom.diplomat.http.fetch :as http-fetch]
    [com.wsscode.pathom.diplomat.http :as p.http]
    [com.wsscode.pathom.connect :as pc]
    [com.wsscode.common.async-cljs :refer [go-catch <!p <?]]

    [book.demos.mastodon :as mastodon]
    [book.demos.parser.test :as test]

    [com.fulcrologic.fulcro.data-fetch :as df]
    [cljs.core.async :as async :refer [chan put! take! >! <! timeout close! alts!]]
    [cljs.core.async :refer-macros [go go-loop alt!]]
    [goog.object :as gobj]
    [com.wsscode.pathom.core :as p]


    [com.fulcrologic.semantic-ui.factory-helpers :as h]
    [com.fulcrologic.semantic-ui.elements.input.ui-input :refer [ui-input]]
    [com.fulcrologic.semantic-ui.collections.form.ui-form :refer [ui-form]]
    [com.fulcrologic.semantic-ui.collections.form.ui-form-input :refer [ui-form-input]]

    [com.fulcrologic.semantic-ui.elements.button.ui-button :refer [ui-button]]
    [com.fulcrologic.semantic-ui.elements.button.ui-button-group :refer [ui-button-group]]
    [com.fulcrologic.semantic-ui.elements.icon.ui-icon :refer [ui-icon]]
    [com.fulcrologic.semantic-ui.icons :as i]

    [com.fulcrologic.semantic-ui.elements.label.ui-label :refer [ui-label]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list :refer [ui-list]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list-item :refer [ui-list-item]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list-content :refer [ui-list-content]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list-list :refer [ui-list-list]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list-header :refer [ui-list-header]]
    [com.fulcrologic.semantic-ui.elements.list.ui-list-description :refer [ui-list-description]]
    [com.fulcrologic.semantic-ui.elements.icon.ui-icon :refer [ui-icon]]
    [clojure.string :as str]
    ))


(comment
  (clog {:message "Hello, CLog" :color "blue"})

  (clojure.repl/doc List)

  )

(defn clog
  "
  The colors have been taken from https://developer.mozilla.org/en-US/docs/Web/CSS/color_value
  "
  [{:keys [message props color] :or {message "Hello, World!" color "green" props {}}}]
  (js/console.log (str "%c" message), (str "color: " color "; font-weight: bold; font-size: small;"))
  (js/console.log props))

(defonce app
         (app/fulcro-app
           {:remotes
            {:remote (http/fulcro-http-remote {})}}))

;; The root. Everything just composes to here (state and query)
;; Note, in core (where we create the app) there is no need to say anything about initial state!
(defmutation bump-number [ignored]
             (action [{:keys [state]}]
                     (swap! state update :ui/number inc)))


(defn listen
  [channel]
  "Listen to our channel for any events and log them"
  (async/go
    (while true
      (js/console.log (async/<! channel)))))


(pc/defresolver random-dog [env {:keys []}]
                {::pc/output [:dog.ceo/random-dog-url]}
                (go-catch
                  {:dog.ceo/random-dog-url
                   (-> (js/fetch "https://dog.ceo/api/breeds/image/random") <!p
                       (.json) <!p
                       (gobj/get "message"))}))

(pc/defresolver bookmark-tree-json [env {:keys []}]
                {::pc/output [:chrome.bookmarks/tree-json]}
                (go-catch
                  {:chrome.bookmarks/tree-json
                   (->
                     (js/fetch "/json/bookmarks-2019-10-02.json") <!p
                     (.json) <!p
                     (js->clj :keywordize-keys true)
                     )}))

(pc/defresolver bookmark-tree [env {:keys []}]
                {::pc/output [:chrome.bookmarks/tree]}
                (go-catch
                  {:chrome.bookmarks/tree
                   (->
                     (new js/Promise
                          (fn [resolve reject]
                            (.getTree js/chrome.bookmarks
                                      (fn [bookmarkTree]
                                        (do
                                          (resolve bookmarkTree))))))
                     <!p
                     (js->clj :keywordize-keys true)
                     )}))

(pc/defresolver bookmark-search
  [env {:search/keys [search]}]
  {::pc/input #{:search/search}
   ::pc/output [:chrome.bookmarks/search]}
  (go-catch
    (println (str "search term is: " search))
    {:chrome.bookmarks/search
     (->
       (new js/Promise
            (fn [resolve reject]
              (.search js/chrome.bookmarks
                       search
                       (fn [bookmarkTree]
                         (do
                           (prn bookmarkTree)
                           (resolve bookmarkTree))))))
       <!p
       (js->clj :keywordize-keys true)
       )}))




;; How to go from :person/id to that person's details
(pc/defresolver person-resolver [env {:keys [person/id] :as params}]
  ;; The minimum data we must already know in order to resolve the outputs
  {::pc/input  #{:person/id}
   ;; A query template for what this resolver outputs
   ::pc/output [:person/name {:person/address [:address/id]}]}
  ;; normally you'd pull the person from the db, and satisfy the listed
  ;; outputs. For demo, we just always return the same person details.
  {:person/name    "Tom"
   :person/address {:address/id 1}})

(pc/defresolver address-resolver [env {:keys [address/id] :as params}]
  {::pc/input  #{:address/id}
   ::pc/output [:address/city :address/state]}
  {:address/city "Salem"
   :address/state "MA"})

(defn spacex-plugin []
  {::pc/register [random-dog]})


(def http-driver http-fetch/request-async)

(defonce indexes (atom {}))

(def dog2-parser
  (p/parallel-parser
    {::p/env     {::p/reader               [p/map-reader
                                            pc/parallel-reader
                                            pc/open-ident-reader
                                            p/env-placeholder-reader]
                  ::p/placeholder-prefixes #{">"}
                  ::p.http/driver          http-driver}
     ::p/plugins [(pc/connect-plugin {#_#_::pc/register app-registry
                                      ::pc/indexes indexes})
                  (spacex-plugin)
                  p/error-handler-plugin
                  p/trace-plugin]}))

(def dog-parser
  (p/parallel-parser
    {::p/env     {::p/reader               [p/map-reader
                                            pc/parallel-reader
                                            pc/open-ident-reader
                                            p/env-placeholder-reader]
                  ::p/placeholder-prefixes #{">"}}
     ::p/plugins [(pc/connect-plugin {::pc/register
                                      [random-dog
                                       bookmark-tree
                                       bookmark-tree-json
                                       bookmark-search
                                       person-resolver
                                       address-resolver
                                       ]})
                  p/error-handler-plugin
                  p/trace-plugin]}))

(defn dog-api [entity query fn1]
  (take! (dog-parser {::p/entity (atom entity)} query) fn1))

(defmutation get-random-dog [_]
             (action [{:keys [state]}]
                     (clog {:message "random dog" :color "magenta" :props state})
                     (dog-api {} [:dog.ceo/random-dog-url] #(swap! state into %))))

(defmutation get-bookmark-tree [_]
             (action [{:keys [state]}]
                     (dog-api {} [:chrome.bookmarks/tree] #(swap! state into %))))

(defmutation get-bookmark-tree-json [_]
             (action [{:keys [state]}]
                     (dog-api {} [:chrome.bookmarks/tree-json] #(swap! state into %))))

(defmutation search-bookmark [{:keys [search-term]}]
  (action [{:keys [state]}]
          (dog-api {} [{[:search/search search-term] [:chrome.bookmarks/search]}]
                   (fn [result]
                     (do
                       (swap! state into (second (first result))))))))

(declare build-tag-db)

(defn build-tag-list [bookmark-tree]
  (let [tag-list (filter #(not (= "" %)) (map str/trim (str/split (:tags bookmark-tree) #",")))]
    (flatten (conj (or tag-list nil) (map
                                       #(build-tag-list %)
                                       (:children bookmark-tree))))))

(declare build-tag-cloud)
(defn build-tag-cloud [bookmark-tree tag-cloud]
  (let [
        tag-list (filter #(not (= "" %)) (map str/trim (str/split (:tags bookmark-tree) #",")))
        tg (conj
             tag-cloud
             (map
               (fn [tag] ({tag ((cons bookmark-tree (tag tag-cloud)))}))
               tag-list))
        ]
    (flatten (conj (or tag-cloud nil) (map
                                       #(build-tag-cloud % (flatten tg))
                                       (:children bookmark-tree))))))

(defn pi
  "Approximate Pi to the 1/n decimal with Leibniz formula"
  [n]
  (transduce
    (comp (map #(/ 4 %)) (take n))
    +
    (iterate #(* ((if (pos? %) + -) % 2) -1) 1.0)))

(comment
  (prn (pi 1e8)))



(comment

  (dog-api {} [:dog.ceo/random-dog-url] #(prn %))

  (dog-api {} [:chrome.bookmarks/tree] #(prn %))

  (dog-api {} [{[:search/search "clojure"] [:chrome.bookmarks/search]}] #(prn %))

  (dog-api {} [:chrome.bookmarks/tree-json] (fn [tree]
                                              (prn (count  (build-tag-list (:chrome.bookmarks/tree-json tree))))))

  (dog-api {} [:chrome.bookmarks/tree-json] (fn [tree]
                                              (prn (build-tag-cloud (:chrome.bookmarks/tree-json tree) nil))))

  (dog-api {} [{[:person/id 1] [:person/name ]}] #(prn %))


  (dog-parser {} [:dog.ceo/random-dog-url])

  (go
    (prn
      (<? (dog-parser {} [:dog.ceo/random-dog-url]))))
  )




(declare ui-bookmarknode)

(defsc BookmarkNode [this {:keys [id title dateAdded children tags uri]}]
  {
   :initial-state { :id 0
                   :title ""
                   :dateAdded 0
                   :children []
                   :tags "test, element"
                   :uri "#"
                   }
   :ident [:bookmark/by-id :id]
   }
  (ui-list-item {}
    (ui-icon {:name i/folder-icon})
    (ui-list-content {}
      (ui-list-header {}
                      (dom/a {:href uri :target "__blank"}
                             (str title)))
      (ui-list-content {}
                       (dom/div
                         (.toString (js/Date. dateAdded)))
                       (let [tag-list (filter #(not (= "" %)) (map str/trim (str/split tags #",")))]
                         (dom/div
                           (map
                             (fn [tag]
                               (ui-label {:key tag} (str tag )))
                             tag-list
                             )
                           ))
                       )
      (ui-list-list {}
        (map (fn [child]
               (ui-bookmarknode child)
               )
             children
             )
        ))))

(defsc BookmarkNodeLegacy [this {:keys [id title dateAdded children url]}]
  {
   :initial-state { :id "0"
                   :title ""
                   :dateAdded 0
                   :uri "#"
                   :children []
                   }}
  (dom/div {:className "item"}
    (ui-icon {:name i/folder-icon})
    (dom/div {:className "content"}
      (dom/div {:className "title"}
              (dom/a {:href url}
                     title))
      (dom/div {:className "description"} (.toString (js/Date. dateAdded)))
      (dom/div {:className "list"
                }
        (map (fn [child]
               (ui-bookmarknode child)
               )
             children
             )
        ))))



(def ui-bookmarknode (comp/factory BookmarkNode {:key-fn :id}))


(defsc Bookmarktree [this {:chrome.bookmarks/keys [tree tree-json search] :search/keys [search-term]}]
  {:query         [:chrome.bookmarks/tree-json :chrome.bookmarks/tree :chrome.bookmarks/search :search/search-term]
   :ident         (fn [] [::id "tree-singleton"])
   :initial-state {
                   :chrome.bookmarks/tree-json  {:id "0" :title "json" :dateAdded 0 :children []}
                   :chrome.bookmarks/tree [{:id "0" :title "tree" :dateAdded 0 :children []}]
                   :chrome.bookmarks/search [{:id "0" :title "search" :dateAdded 0 :children []}]
                   :search/search-term ""
                   }}
  (let [on-search-term-change (fn [evt _] (set-string! this :search/search-term :event evt))]
    (clog {:message (str tree)})
    (dom/div
      (ui-button-group nil
                       (ui-button {:icon true}
                                  (ui-icon {:name i/align-left-icon}))
                       (ui-button {:icon true}
                                  (ui-icon {:name i/align-center-icon}))
                       (ui-button {:icon true}
                                  (ui-icon {:name i/align-right-icon}))
                       (ui-button {:icon true}
                                  (ui-icon {:name i/align-justify-icon}))
                       )
      (ui-button {:onClick #(comp/transact! this `[(get-bookmark-tree {})])}
                  "get bookmarks")
      (ui-button {:onClick #(comp/transact! this `[(get-bookmark-tree-json {})])}
                  "get json")
      (ui-input {:value search-term :onChange on-search-term-change})
      (ui-button {:icon true :onClick #(comp/transact! this `[(search-bookmark {:search-term  ~search-term})])}
                 (ui-icon {:name i/search-icon})
                 (str "find " search-term)
                 )
      (dom/div
        (ui-list {}
                 (map
                   (fn [treeNode]
                     (ui-bookmarknode treeNode))
                   tree))
        (ui-list {}
                 (map
                   (fn [treeNode]
                     (ui-bookmarknode treeNode))
                   search))
        (ui-list {}
                 (ui-bookmarknode tree-json)))
      )))

(def ui-bookmarktree (comp/factory Bookmarktree))

(defsc Root [this {:ui/keys [number] :dog.ceo/keys [random-dog-url] :as props}]
       {:query         [:ui/number
                        :dog.ceo/random-dog-url
                        {:chrome.bookmarks/tree (comp/get-query Bookmarktree)}]
        :initial-state {:ui/number 0
                        :dog.ceo/random-dog-url "test"
                        }}
       (dom/div
         (dom/h4 "This is an example.")
         (dom/button {:onClick #(comp/transact! this `[(bump-number {})])}
                     "You've clicked this button " number " times.")
         (dom/button {:onClick #(comp/transact! this `[(get-random-dog {})])}
                     "get a random dog")
         (dom/img {:src random-dog-url :alt random-dog-url})
         (ui-bookmarktree props)
         ))




(defn ^:export init
  "Shadow-cljs sets this up to be our entry-point function. See shadow-cljs.edn `:init-fn` in the modules of the main build."
  []
  (app/mount! app Bookmarktree "app")
  (js/console.log "Loaded"))

(defn ^:export refresh
  "During development, shadow-cljs will call this on every hot reload of source. See shadow-cljs.edn"
  []
  ;; re-mounting will cause forced UI refresh, update internals, etc.
  (app/mount! app Bookmarktree "app")
  (js/console.log "Hot reload"))

(def bookmark-out
  [:bookmark/index
   :bookmark/type
   :bookmark/title
   :bookmark/id
   :bookmark/url
   :bookmark/dateAdded
   :bookmark/dateGroupModified
   :bookmark/parentId
   ])

