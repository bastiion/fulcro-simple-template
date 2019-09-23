(ns book.demos.root
  (:require
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.mutations :refer [defmutation]]
    [com.fulcrologic.fulcro.application :as app]))

(defonce app (app/fulcro-app))

;; The root. Everything just composes to here (state and query)
;; Note, in core (where we create the app) there is no need to say anything about initial state!
(defmutation bump-number [ignored]
             (action [{:keys [state]}]
                     (swap! state update :ui/number inc)))

(defsc Root [this {:ui/keys [number]}]
       {:query         [:ui/number]
        :initial-state {:ui/number 0}}
       (dom/div
         (dom/h4 "This is an example.")
         (dom/button {:onClick #(comp/transact! this `[(bump-number {})])}
                     "You've clicked this button " number " times.")))

(defn ^:export init
  "Shadow-cljs sets this up to be our entry-point function. See shadow-cljs.edn `:init-fn` in the modules of the main build."
  []
  (app/mount! app Root "app")
  (js/console.log "Loaded"))

(defn ^:export refresh
  "During development, shadow-cljs will call this on every hot reload of source. See shadow-cljs.edn"
  []
  ;; re-mounting will cause forced UI refresh, update internals, etc.
  (app/mount! app Root "app")
  (js/console.log "Hot reload"))
