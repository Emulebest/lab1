(ns lab1.core
    (:require [rum.core :as rum]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<! go]]))

(enable-console-print!)

(def input-state (atom {:text "Hello stranger"}))


(rum/defc input-with-state < rum/reactive []
  [:div
   [:h1 (:text (rum/react input-state))]
   [:input {:type      "text"
            :on-change (fn [e]
                         (swap! input-state update :text #(if (= (.. e -target -value) "") "Hello stranger" (.. e -target -value))))}]])

(rum/defc button-render-dogs [dogs]
  [:div
   [:button {:on-click (fn [_]
                         (go (let [response (<! (http/get "https://dog.ceo/api/breeds/list/all"
                                                          {:with-credentials? false}))]
                               (prn (:status response))
                               (prn (:body response))
                               (swap! dogs #(-> response
                                                :body
                                                :message)))))} "Get dogs"]])


(defn print-nested
  [vals]
  (map (fn [nested-name]
         [:h5 {:style {:color "red"}} nested-name]) vals))

(defn random-breed-button
  [brd dogs]
  [:div
   [:button {:on-click #(reset! brd (rand-nth (filter (fn [kv] (-> kv
                                                                   second
                                                                   empty?
                                                                   not)) dogs)))}
    "Random breed"]])

(rum/defcs random-breed < (rum/local nil :breed)
  [state dogs]
  (let [brd (:breed state)]
    (if (nil? @brd)
      (random-breed-button brd dogs)
      [:div
       (random-breed-button brd dogs)
       [:hr]
       [:h4 (-> @brd
                first
                name)]
       (print-nested (second @brd))
       [:hr]])))

(defn print-dogs
  [dogs]
  (reduce-kv (fn [m k v]
               (if (empty? v)
                 (conj m [:div
                          [:h4 (name k)]])
                 (conj m [:div
                          [:h4 (name k)]
                          (print-nested v)]))
               ) [] dogs))

(defn rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(rum/defc dogs-util-buttons [dogs]
  [:div
   [:button {:on-click (fn [_]
                         (swap! dogs #(assoc % (keyword (rand-str 12)) [])))}
    "Add new breed"]
   [:button {:on-click (fn [_]
                         (swap! dogs #(dissoc % (first (rand-nth (seq %))))))}
    "Delete random breed"]
   [:button {:on-click (fn [_]
                         (prn (shuffle @dogs)))}
    "Shuffle breeds (output in console)"]])

(rum/defcs dogs-component < (rum/local [] :dogs)
  [state]
  (let [dogs (:dogs state)]
    (prn @dogs)
    (if (not (empty? @dogs))
      [:div
       (dogs-util-buttons dogs)
       (random-breed @dogs)
       (print-dogs @dogs)]
      (button-render-dogs dogs))))

(rum/defc hello-world []
  [:div
   [(input-with-state)]
   (dogs-component)])

(rum/mount (hello-world)
           (. js/document (getElementById "app")))
