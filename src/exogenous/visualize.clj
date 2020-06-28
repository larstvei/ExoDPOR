(ns exogenous.visualize
  (:require [exogenous.core :as exo]
            [quil.core :as q]
            [clojure.string :as s]
            [quil.middleware :as m]
            [clojure.set :refer [difference]]
            [render-latex.core :as tex]))

(def event-short-name
  (memoize
   (let [id (atom 3)]
     (fn [ev]
       (or
        ({{:thread 0, :tag 0, :op :r, :x 0, :ms 100} "e_0"
          {:thread 1, :tag 0, :op :w, :x 0, :ms 100} "e_1"
          {:thread 2, :tag 0, :op :r, :x 0, :ms 100} "e_2"
          {:thread 2, :tag 1, :op :r, :x 1, :ms 100} "e_3"} ev)
        (apply str "e_" (str (swap! id inc))))))))

(def init-state {[] {}})

(defn setup
  ([] (setup {:steps [@exo/search-state]
              :pos 0 :bg 255 :fg 0 :pt 5}))
  ([state]
   (q/no-stroke)
   (q/stroke-weight 2)
   (q/rect-mode :center)
   (q/text-align :center)
   (q/shape-mode :center)
   state))

(defn update-state [state]
  (let [current-state @exo/search-state]
    (cond (= (peek (:steps state)) current-state) state
          (= current-state {}) (setup)
          :else (-> (assoc state :pos (count (:steps state)))
                    (update :steps conj current-state)))))

;;; Implementation of http://logical.ai/fn.trees/drawingtrees.pdf
(defn move-tree [tree x]
  (update tree :x + x))

(defn move-extent [e x]
  (map (fn [[p q]] [(+ p x) (+ p x)]) e))

(defn merge-extent [[[p _] & ps :as e1] [[_ q] & qs :as e2]]
  (cond (nil? q) e1
        (nil? p) e2
        :else (cons [p q] (merge-extent ps qs))))

(defn merge-extent-list [es]
  (reduce merge-extent () es))

(defn rmax [p q]
  (if (> p q) p q))

(defn fit-extent [[[_ p] & ps] [[q _] & qs]]
  (if (and p q)
    (rmax (+ (- p q) 1.0) (fit-extent ps qs))
    0.0))

(defn fit-extent-list-l [es]
  (letfn [(fitlistl [acc [e & es]]
            (if (nil? e)
              ()
              (let [x (fit-extent acc e)
                    e2 (merge-extent acc (move-extent e x))]
                (cons x (fitlistl e2 es)))))]
    (fitlistl () es)))

(defn flip-extent [es]
  (map (fn [[p q]] [(- q) (- p)]) es))

(defn fit-extent-list-r [es]
  (->> (reverse es)
       (map flip-extent)
       (fit-extent-list-l)
       (map -)
       (reverse)))

(defn mean [x y]
  (/ (+ x y) 2.0))

(defn fit-extent-list [es]
  (map mean (fit-extent-list-l es) (fit-extent-list-r es)))

(defn search-state->tree
  ([state] (search-state->tree state []))
  ([state path]
   (let [enabled (get-in state [path :enabled])
         paths (->> (map #(conj path %) enabled)
                    (filter state))]
     {:path path
      :node (state path)
      :subtrees (->> (sort-by (comp event-short-name last) paths)
                     (map #(search-state->tree state %)))})))

(defn design [state]
  (letfn [(design-1 [{:keys [subtrees] :as tree}]
            (let [res (map design-1 subtrees)
                  trees (map first res)
                  extents (map second res)
                  positions (fit-extent-list extents)
                  ptrees (map move-tree trees positions)
                  pextents (map move-extent extents positions)
                  result-extent (cons [0.0 0.0] (merge-extent-list pextents))
                  result-tree (assoc (assoc tree :x 0.0) :subtrees ptrees)]
              [result-tree result-extent]))]
    (first (design-1 state))))

(defn minmax-width
  ([tree] [(minmax-width min-key tree)
           (minmax-width max-key tree)])
  ([f-key {:keys [x subtrees]}]
   (if (empty? subtrees)
     x
     (+ x (/ (minmax-width f-key (apply f-key :x subtrees))
             (count subtrees))))))

(defn draw-tex-on-white [shp]
  (q/push-style)
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 (+ 10 (.-width shp)) (+ 10 (.-height shp)))
  (q/fill 0)
  (q/shape shp)
  (q/pop-style))

(defn draw-node [node]
  (let [enabled (sort (map event-short-name (:enabled node)))
        backset (sort (map event-short-name (:backset node)))
        sleep (sort (map event-short-name (:sleep node)))
        shp (tex/latex
             (tex/tex
              '(:begin align*)
              "E &= \\{" (s/join "," enabled) "\\}\\\\"
              "B &= \\{" (s/join "," backset) "\\}\\\\"
              "S &= \\{" (s/join "," sleep) "\\}\\\\"
              '(:end align*)))]
    (draw-tex-on-white shp)))

(defn draw-tree [{:keys [path node subtrees x]} space h]
  (let [nspace (/ space (max 1 (count subtrees)))]
    (doseq [t subtrees]
      (let [x1 (* nspace (:x t))
            label (tex/$ (event-short-name (last (:path t))))]
        (q/line 0 0 x1 h)
        (q/with-translation [(/ x1 2) (/ h 2)]
          (draw-tex-on-white (tex/latex label)))
        (q/with-translation [x1 h]
          (draw-tree t nspace h)))))
  (draw-node node))

(defn event-str [{:keys [thread tag op x]}]
  (tex/tex :langle
           :iota \_ (inc thread) \,
           "t_" (inc tag) \,
           (if (= op :r) "R" "W") \,
           (or ((zipmap (range) "xyzw") x) (str "x_{" x "}"))
           :rangle))

(defn draw-event-names [offset ss]
  (let [events (distinct (flatten (keys ss)))
        short-names (vec (map event-short-name events))
        m (sort-by first (zipmap short-names events))
        shp (tex/latex
             (tex/tex
              '(:begin align*)
              (for [[id ev] m]
                (str id " &= " (event-str ev) "\\\\"))
              '(:end align*)))]
    (q/with-translation [(- (* 0.85 offset)) 0]
      (draw-tex-on-white shp))))

(defn draw [{:keys [steps pos bg fg pt info]}]
  (q/background bg)
  (q/fill fg)
  (when info
    (q/text (str "fps: " (int (q/current-frame-rate))) 50 20))
  (let [search-state (steps pos)
        pad (* 0.05 (max (q/width) (q/height)))
        lengths (map count (keys search-state))
        tree (design (search-state->tree search-state))
        depth (apply max 1 lengths)
        h (/ (- (q/height) (* 2 pad)) depth)
        w (- (q/width) (* 2 pad))
        [minwidth maxwidth] (minmax-width tree)
        offset (if (= minwidth maxwidth)
                 (/ w 2)
                 (q/map-range 0 minwidth maxwidth 0 w))]
    (q/translate (+ offset pad) pad)
    (draw-event-names offset search-state)
    (q/stroke 0)
    (draw-tree tree (/ w 1.2) h)))

(defn render-pdf [state]
  (let [w (* (q/display-density) (q/width))
        h (* (q/display-density) (q/height))
        path "resources/screenshots/out-%02d.pdf"]
    (clojure.java.io/make-parents path)
    (dotimes [pos (count (:steps state))]
      (q/do-record
       (q/create-graphics w h :pdf (format path pos))
       (draw (setup (assoc state :pos pos))))))
  state)

(defn key-handler [state event]
  (case (q/key-as-keyword)
    :r (setup)
    :right (update state :pos #(mod (inc %) (count (:steps state))))
    :left (update state :pos #(mod (dec %) (count (:steps state))))
    :w (-> (assoc state :bg 255) (assoc :fg 0))
    :b (-> (assoc state :bg 0) (assoc :fg 255))
    :i (update state :info not)
    :s (render-pdf state)
    state))

(defn run-sketch []
  (q/defsketch exo-viz
    :title "exo-viz"
    :host "host"
    :size [600 600]
    :setup setup
    :update update-state
    :draw draw
    :key-pressed key-handler
    :settings (fn [] (q/pixel-density (q/display-density)))
    ;;   :renderer :p2d
    :features [:no-bind-output :resizable]
    :middleware [m/fun-mode]))

#_(run-sketch)
