(ns pixel.core
  (:gen-class))

(defn create-window []
  (let [f (java.awt.Frame. "pixel")]
    (.setUndecorated f true)
    f))

(defn screen-service []
  (let [ge (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
        sd (.getDefaultScreenDevice ge)]
    sd))

(defn set-full-screen [f]
  (.setIgnoreRepaint f true)
  (.setFullScreenWindow (screen-service) f))

(defn unset-full-screen []
  (.setFullScreenWindow (screen-service) nil))

(defrecord DevGameLoop [game-step-fn reload update-fn render-fn])
(defn game-step-fn
  "get the game step function of a game loop"
  [loop] (:game-step-fn loop))
(defn game-step
  "invoke the game step function of a game loop
  a game step function takes a function g, which takes no arguments and must return a java.awt.Graphics object
  the game step function should be invoked as often as possible,
  it will render the game and update the state as appropriate
  the loop will stop, once (false? state) holds"
  [loop g] ((game-step-fn loop) g))
(defn reload
  "dynamically change render-fn, update-fn and/or the game state of a game loop
  provide named arguments :render, :update and/or :state respectively
  :state may either be a function transitioning the current state into a new desired state,
  or just anything else, which will then become the new state"
  [loop & args] (apply (:reload loop) args))
(defn update-fn [loop]
  "get the update function of a game loop"
  ((:update-fn loop)))
(defn update
  "invoke the update function of a game loop
  to obtain the initial state of the game loop, the function is invoked with no arguments
  to progress the state, the function will be called with the previous state
  in both cases, the function is supposed to return the next game state"
  ([loop] ((update-fn loop)))
  ([loop state] ((update-fn loop) state)))
(defn render-fn
  "get the render function of a game loop"
  [loop] ((:render-fn loop)))
(defn render [loop g]
  "invoke the render function of a game loop
  the render function takes a function g, which takes no argument and must return a java.awt.Graphics object
  this object can be used to render the scene"
  ((render-fn loop) g))

(defn- noop [& args] true)

(defn dev-game-loop [update-f render-f]
  (let [fs (atom {:update (or update-f noop)
                  :render (or render-f noop)})]
    (map->DevGameLoop
      {:game-step-fn (fn [g]
                       (loop [state ((:update @fs))]
                         (let [{:keys [update render]} @fs]
                           (when state
                             (render g state)
                             (recur (update state))))))
       :reload       (fn [& {:keys [update render state]}]
                       (let [previous @fs
                             prev-render (:render previous)
                             new-render (or render prev-render)
                             prev-update (:update previous)
                             u (or update prev-update)      ; todo keep update + render consistend, i.e. exchange render
                                                            ; only after one updated occured (i.e. add a swap! render
                                                            ; into the new update function
                             new-update (cond
                                          (nil? state)
                                          u

                                          (and (ifn? state) (not (coll? state)))
                                          (fn [& s]         ; todo, care for no state yet (i.e. loop not yet started)
                                            (swap! fs assoc :update u) ; reset update function on next iteration
                                            (apply state s))

                                          :else
                                          (fn [& _]
                                            (swap! fs assoc :update u)
                                            state))
                             ]
                         (swap! fs assoc :update new-update :render new-render)))
       :update-fn    (fn [] (:update @fs))
       :render-fn    (fn [] (:render @fs))})))

(defn render-square [g s]
  (let [g (g)]
    (.setColor g java.awt.Color/RED)
    (.drawRect g 0 0 100 100)
    (.dispose g)))

(defn render-fill-square [g s]
  (let [g (g)]
    (.setColor g java.awt.Color/RED)
    (.fillRect g 0 0 100 100)
    (.dispose g)))

(defn with-full-screen-window [step-fn]
  (let [w (create-window)]
    (try
      (set-full-screen w)
      (step-fn #(.getGraphics w))
      (finally
        (unset-full-screen)
        (.dispose w)))))

(defn run-game-loop [loop]
  (with-full-screen-window (game-step-fn loop)))

(defn- dummy-update
  ([] {})
  ([s] {}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))