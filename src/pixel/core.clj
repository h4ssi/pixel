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

(defn render-loop [g]
  (loop []
    (let [g (g)]
      (.setColor g java.awt.Color/RED)
      (.drawRect g 0 0 100 100)
      (.dispose g)
      (recur))))

(defn with-full-screen-window [f]
  (let [w (create-window)]
    (try
      (set-full-screen w)
      (f #(.getGraphics w))
      (finally
        (unset-full-screen)
        (.dispose w)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
