(ns markov-toy-main
  (:require [clojure.string :as string]
            [markov-toy :refer [load-chain generate-tweet]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(comment "making new chains"
         (def files ["nfaa.txt" "testamente.txt"])
         (clojure.pprint/pprint markov-toy/functional-svenska (clojure.java.io/writer "foo.edn"))
         (def functional-svenska
           (apply merge-with clojure.set/union (map process-file files))))

(def svenska (load-chain "svenska.edn.gz"))

(def prefix-list ["hans nåd" "och så" "men det" "därför att" "jag har"
                  "hans nåd." "en liten" "men jag" "en smula" "hennes nåd" "jag vill"
                  "nej, det" "ja, det" "vill du" "nå så," "han tog" "ja, jag"
                  "skulle det" "va fan" "jag skulle" "så förbålt"
                  "men du" "i ett" "jag tycker" "han har" "fördöme mig," "hör nu,"
                  "jag tror" "jag skall" "en gammal" "för resten"
                  "men jag" "den unge" "nej, men" "jag ska"
                  "kära syster" "lilla kusin"
                  "tycker du" "sådana där"

                  "t. ex." "i synnerhet" "för den" "till den" "en del" "för en"
                  "en stor" "i allmänhet" "till följd" "bl. a." "ett slags"
                  "på det" "det är" "genom sin" "genom att" "till det"
                  "må nämnas" "i sitt" "från den" "i början" "under den" "på grund"
                  "en viss" "till konung" "största delen" "i sjelfva"
                  "för öfrigt" "i forntiden"])

(def prefix-list (concat prefix-list (map string/capitalize prefix-list)))

(defn something-interesting []
  (loop [candidate (generate-tweet prefix-list svenska)]
    (if (< 20 (count candidate))
      candidate
      (something-interesting))))

(defn handler [_]
  {:status  200
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    (str (something-interesting) "\n")})

(defn start-server [port]
  (run-jetty handler {:port  port
                      :join? false}))

(defn -main []
  (let [port (java.lang.Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (start-server port)))
