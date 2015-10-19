(ns book-examples )


(let [[f s & rest] "abcd"]
  (println rest))


(def v [1 2 3 4 5])
(v 3)

(def ass-array {:a 1 :b 2 :c 3})
(let [{frs :a mid :b lst :c} ass-array]
  lst)

(def map-in-vec ["James" {:birthday (java.util.Date. 73 1 6)}])

(let [[name {bd :birthday}] map-in-vec] 
  (str name " was born on " bd))

(apply (partial + 2) 3 4)


(def ↑2 #(* % %))
(defn fact [n]
  (if (= n 0) 1 (* n (fact (- n 1)))))

(defn replace-symbol [ch s]
  (if (= nil (next s)) (first s)
  (str
   (if (= ch (first s)) \. (first s))
   (replace-symbol ch (next s)))))

(def replace-0 (partial replace-symbol \0))
(def replace-2 (partial replace-symbol \2))

(replace-2 "00120020")
(def simple-range (range 0 10000))


;; ------------------------------------------------

(defn str-seq [n]
  (if (= n 0) 
    "0"
    (str n "," (str-seq (- n 1)))))

(defn run-test [f n label]
  (println label)
  (time (dorun (f str-seq (range 1 n)))))

(defn compare-map-pmap [n]
  (run-test map n "Single:")
  (run-test pmap n "Multi:"))


(compare-map-pmap 1500)


;; ------------------------------------------------

(sort < (repeatedly 10 #(rand-int 10)))

(sort (comp - compare) (repeatedly 10 #(rand-int 10)))

(map (partial * 2) #{1 23 })



(http/get couch-host)

(def couch-host "http://myvm.local:5984")
(defn couch-get 
  ([uri]
     (json/read-str 
      (:body (http/get (str couch-host uri)))
      :key-fn keyword))
  ([uri key]
     (key (json/read-str 
           (:body (http/get (str couch-host uri)))
           :key-fn keyword))))

(couch-get "" :couchdb)

(let 
    [{{name :name} :vendor } 
     (couch-get "/")]
  (println name))

(couch-get "/")

(:body (http/delete "http://myvm.local:5984/baseball"))

(clojure.string/split "1,2,3,4" #",")



;; ----------  ---------- ------------ 

(http/put "http://myvm.local:5984/albums")

(couch-get "/albums")
(couch-get "/_uuids?count=10")

(http/put
 "http://myvm.local:5984/albums/6e1295ed6c29495e54cc05947f18c8af"
 {:body 
  (json/write-str 
   {:title "just hello.." :artist "DD-cage"} )})

(:_rev
 (couch-get "/albums/6e1295ed6c29495e54cc05947f18c8af" ))

(http/put
 "http://myvm.local:5984/albums/6e1295ed6c29495e54cc05947f18c8af"
 {:body 
  (json/write-str 
   {:_rev (:_rev (couch-get "/albums/6e1295ed6c29495e54cc05947f18c8af" ))
    :title "just hello.." 
    :artist "HAHA"} )})

(couch-get "/hello/")


;; ------------- -------------------------- --------------------


(http/put "http://myvm.local:5984/basic")

(http/put 
 "http://myvm.local:5984/basic/_design/example"
 {:body
  (json/write-str 
   {:_id "_design/example",
    :views 
    {:foo 
     {:map "function(doc){ emit(doc._id, doc._rev)}"}}} 
                  :escape-slash false)})

(http/post 
 "http://myvm.local:5984/basic"
 {:body "{}"  :content-type :json})


(doseq 
    [row (:rows (couch-get "/basic/_design/example/_view/foo"))]
  (println row))

(defn fact [n]
  (if (= 0 n) 1 (* n (fact (- n 1)))))


update-in
