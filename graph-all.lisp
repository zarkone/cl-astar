(load "graph-one-thread.fasl")
(print "haha.")
(open-graph "tests/256.txt")
(time
 (a-star 
  (select-v 3 4) 
  (select-v 188 125)))

