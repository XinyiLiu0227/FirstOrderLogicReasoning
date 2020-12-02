This code is built upon the code I submitted for lisp assignment 2. Hypothesize5 generates hypotheses from a set of fact without replication for the same event even when it participates in multiple right-before relationship.

Due to the time limitation, the hypotheses generation function I implemented is limited to at most 9 e{i}.f needed, in other words, 10 events in specified sequence. Also, this function just ignores those facts that are not of interest to us instead of reporting error.

This method learns from the facts of sequential events happening and make inductive inference from it. Therefore, it help machine understand dynamic world instead of just static world. It is promising just as the last homework that it can learn more accurate certainties with more data available. However, unlike human being, it has limitation to actually build casual relationship between events that happen in order, which further hinder it from gaining more accurate predication from limited data. 

