(ns arachne.aristotle-test
  (:require [clojure.test :refer :all]
            [arachne.aristotle :as aa]
            [arachne.aristotle.registry :as reg]
            [arachne.aristotle.query :as q]
            [arachne.aristotle.graph :as g]
            [arachne.aristotle.validation :as v]
            [clojure.java.io :as io])
  (:import [org.apache.jena.graph Graph]
           [org.apache.jena.sparql.util IsoMatcher]))

(deftest read-write
  (let [graph (aa/graph :simple)
        graph2 (aa/graph :simple)]
    (aa/read graph "./foaf.rdf")
    (aa/write graph "./test/test.rdf" "RDFXML")
    (aa/read graph2 "./test/test.rdf")
    (io/delete-file "./test/test.rdf")
    (is (IsoMatcher/isomorphic ^Graph graph ^Graph graph2))))

(deftest read-write-2
  (let [graph (aa/graph :simple)
        graph2 (aa/graph :simple)]
    (aa/read graph "./la_census.rdf")
    (aa/write graph "./test/test.rdf" "RDFXML")
    (aa/read graph2 "./test/test.rdf")
    (io/delete-file "./test/test.rdf")
    (is (IsoMatcher/isomorphic ^Graph graph ^Graph graph2))))

(deftest read-write-3
  (let [graph (aa/graph :simple)
        graph2 (aa/graph :simple)]
    (aa/read graph "./TheFirm.n3")
    (aa/write graph "./test/test.rdf" "RDFXML")
    (aa/read graph2 "./test/test.rdf")
    (io/delete-file "./test/test.rdf")
    (is (IsoMatcher/isomorphic ^Graph graph ^Graph graph2))))