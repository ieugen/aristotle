(ns arachne.aristotle
  "Primary API"
  (:require [arachne.aristotle.graph :as g]
            [arachne.aristotle.query :as q]
            [arachne.aristotle.registry :as reg]
            [arachne.aristotle.inference :as inf]
            [arachne.aristotle.rdf-edn]
            [clojure.java.io :as io])
  (:import [org.apache.jena.reasoner.rulesys GenericRuleReasoner]
           [org.apache.jena.graph Factory Graph GraphUtil]
           [org.apache.jena.riot RDFDataMgr RDFFormat]
           [java.net URL]
           [java.io File])
  (:refer-clojure :exclude [read]))

(defmulti graph
  "Build a new, empty graph of the specified type.

   Built-in types are:

   :simple - A basic in-memory RDF graph with no reasoner.
   :jena-mini - Jena's partial implementation of OWL Full with
                an in-memory store..
   :jena-rules - Jena's GenericRuleReasoner. Takes a second argument,
                 which is a collection of rules to use (see
                 arachne.aristotle.inference for tools to create
                 rules and some pre-built rulesets.)"
  (fn [type & _] type))

(defmethod graph :simple
  [_]
  (Factory/createGraphMem))

(defmethod graph :jena-mini
  [_]
  (graph :jena-rules inf/mini-rules))

;; Note: You'll probably want to include the basic tabling rule to
;; avoid infinite lookups on recursive backchains

(defmethod graph :jena-rules
  [_ initial-rules]
  (let [reasoner (GenericRuleReasoner. initial-rules)]
     (.setOWLTranslation reasoner true)
     (.setTransitiveClosureCaching reasoner true)
     (.bind reasoner (Factory/createGraphMem))))

(defn add
  "Add the given data to a graph, returning the graph. Data must satisfy
  arachne.aristotle.graph/AsTriples. If the data is a Graph it will be
  added directly."
  [graph data]
  (if (instance? Graph data)
    (GraphUtil/addInto ^Graph graph ^Graph data)
    (GraphUtil/add ^Graph graph ^java.util.List (g/triples data)))
  graph)

(defn read
  "Load a file containing serialized RDF data into a graph, returning
  the graph. The file may be specified using:

  - String URIs,
  - java.net.URI,
  - java.net.URL
  - java.io.File"
  [^Graph graph file]
  (cond
    (string? file) (RDFDataMgr/read ^Graph graph ^String file)
    (uri? file) (RDFDataMgr/read ^Graph graph ^String (str file))
    (instance? java.net.URL file) (RDFDataMgr/read graph (str (.toURI ^URL file)))
    (instance? java.io.File file) (RDFDataMgr/read graph
                                                   (-> ^File file
                                                       (.getAbsoluteFile)
                                                       (.toURI)
                                                       (str))))
  graph)

(defn write
  "Serialize a graph and write to file.
  The file may be specified using:
  - String URIs,
  - java.net.URI,
  - java.net.URL
  - java.io.File
  File types available:
  ABBREV, BLOCKS, FLAT, NQ, NQUADS, NT, NTRIPLES, PLAIN, PRETTY, RDFJSON, RDFNULL,
  RDFXML, RDFXML_ABBREV, RDFXML_PLAIN, RDFXML_PRETTY, TRIG, TRIG_BLOCKS, TRIG_FLAT,
  TRIG_PRETTY, TTL, TURTLE, TURTLE_BLOCKS, TURTLE_FLAT, TURTLE_PRETTY"
  [^Graph graph file type]
  (let [t (cond
            (= type "ABBREV")        (RDFFormat/ABBREV)
            (= type "BLOCKS")        (RDFFormat/BLOCKS)
            (= type "FLAT")          (RDFFormat/FLAT)
            (= type "NQ")            (RDFFormat/NQ)
            (= type "NQUADS")        (RDFFormat/NQUADS)
            (= type "NT")            (RDFFormat/NT)
            (= type "NTRIPLES")      (RDFFormat/NTRIPLES)
            (= type "PLAIN")         (RDFFormat/PLAIN)
            (= type "PRETTY")        (RDFFormat/PRETTY)
            (= type "RDFJSON")       (RDFFormat/RDFJSON)
            (= type "RDFNULL")       (RDFFormat/RDFNULL)
            (= type "RDFXML")        (RDFFormat/RDFXML)
            (= type "RDFXML_ABBREV") (RDFFormat/RDFXML_ABBREV)
            (= type "RDFXML_PLAIN")  (RDFFormat/RDFXML_PLAIN)
            (= type "RDFXML_PRETTY") (RDFFormat/RDFXML_PRETTY)
            (= type "TRIG")          (RDFFormat/TRIG)
            (= type "TRIG_BLOCKS")   (RDFFormat/TRIG_BLOCKS)
            (= type "TRIG_FLAT")     (RDFFormat/TRIG_FLAT)
            (= type "TRIG_PRETTY")   (RDFFormat/TRIG_PRETTY)
            (= type "TTL")           (RDFFormat/TTL)
            (= type "TURTLE")        (RDFFormat/TURTLE)
            (= type "TURTLE_BLOCKS") (RDFFormat/TURTLE_BLOCKS)
            (= type "TURTLE_FLAT")   (RDFFormat/TURTLE_FLAT)
            (= type "TURTLE_PRETTY") (RDFFormat/TURTLE_PRETTY))]
    (with-open [o (io/output-stream
                    (cond
                      (string? file) file
                      (uri? file) (str file)
                      (instance? java.net.URL file) (str (.toURI ^URL file))
                      (instance? java.io.File file) (-> ^File file
                                                        (.getAbsoluteFile)
                                                        (.toURI)
                                                        (str))))]
      (RDFDataMgr/write o ^Graph graph ^RDFFormat t))))
