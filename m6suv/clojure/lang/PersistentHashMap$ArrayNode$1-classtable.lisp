; PersistentHashMap$ArrayNode$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentHashMap$ArrayNode$1*
 (make-class-def
      '(class "clojure.lang.PersistentHashMap$ArrayNode$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$node" (class "clojure.lang.PersistentHashMap$INode") (accessflags  *class*  *final* ) -1)
                        (field "val$combinef" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "val$reducef" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "val$fjtask" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "val$fjfork" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "val$fjjoin" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "clojure.lang.PersistentHashMap$ArrayNode") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.PersistentHashMap$ArrayNode") (class "clojure.lang.PersistentHashMap$INode") (class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 8) (code_length . 44)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.PersistentHashMap$ArrayNode"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$node" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.PersistentHashMap$INode"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$combinef" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (15 (aload_0))
                                      (16 (aload 4))
                                      (18 (putfield (fieldCP "val$reducef" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (21 (aload_0))
                                      (22 (aload 5))
                                      (24 (putfield (fieldCP "val$fjtask" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (27 (aload_0))
                                      (28 (aload 6))
                                      (30 (putfield (fieldCP "val$fjfork" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (33 (aload_0))
                                      (34 (aload 7))
                                      (36 (putfield (fieldCP "val$fjjoin" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (39 (aload_0))
                                      (40 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (43 (return))
                                      (endofcode 44))
                                   (Exceptions )
                                   (StackMap )))
                        (method "call"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$node" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.PersistentHashMap$INode"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$combinef" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$reducef" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "val$fjtask" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "val$fjfork" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "val$fjjoin" "clojure.lang.PersistentHashMap$ArrayNode$1" (class "clojure.lang.IFn"))))
                                      (24 (invokeinterface
					(methodCP "fold" "clojure.lang.PersistentHashMap$INode" ((class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn") (class "clojure.lang.IFn")) (class "java.lang.Object")) 6))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.concurrent.Callable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *PersistentHashMap$ArrayNode$1-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentHashMap$ArrayNode$1*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentHashMap$ArrayNode$1" . "clojure.lang"))

