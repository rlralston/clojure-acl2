; Obj-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(include-book "../../model-translation")

(defconst *clojure.lang.Obj*
 (make-class-def
      '(class "clojure.lang.Obj"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "_meta" (class "clojure.lang.IPersistentMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
                                        (methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "_meta" "clojure.lang.Obj" (class "clojure.lang.IPersistentMap"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
                                        (methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "_meta" "clojure.lang.Obj" (class "clojure.lang.IPersistentMap"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "meta"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "_meta" "clojure.lang.Obj" (class "clojure.lang.IPersistentMap"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        ;(method "withMeta"
                        ;      (parameters (class "clojure.lang.IPersistentMap"))
                        ;      (returntype . (class "clojure.lang.Obj"))
                        ;      (accessflags  *abstract*  *class*  *public* )
                        ;      (code))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
                                        (methodCP "withMeta" "clojure.lang.Obj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.Obj"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IObj" "java.io.Serializable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))#|ACL2s-ToDo-Line|#



;(defconst *Obj-class-table*
;  (make-static-class-decls 
;   *clojure.lang.Obj*))

;(defconst *package-name-map* 
;  ("clojure.lang.Obj" . "clojure.lang"))
