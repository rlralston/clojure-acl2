; Agent$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Agent$1*
 (make-class-def
      '(class "clojure.lang.Agent$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$format" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "val$threadPoolCounter" (class "java.util.concurrent.atomic.AtomicLong") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.util.concurrent.atomic.AtomicLong"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$format" "clojure.lang.Agent$1" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$threadPoolCounter" "clojure.lang.Agent$1" (class "java.util.concurrent.atomic.AtomicLong"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newThread"
                              (parameters (class "java.lang.Runnable"))
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 39)
                                   (parsedcode
                                      (0 (new (class "java.lang.Thread")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.Thread" ((class "java.lang.Runnable")) void)))
                                      (8 (astore_2))
                                      (9 (aload_2))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "val$format" "clojure.lang.Agent$1" (class "java.lang.String"))))
                                      (14 (iconst_1))
                                      (15 (anewarray (class "java.lang.Object")))
                                      (18 (dup))
                                      (19 (iconst_0))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "val$threadPoolCounter" "clojure.lang.Agent$1" (class "java.util.concurrent.atomic.AtomicLong"))))
                                      (24 (invokevirtual
					(methodCP "getAndIncrement" "java.util.concurrent.atomic.AtomicLong" () long)))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (aastore))
                                      (31 (invokestatic
					(methodCP "format" "java.lang.String" ((class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.String"))))
                                      (34 (invokevirtual
					(methodCP "setName" "java.lang.Thread" ((class "java.lang.String")) void)))
                                      (37 (aload_2))
                                      (38 (areturn))
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.concurrent.ThreadFactory")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Agent$1-class-table*
  (make-static-class-decls 
   *clojure.lang.Agent$1*))

(defconst *package-name-map* 
  ("clojure.lang.Agent$1" . "clojure.lang"))

