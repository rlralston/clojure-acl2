; Agent$ActionQueue-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Agent$ActionQueue*
 (make-class-def
      '(class "clojure.lang.Agent$ActionQueue"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "q" (class "clojure.lang.IPersistentStack") (accessflags  *class*  *final*  *public* ) -1)
                        (field "error" (class "java.lang.Throwable") (accessflags  *class*  *final*  *public* ) -1)
                        (field "EMPTY" (class "clojure.lang.Agent$ActionQueue") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentStack") (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "q" "clojure.lang.Agent$ActionQueue" (class "clojure.lang.IPersistentStack"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "error" "clojure.lang.Agent$ActionQueue" (class "java.lang.Throwable"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.Agent$ActionQueue")))
                                      (3 (dup))
                                      (4 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentQueue" (class "clojure.lang.PersistentQueue"))))
                                      (7 (aconst_null))
                                      (8 (invokespecial
					(methodCP "<init>" "clojure.lang.Agent$ActionQueue" ((class "clojure.lang.IPersistentStack") (class "java.lang.Throwable")) void)))
                                      (11 (putstatic (fieldCP "EMPTY" "clojure.lang.Agent$ActionQueue" (class "clojure.lang.Agent$ActionQueue"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Agent$ActionQueue-class-table*
  (make-static-class-decls 
   *clojure.lang.Agent$ActionQueue*))

(defconst *package-name-map* 
  ("clojure.lang.Agent$ActionQueue" . "clojure.lang"))
