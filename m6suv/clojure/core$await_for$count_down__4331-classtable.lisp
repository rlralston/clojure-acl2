; core$await_for$count_down__4331-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$await_for$count_down__4331*
 (make-class-def
      '(class "clojure.core$await_for$count_down__4331"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "latch" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "latch" "clojure.core$await_for$count_down__4331" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "latch" "clojure.core$await_for$count_down__4331" (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.util.concurrent.CountDownLatch")))
                                      (7 (invokevirtual
					(methodCP "countDown" "java.util.concurrent.CountDownLatch" () void)))
                                      (10 (aconst_null))
                                      (11 (pop))
                                      (12 (aload_1))
                                      (13 (aconst_null))
                                      (14 (astore_1))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$await_for$count_down__4331-class-table*
  (make-static-class-decls 
   *clojure.core$await_for$count_down__4331*))

(defconst *package-name-map* 
  ("clojure.core$await_for$count_down__4331" . "clojure"))

