; core$pref-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$pref*
 (make-class-def
      '(class "clojure.core$pref"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields)
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
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.lang.Class"))) 
                                      (4 (aload_2)) 
                                      (5 (checkcast (class "java.lang.Class"))) 
                                      (8 (invokevirtual (methodCP "isAssignableFrom" "java.lang.Class" ((class "java.lang.Class")) boolean))) 
                                      (11 (ifeq 21))  ;;to TAG_0
                                      (14 (aload_2)) 
                                      (15 (aconst_null)) 
                                      (16 (astore_2)) 
                                      (17 (goto 24)) ;;to TAG_1
                                      (20 (pop)) 
                                      (21 (aload_1)) ;;at TAG_0
                                      (22 (aconst_null)) 
                                      (23 (astore_1)) 
                                      (24 (areturn)) ;;at TAG_1
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$pref-class-table*
  (make-static-class-decls 
   *clojure.core$pref*))

(defconst *package-name-map* 
  ("clojure.core$pref" . "clojure"))

