; test$inc_report_counter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$inc_report_counter*
 (make-class-def
      '(class "clojure.test$inc_report_counter"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.test")
                        (STRING  "*report-counters*"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.test"
                                      (2 (ldc 1))         ;;STRING:: "*report-counters*"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$inc_report_counter" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 38)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$inc_report_counter" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (dup)) 
                                      (7 (ifnull 35)) ;;to TAG_0
                                      (10 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (13 (if_acmpeq 36)) ;;to TAG_1
                                      (16 (new (class "clojure.test$inc_report_counter$fn__7050"))) 
                                      (19 (dup)) 
                                      (20 (aload_1)) 
                                      (21 (aconst_null)) 
                                      (22 (astore_1)) 
                                      (23 (invokespecial (methodCP "<init>" "clojure.test$inc_report_counter$fn__7050" ((class "java.lang.Object")) void))) 
                                      (26 (checkcast (class "java.util.concurrent.Callable"))) 
                                      (29 (invokestatic (methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object")))) 
                                      (32 (goto 37))  ;;to TAG_2
                                      (35 (pop)) ;;at TAG_0
                                      (36 (aconst_null)) ;;at TAG_1
                                      (37 (areturn)) ;;at TAG_2
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$inc_report_counter-class-table*
  (make-static-class-decls 
   *clojure.test$inc_report_counter*))

(defconst *package-name-map* 
  ("clojure.test$inc_report_counter" . "clojure"))

