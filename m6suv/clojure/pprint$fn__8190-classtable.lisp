; pprint$fn__8190-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__8190*
 (make-class-def
      '(class "clojure.pprint$fn__8190"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "string?")
                        (STRING  "clojure.pprint")
                        (STRING  "cached-compile")
                        (STRING  "~<[~;~@{~w~^, ~:_~}~;]~:>"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "string?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__8190" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "cached-compile"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$fn__8190" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 61)
                                   (parsedcode
                                      (0 (ldc 4)) ;;STRING:: "~<[~;~@{~w~^, ~:_~}~;]~:>"
                                      (2 (astore_1)) 
                                      (3 (getstatic (fieldCP "const__0" "clojure.pprint$fn__8190" (class "clojure.lang.Var")))) 
                                      (6 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (9 (checkcast (class "clojure.lang.IFn"))) 
                                      (12 (aload_1)) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (18 (dup)) 
                                      (19 (ifnull 45)) ;;to TAG_0
                                      (22 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (25 (if_acmpeq 46)) ;;to TAG_1
                                      (28 (getstatic (fieldCP "const__1" "clojure.pprint$fn__8190" (class "clojure.lang.Var")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_1)) 
                                      (35 (aconst_null)) 
                                      (36 (astore_1)) 
                                      (37 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (42 (goto 49))  ;;to TAG_2
                                      (45 (pop)) ;;at TAG_0
                                      (46 (aload_1)) ;;at TAG_1
                                      (47 (aconst_null)) 
                                      (48 (astore_1)) 
                                      (49 (astore_2)) ;;at TAG_2
                                      (50 (new (class "clojure.pprint$fn__8190$fn__8191"))) 
                                      (53 (dup)) 
                                      (54 (aload_2)) 
                                      (55 (aconst_null)) 
                                      (56 (astore_2)) 
                                      (57 (invokespecial (methodCP "<init>" "clojure.pprint$fn__8190$fn__8191" ((class "java.lang.Object")) void))) 
                                      (60 (areturn)) 
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__8190-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__8190*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__8190" . "clojure"))

