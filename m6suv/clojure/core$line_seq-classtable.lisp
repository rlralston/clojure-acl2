; core$line_seq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$line_seq*
 (make-class-def
      '(class "clojure.core$line_seq"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "cons"))
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
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "cons"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$line_seq" (class "clojure.lang.Var"))))
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
                                   (max_stack . 8) (max_locals . 4) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.BufferedReader"))) 
                                      (4 (invokevirtual (methodCP "readLine" "java.io.BufferedReader" () (class "java.lang.String")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (dup)) 
                                      (10 (ifnull 63)) ;;to TAG_0
                                      (13 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (16 (if_acmpeq 64)) ;;to TAG_1
                                      (19 (aload_2)) 
                                      (20 (aconst_null)) 
                                      (21 (astore_2)) 
                                      (22 (astore_3)) 
                                      (23 (getstatic (fieldCP "const__0" "clojure.core$line_seq" (class "clojure.lang.Var")))) 
                                      (26 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload_3)) 
                                      (33 (aconst_null)) 
                                      (34 (astore_3)) 
                                      (35 (new (class "clojure.lang.LazySeq"))) 
                                      (38 (dup)) 
                                      (39 (new (class "clojure.core$line_seq$fn__4291"))) 
                                      (42 (dup)) 
                                      (43 (aload_1)) 
                                      (44 (aconst_null)) 
                                      (45 (astore_1)) 
                                      (46 (invokespecial (methodCP "<init>" "clojure.core$line_seq$fn__4291" ((class "java.lang.Object")) void))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (invokespecial (methodCP "<init>" "clojure.lang.LazySeq" ((class "clojure.lang.IFn")) void))) 
                                      (55 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (60 (goto 65))  ;;to TAG_2
                                      (63 (pop)) ;;at TAG_0
                                      (64 (aconst_null)) ;;at TAG_1
                                      (65 (areturn)) ;;at TAG_2
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$line_seq-class-table*
  (make-static-class-decls 
   *clojure.core$line_seq*))

(defconst *package-name-map* 
  ("clojure.core$line_seq" . "clojure"))

