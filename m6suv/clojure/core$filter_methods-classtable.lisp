; core$filter_methods-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$filter_methods*
 (make-class-def
      '(class "clojure.core$filter_methods"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 28)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$filter_methods" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$filter_methods" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$filter_methods" (class "java.lang.Object"))))
                                      (27 (return))
                                      (endofcode 28))
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
                                   (max_stack . 6) (max_locals . 9) (code_length . 108)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (3 (astore_3)) 
                                      (4 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashSet" (class "clojure.lang.PersistentHashSet")))) 
                                      (7 (astore 4)) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (astore 5)) 
                                      (14 (aload 5)) ;;at TAG_2
                                      (16 (dup)) 
                                      (17 (ifnull 105)) ;;to TAG_0
                                      (20 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (23 (if_acmpeq 106)) ;;to TAG_1
                                      (26 (new (class "clojure.core$filter_methods$fn__5502"))) 
                                      (29 (dup)) 
                                      (30 (aload_3)) 
                                      (31 (aload_2)) 
                                      (32 (aload 5)) 
                                      (34 (aload 4)) 
                                      (36 (invokespecial (methodCP "<init>" "clojure.core$filter_methods$fn__5502" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (47 (astore 6)) 
                                      (49 (aload 6)) 
                                      (51 (lconst_0)) 
                                      (52 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (55 (aconst_null)) 
                                      (56 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (59 (astore 7)) 
                                      (61 (aload 6)) 
                                      (63 (aconst_null)) 
                                      (64 (astore 6)) 
                                      (66 (lconst_1)) 
                                      (67 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (70 (aconst_null)) 
                                      (71 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (74 (astore 8)) 
                                      (76 (aload 7)) 
                                      (78 (aconst_null)) 
                                      (79 (astore 7)) 
                                      (81 (aload 8)) 
                                      (83 (aconst_null)) 
                                      (84 (astore 8)) 
                                      (86 (aload 5)) 
                                      (88 (checkcast (class "java.lang.Class"))) 
                                      (91 (invokevirtual (methodCP "getSuperclass" "java.lang.Class" () (class "java.lang.Class")))) 
                                      (94 (astore 5)) 
                                      (96 (astore 4)) 
                                      (98 (astore_3)) 
                                      (99 (goto 14))  ;;to TAG_2
                                      (102 (goto 107)) ;;to TAG_3
                                      (105 (pop)) ;;at TAG_0
                                      (106 (aload_3)) ;;at TAG_1
                                      (107 (areturn)) ;;at TAG_3
                                      (endofcode 108))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$filter_methods-class-table*
  (make-static-class-decls 
   *clojure.core$filter_methods*))

(defconst *package-name-map* 
  ("clojure.core$filter_methods" . "clojure"))

