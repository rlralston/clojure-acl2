; pprint$consume-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$consume*
 (make-class-def
      '(class "clojure.pprint$consume"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply")
                        (STRING  "nth")
                        (STRING  "not")
                        (STRING  "conj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 67)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$consume" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$consume" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$consume" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$consume" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "not"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$consume" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "conj"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.pprint$consume" (class "clojure.lang.Var"))))
                                      (66 (return))
                                      (endofcode 67))
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
                                   (max_stack . 6) (max_locals . 8) (code_length . 147)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_2)) 
                                      (3 (astore_3)) 
                                      (4 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (7 (astore 4)) 
                                      (9 (getstatic (fieldCP "const__0" "clojure.pprint$consume" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_1)) 
                                      (19 (iconst_1)) 
                                      (20 (anewarray (class "java.lang.Object"))) 
                                      (23 (dup)) 
                                      (24 (iconst_0)) 
                                      (25 (aload_3)) 
                                      (26 (aastore)) 
                                      (27 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (35 (astore 5)) 
                                      (37 (aload 5)) 
                                      (39 (lconst_0)) 
                                      (40 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (43 (aconst_null)) 
                                      (44 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (47 (astore 6)) 
                                      (49 (aload 5)) 
                                      (51 (aconst_null)) 
                                      (52 (astore 5)) 
                                      (54 (lconst_1)) 
                                      (55 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (58 (aconst_null)) 
                                      (59 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (62 (astore 7)) 
                                      (64 (getstatic (fieldCP "const__4" "clojure.pprint$consume" (class "clojure.lang.Var")))) 
                                      (67 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (70 (checkcast (class "clojure.lang.IFn"))) 
                                      (73 (aload 6)) 
                                      (75 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (80 (dup)) 
                                      (81 (ifnull 113)) ;;to TAG_0
                                      (84 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (87 (if_acmpeq 114)) ;;to TAG_1
                                      (90 (iconst_2)) 
                                      (91 (anewarray (class "java.lang.Object"))) 
                                      (94 (dup)) 
                                      (95 (iconst_0)) 
                                      (96 (aload 4)) 
                                      (98 (aastore)) 
                                      (99 (dup)) 
                                      (100 (iconst_1)) 
                                      (101 (aload 7)) 
                                      (103 (aconst_null)) 
                                      (104 (astore 7)) 
                                      (106 (aastore)) 
                                      (107 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (110 (goto 146))  ;;to TAG_2
                                      (113 (pop)) ;;at TAG_0
                                      (114 (aload 7)) ;;at TAG_1
                                      (116 (aconst_null)) 
                                      (117 (astore 7)) 
                                      (119 (getstatic (fieldCP "const__5" "clojure.pprint$consume" (class "clojure.lang.Var")))) 
                                      (122 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (125 (checkcast (class "clojure.lang.IFn"))) 
                                      (128 (aload 4)) 
                                      (130 (aload 6)) 
                                      (132 (aconst_null)) 
                                      (133 (astore 6)) 
                                      (135 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (140 (astore 4)) 
                                      (142 (astore_3)) 
                                      (143 (goto 9)) ;;to TAG_3
                                      (146 (areturn)) ;;at TAG_2
                                      (endofcode 147))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$consume-class-table*
  (make-static-class-decls 
   *clojure.pprint$consume*))

(defconst *package-name-map* 
  ("clojure.pprint$consume" . "clojure"))

