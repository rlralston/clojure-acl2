; EdnReader$DispatchReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.EdnReader$DispatchReader*
 (make-class-def
      '(class "clojure.lang.EdnReader$DispatchReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "EOF while reading character")
                        (STRING  "No dispatch macro for: %c"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 104)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.Reader"))) 
                                      (4 (invokestatic (methodCP "read1" "clojure.lang.EdnReader" ((class "java.io.Reader")) int))) 
                                      (7 (istore 4)) 
                                      (9 (iload 4)) 
                                      (11 (iconst_m1)) 
                                      (12 (if_icmpne 21)) ;;to TAG_0
                                      (15 (ldc 0)) ;;STRING:: "EOF while reading character"
                                      (17 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (20 (athrow)) 
                                      (21 (getstatic (fieldCP "dispatchMacros" "clojure.lang.EdnReader" (array (class "clojure.lang.IFn"))))) ;;at TAG_0
                                      (24 (iload 4)) 
                                      (26 (aaload)) 
                                      (27 (astore 5)) 
                                      (29 (aload 5)) 
                                      (31 (ifnonnull 89)) ;;to TAG_1
                                      (34 (iload 4)) 
                                      (36 (invokestatic (methodCP "isLetter" "java.lang.Character" (int) boolean))) 
                                      (39 (ifeq 67))  ;;to TAG_2
                                      (42 (aload_1)) 
                                      (43 (checkcast (class "java.io.PushbackReader"))) 
                                      (46 (iload 4)) 
                                      (48 (invokestatic (methodCP "unread" "clojure.lang.EdnReader" ((class "java.io.PushbackReader") int) void))) 
                                      (51 (getstatic (fieldCP "taggedReader" "clojure.lang.EdnReader" (class "clojure.lang.IFn")))) 
                                      (54 (aload_1)) 
                                      (55 (iload 4)) 
                                      (57 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (60 (aload_3)) 
                                      (61 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (66 (areturn)) 
                                      (67 (ldc 1)) ;;at TAG_2;;STRING:: "No dispatch macro for: %c"
                                      (69 (iconst_1)) 
                                      (70 (anewarray (class "java.lang.Object"))) 
                                      (73 (dup)) 
                                      (74 (iconst_0)) 
                                      (75 (iload 4)) 
                                      (77 (i2c)) 
                                      (78 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (81 (aastore)) 
                                      (82 (invokestatic (methodCP "format" "java.lang.String" ((class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.String")))) 
                                      (85 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (88 (athrow)) 
                                      (89 (aload 5)) ;;at TAG_1
                                      (91 (aload_1)) 
                                      (92 (iload 4)) 
                                      (94 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (97 (aload_3)) 
                                      (98 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (103 (areturn)) 
                                      (endofcode 104))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *EdnReader$DispatchReader-class-table*
  (make-static-class-decls 
   *clojure.lang.EdnReader$DispatchReader*))

(defconst *package-name-map* 
  ("clojure.lang.EdnReader$DispatchReader" . "clojure.lang"))
