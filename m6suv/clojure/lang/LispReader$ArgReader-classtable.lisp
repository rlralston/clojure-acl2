; LispReader$ArgReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$ArgReader*
 (make-class-def
      '(class "clojure.lang.LispReader$ArgReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "arg literal must be %, %& or %integer"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 118)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.PushbackReader"))) 
                                      (4 (astore_3)) 
                                      (5 (getstatic (fieldCP "ARG_ENV" "clojure.lang.LispReader" (class "clojure.lang.Var")))) 
                                      (8 (invokevirtual (methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (11 (ifnonnull 24)) ;;to TAG_0
                                      (14 (aload_3)) 
                                      (15 (bipush 37)) 
                                      (17 (invokestatic (methodCP "access$100" "clojure.lang.LispReader" ((class "java.io.PushbackReader") char) (class "java.lang.String")))) 
                                      (20 (invokestatic (methodCP "access$200" "clojure.lang.LispReader" ((class "java.lang.String")) (class "java.lang.Object")))) 
                                      (23 (areturn)) 
                                      (24 (aload_3)) ;;at TAG_0
                                      (25 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (28 (istore 4)) 
                                      (30 (aload_3)) 
                                      (31 (iload 4)) 
                                      (33 (invokestatic (methodCP "unread" "clojure.lang.LispReader" ((class "java.io.PushbackReader") int) void))) 
                                      (36 (iload 4)) 
                                      (38 (iconst_m1)) 
                                      (39 (if_icmpeq 58)) ;;to TAG_1
                                      (42 (iload 4)) 
                                      (44 (invokestatic (methodCP "isWhitespace" "clojure.lang.LispReader" (int) boolean))) 
                                      (47 (ifne 58)) ;;to TAG_1
                                      (50 (iload 4)) 
                                      (52 (invokestatic (methodCP "access$300" "clojure.lang.LispReader" (int) boolean))) 
                                      (55 (ifeq 63))  ;;to TAG_2
                                      (58 (iconst_1)) ;;at TAG_1
                                      (59 (invokestatic (methodCP "registerArg" "clojure.lang.LispReader" (int) (class "clojure.lang.Symbol")))) 
                                      (62 (areturn)) 
                                      (63 (aload_3)) ;;at TAG_2
                                      (64 (iconst_1)) 
                                      (65 (aconst_null)) 
                                      (66 (iconst_1)) 
                                      (67 (invokestatic (methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object")))) 
                                      (70 (astore 5)) 
                                      (72 (aload 5)) 
                                      (74 (getstatic (fieldCP "_AMP_" "clojure.lang.Compiler" (class "clojure.lang.Symbol")))) 
                                      (77 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (80 (ifeq 88)) ;;to TAG_3
                                      (83 (iconst_m1)) 
                                      (84 (invokestatic (methodCP "registerArg" "clojure.lang.LispReader" (int) (class "clojure.lang.Symbol")))) 
                                      (87 (areturn)) 
                                      (88 (aload 5)) ;;at TAG_3
                                      (90 (instanceof (class "java.lang.Number"))) 
                                      (93 (ifne 106)) ;;to TAG_4
                                      (96 (new (class "java.lang.IllegalStateException"))) 
                                      (99 (dup)) 
                                      (100 (ldc 0)) ;;STRING:: "arg literal must be %, %& or %integer"
                                      (102 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (105 (athrow)) 
                                      (106 (aload 5)) ;;at TAG_4
                                      (108 (checkcast (class "java.lang.Number"))) 
                                      (111 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (114 (invokestatic (methodCP "registerArg" "clojure.lang.LispReader" (int) (class "clojure.lang.Symbol")))) 
                                      (117 (areturn)) 
                                      (endofcode 118))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$ArgReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$ArgReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$ArgReader" . "clojure.lang"))

