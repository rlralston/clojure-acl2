; LispReader$UnquoteReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$UnquoteReader*
 (make-class-def
      '(class "clojure.lang.LispReader$UnquoteReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "EOF while reading character"))
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
                                   (max_stack . 4) (max_locals . 6) (code_length . 72)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.PushbackReader"))) 
                                      (4 (astore_3)) 
                                      (5 (aload_3)) 
                                      (6 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (9 (istore 4)) 
                                      (11 (iload 4)) 
                                      (13 (iconst_m1)) 
                                      (14 (if_icmpne 23))  ;;to TAG_0
                                      (17 (ldc 0)) ;;STRING:: "EOF while reading character"
                                      (19 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (22 (athrow)) 
                                      (23 (iload 4)) ;;at TAG_0
                                      (25 (bipush 64)) 
                                      (27 (if_icmpne 48)) ;;to TAG_1
                                      (30 (aload_3)) 
                                      (31 (iconst_1)) 
                                      (32 (aconst_null)) 
                                      (33 (iconst_1)) 
                                      (34 (invokestatic (methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object")))) 
                                      (37 (astore 5)) 
                                      (39 (getstatic (fieldCP "UNQUOTE_SPLICING" "clojure.lang.LispReader" (class "clojure.lang.Symbol")))) 
                                      (42 (aload 5)) 
                                      (44 (invokestatic (methodCP "list" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (47 (areturn)) 
                                      (48 (aload_3)) ;;at TAG_1
                                      (49 (iload 4)) 
                                      (51 (invokestatic (methodCP "unread" "clojure.lang.LispReader" ((class "java.io.PushbackReader") int) void))) 
                                      (54 (aload_3)) 
                                      (55 (iconst_1)) 
                                      (56 (aconst_null)) 
                                      (57 (iconst_1)) 
                                      (58 (invokestatic (methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object")))) 
                                      (61 (astore 5)) 
                                      (63 (getstatic (fieldCP "UNQUOTE" "clojure.lang.LispReader" (class "clojure.lang.Symbol")))) 
                                      (66 (aload 5)) 
                                      (68 (invokestatic (methodCP "list" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (71 (areturn)) 
                                      (endofcode 72))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$UnquoteReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$UnquoteReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$UnquoteReader" . "clojure.lang"))

