; pprint$capitalize_word_writer$fn__7840$fn__7844-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$capitalize_word_writer$fn__7840$fn__7844*
 (make-class-def
      '(class "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "ref-set")
                        (STRING  "nth")
                        (STRING  "dec")
                        (STRING  "count"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "last_was_whitespace_QMARK_" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "s" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "ref-set"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "dec"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "count"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "s" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 61)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "s" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "java.lang.Object")))) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "s" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" (class "java.lang.Object")))) 
                                      (21 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (24 (i2l)) 
                                      (25 (invokestatic (methodCP "dec" "clojure.lang.Numbers" (long) long))) 
                                      (28 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (31 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int) (class "java.lang.Object")))) 
                                      (34 (checkcast (class "java.lang.Character"))) 
                                      (37 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (40 (invokestatic (methodCP "isWhitespace" "java.lang.Character" (char) boolean))) 
                                      (43 (ifeq 52))  ;;to TAG_0
                                      (46 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (49 (goto 55)) ;;to TAG_1
                                      (52 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (55 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_1
                                      (60 (areturn)) 
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$capitalize_word_writer$fn__7840$fn__7844-class-table*
  (make-static-class-decls 
   *clojure.pprint$capitalize_word_writer$fn__7840$fn__7844*))

(defconst *package-name-map* 
  ("clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" . "clojure"))

