; pprint$column_writer$fn__7358$fn__7362$fn__7363-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363*
 (make-class-def
      '(class "clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "="))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 22)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363" (class "clojure.lang.Var"))))
                                      (13 (bipush 10))
                                      (15 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (18 (putstatic (fieldCP "const__1" "clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363" (class "java.lang.Object"))))
                                      (21 (return))
                                      (endofcode 22))
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (getstatic (fieldCP "const__1" "clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363" (class "java.lang.Object")))) 
                                      (6 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (9 (ifeq 18))  ;;to TAG_0
                                      (12 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (15 (goto 21)) ;;to TAG_1
                                      (18 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (21 (areturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$column_writer$fn__7358$fn__7362$fn__7363-class-table*
  (make-static-class-decls 
   *clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363*))

(defconst *package-name-map* 
  ("clojure.pprint$column_writer$fn__7358$fn__7362$fn__7363" . "clojure"))

