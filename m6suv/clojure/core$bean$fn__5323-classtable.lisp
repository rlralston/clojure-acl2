; core$bean$fn__5323-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$bean$fn__5323*
 (make-class-def
      '(class "clojure.core$bean$fn__5323"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "keys"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "pmap" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "v" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "keys"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$bean$fn__5323" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                      (6 (putfield (fieldCP "pmap" "clojure.core$bean$fn__5323" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "v" "clojure.core$bean$fn__5323" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "clojure.core$bean$fn__5323$thisfn__5324")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "v" "clojure.core$bean$fn__5323" (class "java.lang.Object"))))
                                      (8 (invokespecial
					(methodCP "<init>" "clojure.core$bean$fn__5323$thisfn__5324" ((class "java.lang.Object")) void)))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (getstatic (fieldCP "const__0" "clojure.core$bean$fn__5323" (class "clojure.lang.Var"))))
                                      (17 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (20 (checkcast (class "clojure.lang.IFn")))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "pmap" "clojure.core$bean$fn__5323" (class "java.lang.Object"))))
                                      (27 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (32 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$bean$fn__5323-class-table*
  (make-static-class-decls 
   *clojure.core$bean$fn__5323*))

(defconst *package-name-map* 
  ("clojure.core$bean$fn__5323" . "clojure"))

