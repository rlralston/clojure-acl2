; core$root_resource-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$root_resource*
 (make-class-def
      '(class "clojure.core$root_resource"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "str")
                        (STRING  "name"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 59)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "str"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$root_resource" (class "clojure.lang.Var"))))
                                      (13 (bipush 47))
                                      (15 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (18 (putstatic (fieldCP "const__1" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (21 (ldc 0))        ;;STRING:: "clojure.core"
                                      (23 (ldc 2))        ;;STRING:: "name"
                                      (25 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (28 (checkcast (class "clojure.lang.Var")))
                                      (31 (putstatic (fieldCP "const__2" "clojure.core$root_resource" (class "clojure.lang.Var"))))
                                      (34 (bipush 45))
                                      (36 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (39 (putstatic (fieldCP "const__3" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (42 (bipush 95))
                                      (44 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (47 (putstatic (fieldCP "const__4" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (50 (bipush 46))
                                      (52 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (55 (putstatic (fieldCP "const__5" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (58 (return))
                                      (endofcode 59))
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
                                   (max_stack . 5) (max_locals . 2) (code_length . 83)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$root_resource" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (12 (getstatic (fieldCP "const__2" "clojure.core$root_resource" (class "clojure.lang.Var"))))
                                      (15 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (aload_1))
                                      (22 (aconst_null))
                                      (23 (astore_1))
                                      (24 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (29 (checkcast (class "java.lang.String")))
                                      (32 (getstatic (fieldCP "const__3" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (35 (checkcast (class "java.lang.Character")))
                                      (38 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (41 (getstatic (fieldCP "const__4" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (44 (checkcast (class "java.lang.Character")))
                                      (47 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (50 (invokevirtual
					(methodCP "replace" "java.lang.String" (char char) (class "java.lang.String"))))
                                      (53 (checkcast (class "java.lang.String")))
                                      (56 (getstatic (fieldCP "const__5" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (59 (checkcast (class "java.lang.Character")))
                                      (62 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (65 (getstatic (fieldCP "const__1" "clojure.core$root_resource" (class "java.lang.Object"))))
                                      (68 (checkcast (class "java.lang.Character")))
                                      (71 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (74 (invokevirtual
					(methodCP "replace" "java.lang.String" (char char) (class "java.lang.String"))))
                                      (77 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (82 (areturn))
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$root_resource-class-table*
  (make-static-class-decls 
   *clojure.core$root_resource*))

(defconst *package-name-map* 
  ("clojure.core$root_resource" . "clojure"))
