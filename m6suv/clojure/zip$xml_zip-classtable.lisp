; zip$xml_zip-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$xml_zip*
 (make-class-def
      '(class "clojure.zip$xml_zip"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.zip")
                        (STRING  "zipper")
                        (STRING  "clojure.core")
                        (STRING  "complement")
                        (STRING  "string?")
                        (STRING  "comp")
                        (STRING  "seq")
                        (STRING  "content"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 78)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.zip"
                                      (2 (ldc 1))         ;;STRING:: "zipper"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "complement"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "string?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (39 (ldc 2))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "comp"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "seq"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (65 (aconst_null))
                                      (66 (ldc 7))        ;;STRING:: "content"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.zip$xml_zip" (class "clojure.lang.Keyword"))))
                                      (77 (return))
                                      (endofcode 78))
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
                                   (max_stack . 6) (max_locals . 2) (code_length . 68)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (29 (getstatic (fieldCP "const__3" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (32 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (35 (checkcast (class "clojure.lang.IFn")))
                                      (38 (getstatic (fieldCP "const__4" "clojure.zip$xml_zip" (class "clojure.lang.Var"))))
                                      (41 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (44 (getstatic (fieldCP "const__5" "clojure.zip$xml_zip" (class "clojure.lang.Keyword"))))
                                      (47 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (52 (new (class "clojure.zip$xml_zip$fn__6784")))
                                      (55 (dup))
                                      (56 (invokespecial
					(methodCP "<init>" "clojure.zip$xml_zip$fn__6784" () void)))
                                      (59 (aload_1))
                                      (60 (aconst_null))
                                      (61 (astore_1))
                                      (62 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (67 (areturn))
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *zip$xml_zip-class-table*
  (make-static-class-decls 
   *clojure.zip$xml_zip*))

(defconst *package-name-map* 
  ("clojure.zip$xml_zip" . "clojure"))

