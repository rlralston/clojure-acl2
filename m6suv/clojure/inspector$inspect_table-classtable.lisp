; inspector$inspect_table-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.inspector$inspect_table*
 (make-class-def
      '(class "clojure.inspector$inspect_table"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.inspector")
                        (STRING  "old-table-model")
                        (LONG 400)
                        (LONG 600)
                        (STRING  "Clojure Inspector"))
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
                                   (max_stack . 2) (max_locals . 0) (code_length . 32)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.inspector"
                                      (2 (ldc 1))         ;;STRING:: "old-table-model"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.inspector$inspect_table" (class "clojure.lang.Var"))))
                                      (13 (ldc2_w 2))     ;; LONG:: "400"
                                      (16 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (19 (putstatic (fieldCP "const__1" "clojure.inspector$inspect_table" (class "java.lang.Object"))))
                                      (22 (ldc2_w 3))     ;; LONG:: "600"
                                      (25 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (28 (putstatic (fieldCP "const__2" "clojure.inspector$inspect_table" (class "java.lang.Object"))))
                                      (31 (return))
                                      (endofcode 32))
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
                                   (max_stack . 8) (max_locals . 3) (code_length . 104)
                                   (parsedcode
                                      (0 (new (class "javax.swing.JFrame")))
                                      (3 (dup))
                                      (4 (ldc 4))         ;;STRING:: "Clojure Inspector"
                                      (6 (checkcast (class "java.lang.String")))
                                      (9 (invokespecial
					(methodCP "<init>" "javax.swing.JFrame" ((class "java.lang.String")) void)))
                                      (12 (astore_2))
                                      (13 (aload_2))
                                      (14 (checkcast (class "java.awt.Container")))
                                      (17 (new (class "javax.swing.JScrollPane")))
                                      (20 (dup))
                                      (21 (new (class "javax.swing.JTable")))
                                      (24 (dup))
                                      (25 (getstatic (fieldCP "const__0" "clojure.inspector$inspect_table" (class "clojure.lang.Var"))))
                                      (28 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (31 (checkcast (class "clojure.lang.IFn")))
                                      (34 (aload_1))
                                      (35 (aconst_null))
                                      (36 (astore_1))
                                      (37 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (42 (checkcast (class "javax.swing.table.TableModel")))
                                      (45 (invokespecial
					(methodCP "<init>" "javax.swing.JTable" ((class "javax.swing.table.TableModel")) void)))
                                      (48 (checkcast (class "java.awt.Component")))
                                      (51 (invokespecial
					(methodCP "<init>" "javax.swing.JScrollPane" ((class "java.awt.Component")) void)))
                                      (54 (checkcast (class "java.awt.Component")))
                                      (57 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (60 (pop))
                                      (61 (aload_2))
                                      (62 (checkcast (class "java.awt.Window")))
                                      (65 (ldc2_w 2))     ;; LONG:: "400"
                                      (68 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (71 (ldc2_w 3))     ;; LONG:: "600"
                                      (74 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (77 (invokevirtual
					(methodCP "setSize" "java.awt.Window" (int int) void)))
                                      (80 (aconst_null))
                                      (81 (pop))
                                      (82 (aload_2))
                                      (83 (checkcast (class "java.awt.Window")))
                                      (86 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (89 (checkcast (class "java.lang.Boolean")))
                                      (92 (invokevirtual
					(methodCP "booleanValue" "java.lang.Boolean" () boolean)))
                                      (95 (invokevirtual
					(methodCP "setVisible" "java.awt.Window" (boolean) void)))
                                      (98 (aconst_null))
                                      (99 (pop))
                                      (100 (aload_2))
                                      (101 (aconst_null))
                                      (102 (astore_2))
                                      (103 (areturn))
                                      (endofcode 104))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *inspector$inspect_table-class-table*
  (make-static-class-decls 
   *clojure.inspector$inspect_table*))

(defconst *package-name-map* 
  ("clojure.inspector$inspect_table" . "clojure"))

