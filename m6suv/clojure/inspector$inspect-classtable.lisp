; inspector$inspect-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.inspector$inspect*
 (make-class-def
      '(class "clojure.inspector$inspect"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.inspector")
                        (STRING  "list-model")
                        (STRING  "list-provider")
                        (LONG 400)
                        (STRING  "Clojure Inspector")
                        (STRING  "Back")
                        (STRING  "List")
                        (STRING  "Table")
                        (STRING  "Bean")
                        (STRING  "Line")
                        (STRING  "Bar")
                        (STRING  "Prev")
                        (STRING  "Next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 45)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.inspector"
                                      (2 (ldc 1))         ;;STRING:: "list-model"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.inspector$inspect" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.inspector"
                                      (15 (ldc 2))        ;;STRING:: "list-provider"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.inspector$inspect" (class "clojure.lang.Var"))))
                                      (26 (ldc2_w 3))     ;; LONG:: "400"
                                      (29 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (32 (putstatic (fieldCP "const__2" "clojure.inspector$inspect" (class "java.lang.Object"))))
                                      (35 (ldc2_w 3))     ;; LONG:: "400"
                                      (38 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (41 (putstatic (fieldCP "const__3" "clojure.inspector$inspect" (class "java.lang.Object"))))
                                      (44 (return))
                                      (endofcode 45))
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
                                   (max_stack . 10) (max_locals . 5) (code_length . 415)
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
                                      (17 (new (class "javax.swing.JPanel")))
                                      (20 (dup))
                                      (21 (new (class "java.awt.BorderLayout")))
                                      (24 (dup))
                                      (25 (invokespecial
					(methodCP "<init>" "java.awt.BorderLayout" () void)))
                                      (28 (checkcast (class "java.awt.LayoutManager")))
                                      (31 (invokespecial
					(methodCP "<init>" "javax.swing.JPanel" ((class "java.awt.LayoutManager")) void)))
                                      (34 (astore_3))
                                      (35 (aload_3))
                                      (36 (checkcast (class "java.awt.Container")))
                                      (39 (new (class "javax.swing.JToolBar")))
                                      (42 (dup))
                                      (43 (invokespecial
					(methodCP "<init>" "javax.swing.JToolBar" () void)))
                                      (46 (astore 4))
                                      (48 (aload 4))
                                      (50 (checkcast (class "java.awt.Container")))
                                      (53 (new (class "javax.swing.JButton")))
                                      (56 (dup))
                                      (57 (ldc 5))        ;;STRING:: "Back"
                                      (59 (checkcast (class "java.lang.String")))
                                      (62 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (65 (checkcast (class "java.awt.Component")))
                                      (68 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (71 (pop))
                                      (72 (aload 4))
                                      (74 (checkcast (class "javax.swing.JToolBar")))
                                      (77 (invokevirtual
					(methodCP "addSeparator" "javax.swing.JToolBar" () void)))
                                      (80 (aconst_null))
                                      (81 (pop))
                                      (82 (aload 4))
                                      (84 (checkcast (class "java.awt.Container")))
                                      (87 (new (class "javax.swing.JButton")))
                                      (90 (dup))
                                      (91 (ldc 6))        ;;STRING:: "List"
                                      (93 (checkcast (class "java.lang.String")))
                                      (96 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (99 (checkcast (class "java.awt.Component")))
                                      (102 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (105 (pop))
                                      (106 (aload 4))
                                      (108 (checkcast (class "java.awt.Container")))
                                      (111 (new (class "javax.swing.JButton")))
                                      (114 (dup))
                                      (115 (ldc 7))       ;;STRING:: "Table"
                                      (117 (checkcast (class "java.lang.String")))
                                      (120 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (123 (checkcast (class "java.awt.Component")))
                                      (126 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (129 (pop))
                                      (130 (aload 4))
                                      (132 (checkcast (class "java.awt.Container")))
                                      (135 (new (class "javax.swing.JButton")))
                                      (138 (dup))
                                      (139 (ldc 8))       ;;STRING:: "Bean"
                                      (141 (checkcast (class "java.lang.String")))
                                      (144 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (147 (checkcast (class "java.awt.Component")))
                                      (150 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (153 (pop))
                                      (154 (aload 4))
                                      (156 (checkcast (class "java.awt.Container")))
                                      (159 (new (class "javax.swing.JButton")))
                                      (162 (dup))
                                      (163 (ldc 9))       ;;STRING:: "Line"
                                      (165 (checkcast (class "java.lang.String")))
                                      (168 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (171 (checkcast (class "java.awt.Component")))
                                      (174 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (177 (pop))
                                      (178 (aload 4))
                                      (180 (checkcast (class "java.awt.Container")))
                                      (183 (new (class "javax.swing.JButton")))
                                      (186 (dup))
                                      (187 (ldc 10))      ;;STRING:: "Bar"
                                      (189 (checkcast (class "java.lang.String")))
                                      (192 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (195 (checkcast (class "java.awt.Component")))
                                      (198 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (201 (pop))
                                      (202 (aload 4))
                                      (204 (checkcast (class "javax.swing.JToolBar")))
                                      (207 (invokevirtual
					(methodCP "addSeparator" "javax.swing.JToolBar" () void)))
                                      (210 (aconst_null))
                                      (211 (pop))
                                      (212 (aload 4))
                                      (214 (checkcast (class "java.awt.Container")))
                                      (217 (new (class "javax.swing.JButton")))
                                      (220 (dup))
                                      (221 (ldc 11))      ;;STRING:: "Prev"
                                      (223 (checkcast (class "java.lang.String")))
                                      (226 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (229 (checkcast (class "java.awt.Component")))
                                      (232 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (235 (pop))
                                      (236 (aload 4))
                                      (238 (checkcast (class "java.awt.Container")))
                                      (241 (new (class "javax.swing.JButton")))
                                      (244 (dup))
                                      (245 (ldc 12))      ;;STRING:: "Next"
                                      (247 (checkcast (class "java.lang.String")))
                                      (250 (invokespecial
					(methodCP "<init>" "javax.swing.JButton" ((class "java.lang.String")) void)))
                                      (253 (checkcast (class "java.awt.Component")))
                                      (256 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (259 (pop))
                                      (260 (aload 4))
                                      (262 (aconst_null))
                                      (263 (astore 4))
                                      (265 (checkcast (class "java.awt.Component")))
                                      (268 (getstatic (fieldCP "NORTH" "java.awt.BorderLayout" (class "java.lang.String"))))
                                      (271 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component") (class "java.lang.Object")) void)))
                                      (274 (aconst_null))
                                      (275 (pop))
                                      (276 (aload_3))
                                      (277 (checkcast (class "java.awt.Container")))
                                      (280 (new (class "javax.swing.JScrollPane")))
                                      (283 (dup))
                                      (284 (new (class "javax.swing.JTable")))
                                      (287 (dup))
                                      (288 (getstatic (fieldCP "const__0" "clojure.inspector$inspect" (class "clojure.lang.Var"))))
                                      (291 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (294 (checkcast (class "clojure.lang.IFn")))
                                      (297 (getstatic (fieldCP "const__1" "clojure.inspector$inspect" (class "clojure.lang.Var"))))
                                      (300 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (303 (checkcast (class "clojure.lang.IFn")))
                                      (306 (aload_1))
                                      (307 (aconst_null))
                                      (308 (astore_1))
                                      (309 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (314 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (319 (checkcast (class "javax.swing.table.TableModel")))
                                      (322 (invokespecial
					(methodCP "<init>" "javax.swing.JTable" ((class "javax.swing.table.TableModel")) void)))
                                      (325 (astore 4))
                                      (327 (aload 4))
                                      (329 (checkcast (class "javax.swing.JTable")))
                                      (332 (getstatic (fieldCP "AUTO_RESIZE_LAST_COLUMN" "javax.swing.JTable" int)))
                                      (335 (invokevirtual
					(methodCP "setAutoResizeMode" "javax.swing.JTable" (int) void)))
                                      (338 (aconst_null))
                                      (339 (pop))
                                      (340 (aload 4))
                                      (342 (aconst_null))
                                      (343 (astore 4))
                                      (345 (checkcast (class "java.awt.Component")))
                                      (348 (invokespecial
					(methodCP "<init>" "javax.swing.JScrollPane" ((class "java.awt.Component")) void)))
                                      (351 (checkcast (class "java.awt.Component")))
                                      (354 (getstatic (fieldCP "CENTER" "java.awt.BorderLayout" (class "java.lang.String"))))
                                      (357 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component") (class "java.lang.Object")) void)))
                                      (360 (aconst_null))
                                      (361 (pop))
                                      (362 (aload_3))
                                      (363 (aconst_null))
                                      (364 (astore_3))
                                      (365 (checkcast (class "java.awt.Component")))
                                      (368 (invokevirtual
					(methodCP "add" "java.awt.Container" ((class "java.awt.Component")) (class "java.awt.Component"))))
                                      (371 (pop))
                                      (372 (aload_2))
                                      (373 (checkcast (class "java.awt.Window")))
                                      (376 (ldc2_w 3))    ;; LONG:: "400"
                                      (379 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (382 (ldc2_w 3))    ;; LONG:: "400"
                                      (385 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (388 (invokevirtual
					(methodCP "setSize" "java.awt.Window" (int int) void)))
                                      (391 (aconst_null))
                                      (392 (pop))
                                      (393 (aload_2))
                                      (394 (checkcast (class "java.awt.Window")))
                                      (397 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (400 (checkcast (class "java.lang.Boolean")))
                                      (403 (invokevirtual
					(methodCP "booleanValue" "java.lang.Boolean" () boolean)))
                                      (406 (invokevirtual
					(methodCP "setVisible" "java.awt.Window" (boolean) void)))
                                      (409 (aconst_null))
                                      (410 (pop))
                                      (411 (aload_2))
                                      (412 (aconst_null))
                                      (413 (astore_2))
                                      (414 (areturn))
                                      (endofcode 415))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *inspector$inspect-class-table*
  (make-static-class-decls 
   *clojure.inspector$inspect*))

(defconst *package-name-map* 
  ("clojure.inspector$inspect" . "clojure"))

