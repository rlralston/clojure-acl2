; java_awt_MenuShortcut_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_awt_MenuShortcut_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_awt_MenuShortcut_PersistenceDelegate"
            "java.beans.PersistenceDelegate"
            (constant_pool
                        (STRING  "new"))
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
					(methodCP "<init>" "java.beans.PersistenceDelegate" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mutatesTo"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (invokevirtual
					(methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "instantiate"
                              (parameters (class "java.lang.Object") (class "java.beans.Encoder"))
                              (returntype . (class "java.beans.Expression"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 11) (max_locals . 4) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.awt.MenuShortcut")))
                                      (4 (astore_3))
                                      (5 (new (class "java.beans.Expression")))
                                      (8 (dup))
                                      (9 (aload_1))
                                      (10 (aload_3))
                                      (11 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (14 (ldc 0))        ;;STRING:: "new"
                                      (16 (iconst_2))
                                      (17 (anewarray (class "java.lang.Object")))
                                      (20 (dup))
                                      (21 (iconst_0))
                                      (22 (new (class "java.lang.Integer")))
                                      (25 (dup))
                                      (26 (aload_3))
                                      (27 (invokevirtual
					(methodCP "getKey" "java.awt.MenuShortcut" () int)))
                                      (30 (invokespecial
					(methodCP "<init>" "java.lang.Integer" (int) void)))
                                      (33 (aastore))
                                      (34 (dup))
                                      (35 (iconst_1))
                                      (36 (aload_3))
                                      (37 (invokevirtual
					(methodCP "usesShiftModifier" "java.awt.MenuShortcut" () boolean)))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean"))))
                                      (43 (aastore))
                                      (44 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (47 (areturn))
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *java_awt_MenuShortcut_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_awt_MenuShortcut_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_awt_MenuShortcut_PersistenceDelegate" . "java.beans"))

