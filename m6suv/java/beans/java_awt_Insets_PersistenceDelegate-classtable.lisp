; java_awt_Insets_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_awt_Insets_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_awt_Insets_PersistenceDelegate"
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
                                   (max_stack . 6) (max_locals . 5) (code_length . 68)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.awt.Insets")))
                                      (4 (astore_3))
                                      (5 (iconst_4))
                                      (6 (anewarray (class "java.lang.Object")))
                                      (9 (dup))
                                      (10 (iconst_0))
                                      (11 (aload_3))
                                      (12 (getfield (fieldCP "top" "java.awt.Insets" int)))
                                      (15 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (18 (aastore))
                                      (19 (dup))
                                      (20 (iconst_1))
                                      (21 (aload_3))
                                      (22 (getfield (fieldCP "left" "java.awt.Insets" int)))
                                      (25 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (28 (aastore))
                                      (29 (dup))
                                      (30 (iconst_2))
                                      (31 (aload_3))
                                      (32 (getfield (fieldCP "bottom" "java.awt.Insets" int)))
                                      (35 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (38 (aastore))
                                      (39 (dup))
                                      (40 (iconst_3))
                                      (41 (aload_3))
                                      (42 (getfield (fieldCP "right" "java.awt.Insets" int)))
                                      (45 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (48 (aastore))
                                      (49 (astore 4))
                                      (51 (new (class "java.beans.Expression")))
                                      (54 (dup))
                                      (55 (aload_3))
                                      (56 (aload_3))
                                      (57 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (60 (ldc 0))        ;;STRING:: "new"
                                      (62 (aload 4))
                                      (64 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (67 (areturn))
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *java_awt_Insets_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_awt_Insets_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_awt_Insets_PersistenceDelegate" . "java.beans"))
