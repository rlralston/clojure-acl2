; java_awt_MenuBar_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_awt_MenuBar_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_awt_MenuBar_PersistenceDelegate"
            "java.beans.DefaultPersistenceDelegate"
            (constant_pool
                        (STRING  "add"))
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
					(methodCP "<init>" "java.beans.DefaultPersistenceDelegate" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initialize"
                              (parameters (class "java.lang.Class") (class "java.lang.Object") (class "java.lang.Object") (class "java.beans.Encoder"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 8) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (aload_2)) 
                                      (3 (aload_3)) 
                                      (4 (aload 4)) 
                                      (6 (invokespecial (methodCP "initialize" "java.beans.DefaultPersistenceDelegate" ((class "java.lang.Class") (class "java.lang.Object") (class "java.lang.Object") (class "java.beans.Encoder")) void))) 
                                      (9 (aload_2)) 
                                      (10 (checkcast (class "java.awt.MenuBar"))) 
                                      (13 (astore 5)) 
                                      (15 (aload_3)) 
                                      (16 (checkcast (class "java.awt.MenuBar"))) 
                                      (19 (astore 6)) 
                                      (21 (aload 6)) 
                                      (23 (invokevirtual (methodCP "getMenuCount" "java.awt.MenuBar" () int))) 
                                      (26 (istore 7)) 
                                      (28 (iload 7)) ;;at TAG_1
                                      (30 (aload 5)) 
                                      (32 (invokevirtual (methodCP "getMenuCount" "java.awt.MenuBar" () int))) 
                                      (35 (if_icmpge 66))  ;;to TAG_0
                                      (38 (aload_2)) 
                                      (39 (ldc 0)) ;;STRING:: "add"
                                      (41 (iconst_1)) 
                                      (42 (anewarray (class "java.lang.Object"))) 
                                      (45 (dup)) 
                                      (46 (iconst_0)) 
                                      (47 (aload 5)) 
                                      (49 (iload 7)) 
                                      (51 (invokevirtual (methodCP "getMenu" "java.awt.MenuBar" (int) (class "java.awt.Menu")))) 
                                      (54 (aastore)) 
                                      (55 (aload 4)) 
                                      (57 (invokestatic (methodCP "invokeStatement" "java.beans.java_awt_MenuBar_PersistenceDelegate" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object")) (class "java.beans.Encoder")) void))) 
                                      (60 (iinc 7 1)) 
                                      (63 (goto 28)) ;;to TAG_1
                                      (66 (return)) ;;at TAG_0
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *java_awt_MenuBar_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_awt_MenuBar_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_awt_MenuBar_PersistenceDelegate" . "java.beans"))
