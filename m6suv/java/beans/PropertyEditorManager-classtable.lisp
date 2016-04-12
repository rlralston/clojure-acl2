; PropertyEditorManager-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.PropertyEditorManager*
 (make-class-def
      '(class "java.beans.PropertyEditorManager"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "FINDER_KEY" (class "java.lang.Object") (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "registerEditor"
                              (parameters (class "java.lang.Class") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_2)) 
                                      (4 (aload_2)) 
                                      (5 (ifnull 12))  ;;to TAG_0
                                      (8 (aload_2)) 
                                      (9 (invokevirtual (methodCP "checkPropertiesAccess" "java.lang.SecurityManager" () void))) 
                                      (12 (invokestatic (methodCP "getFinder" "java.beans.PropertyEditorManager" () (class "com.sun.beans.finder.PropertyEditorFinder")))) ;;at TAG_0
                                      (15 (aload_0)) 
                                      (16 (aload_1)) 
                                      (17 (invokevirtual (methodCP "register" "com.sun.beans.finder.PropertyEditorFinder" ((class "java.lang.Class") (class "java.lang.Class")) void))) 
                                      (20 (return)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "findEditor"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.beans.PropertyEditor"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getFinder" "java.beans.PropertyEditorManager" () (class "com.sun.beans.finder.PropertyEditorFinder"))))
                                      (3 (aload_0))
                                      (4 (invokevirtual
					(methodCP "find" "com.sun.beans.finder.PropertyEditorFinder" ((class "java.lang.Class")) (class "java.beans.PropertyEditor"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEditorSearchPath"
                              (parameters )
                              (returntype . (array (class "java.lang.String")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getFinder" "java.beans.PropertyEditorManager" () (class "com.sun.beans.finder.PropertyEditorFinder"))))
                                      (3 (invokevirtual
					(methodCP "getPackages" "com.sun.beans.finder.PropertyEditorFinder" () (array (class "java.lang.String")))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setEditorSearchPath"
                              (parameters (array (class "java.lang.String")))
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_1)) 
                                      (4 (aload_1)) 
                                      (5 (ifnull 12))  ;;to TAG_0
                                      (8 (aload_1)) 
                                      (9 (invokevirtual (methodCP "checkPropertiesAccess" "java.lang.SecurityManager" () void))) 
                                      (12 (invokestatic (methodCP "getFinder" "java.beans.PropertyEditorManager" () (class "com.sun.beans.finder.PropertyEditorFinder")))) ;;at TAG_0
                                      (15 (aload_0)) 
                                      (16 (invokevirtual (methodCP "setPackages" "com.sun.beans.finder.PropertyEditorFinder" ((array (class "java.lang.String"))) void))) 
                                      (19 (return)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFinder"
                              (parameters )
                              (returntype . (class "com.sun.beans.finder.PropertyEditorFinder"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (getstatic (fieldCP "FINDER_KEY" "java.beans.PropertyEditorManager" (class "java.lang.Object")))) 
                                      (8 (invokevirtual (methodCP "get" "sun.awt.AppContext" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (11 (astore_1)) 
                                      (12 (aload_1)) 
                                      (13 (instanceof (class "com.sun.beans.finder.PropertyEditorFinder"))) 
                                      (16 (ifeq 24))  ;;to TAG_0
                                      (19 (aload_1)) 
                                      (20 (checkcast (class "com.sun.beans.finder.PropertyEditorFinder"))) 
                                      (23 (areturn)) 
                                      (24 (new (class "com.sun.beans.finder.PropertyEditorFinder"))) ;;at TAG_0
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "com.sun.beans.finder.PropertyEditorFinder" () void))) 
                                      (31 (astore_2)) 
                                      (32 (aload_0)) 
                                      (33 (getstatic (fieldCP "FINDER_KEY" "java.beans.PropertyEditorManager" (class "java.lang.Object")))) 
                                      (36 (aload_2)) 
                                      (37 (invokevirtual (methodCP "put" "sun.awt.AppContext" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (40 (pop)) 
                                      (41 (aload_2)) 
                                      (42 (areturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.Object")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (7 (putstatic (fieldCP "FINDER_KEY" "java.beans.PropertyEditorManager" (class "java.lang.Object"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PropertyEditorManager-class-table*
  (make-static-class-decls 
   *java.beans.PropertyEditorManager*))

(defconst *package-name-map* 
  ("java.beans.PropertyEditorManager" . "java.beans"))
