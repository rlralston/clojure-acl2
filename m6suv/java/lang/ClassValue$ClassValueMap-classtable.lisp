; ClassValue$ClassValueMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.ClassValue$ClassValueMap*
 (make-class-def
      '(class "java.lang.ClassValue$ClassValueMap"
            "java.util.WeakHashMap"
            (constant_pool)
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
					(methodCP "<init>" "java.util.WeakHashMap" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "preInitializeEntry"
                              (parameters (class "java.lang.ClassValue"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "containsKey" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object")) boolean))) 
                                      (5 (ifne 15))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (invokevirtual (methodCP "put" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (14 (pop)) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initializeEntry"
                              (parameters (class "java.lang.ClassValue") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (5 (astore_3)) 
                                      (6 (aload_3)) 
                                      (7 (ifnull 16))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (aload_3)) 
                                      (12 (invokevirtual (methodCP "unmaskNull" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (15 (areturn)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (aload_0)) 
                                      (19 (aload_2)) 
                                      (20 (invokevirtual (methodCP "maskNull" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (23 (invokevirtual (methodCP "put" "java.lang.ClassValue$ClassValueMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (26 (pop)) 
                                      (27 (aload_2)) 
                                      (28 (areturn)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "maskNull"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 8))  ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (goto 9)) ;;to TAG_1
                                      (8 (aload_1)) ;;at TAG_0
                                      (9 (areturn)) ;;at TAG_1
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "unmaskNull"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 9))  ;;to TAG_0
                                      (5 (aconst_null)) 
                                      (6 (goto 10)) ;;to TAG_1
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (areturn)) ;;at TAG_1
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ClassValue$ClassValueMap-class-table*
  (make-static-class-decls 
   *java.lang.ClassValue$ClassValueMap*))

(defconst *package-name-map* 
  ("java.lang.ClassValue$ClassValueMap" . "java.lang"))

