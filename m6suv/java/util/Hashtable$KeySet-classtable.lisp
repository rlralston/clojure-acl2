; Hashtable$KeySet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Hashtable$KeySet*
 (make-class-def
      '(class "java.util.Hashtable$KeySet"
            "java.util.AbstractSet"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.Hashtable") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Hashtable"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.AbstractSet" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable"))))
                                      (4 (iconst_0))
                                      (5 (invokestatic
					(methodCP "access$100" "java.util.Hashtable" ((class "java.util.Hashtable") int) (class "java.util.Iterator"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable"))))
                                      (4 (invokestatic
					(methodCP "access$200" "java.util.Hashtable" ((class "java.util.Hashtable")) int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "containsKey" "java.util.Hashtable" ((class "java.lang.Object")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "remove" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (8 (ifnull 15))  ;;to TAG_0
                                      (11 (iconst_1)) 
                                      (12 (goto 16)) ;;to TAG_1
                                      (15 (iconst_0)) ;;at TAG_0
                                      (16 (ireturn)) ;;at TAG_1
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$KeySet" (class "java.util.Hashtable"))))
                                      (4 (invokevirtual
					(methodCP "clear" "java.util.Hashtable" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Hashtable") (class "java.util.Hashtable$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.Hashtable$KeySet" ((class "java.util.Hashtable")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Hashtable$KeySet-class-table*
  (make-static-class-decls 
   *java.util.Hashtable$KeySet*))

(defconst *package-name-map* 
  ("java.util.Hashtable$KeySet" . "java.util"))
