; Collections$SetFromMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$SetFromMap*
 (make-class-def
      '(class "java.util.Collections$SetFromMap"
            "java.util.AbstractSet"
            (constant_pool
                        (LONG 2454657854757543876)
                        (STRING  "Map is non-empty"))
            (fields
                        (field "m" (class "java.util.Map") (accessflags  *class*  *final*  *private* ) -1)
                        (field "s" (class "java.util.Set") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 39)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.AbstractSet" () void))) 
                                      (4 (aload_1)) 
                                      (5 (invokeinterface (methodCP "isEmpty" "java.util.Map" () boolean) 1)) 
                                      (10 (ifne 23))  ;;to TAG_0
                                      (13 (new (class "java.lang.IllegalArgumentException"))) 
                                      (16 (dup)) 
                                      (17 (ldc 1)) ;;STRING:: "Map is non-empty"
                                      (19 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (aload_1)) 
                                      (25 (putfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map")))) 
                                      (28 (aload_0)) 
                                      (29 (aload_1)) 
                                      (30 (invokeinterface (methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (35 (putfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set")))) 
                                      (38 (return)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "clear" "java.util.Map" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "size" "java.util.Map" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "isEmpty" "java.util.Map" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "containsKey" "java.util.Map" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map")))) 
                                      (4 (aload_1)) 
                                      (5 (invokeinterface (methodCP "remove" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (10 (ifnull 17))  ;;to TAG_0
                                      (13 (iconst_1)) 
                                      (14 (goto 18)) ;;to TAG_1
                                      (17 (iconst_0)) ;;at TAG_0
                                      (18 (ireturn)) ;;at TAG_1
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map")))) 
                                      (4 (aload_1)) 
                                      (5 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (8 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (13 (ifnonnull 20))  ;;to TAG_0
                                      (16 (iconst_1)) 
                                      (17 (goto 21)) ;;to TAG_1
                                      (20 (iconst_0)) ;;at TAG_0
                                      (21 (ireturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (invokeinterface
					(methodCP "iterator" "java.util.Set" () (class "java.util.Iterator")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters )
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (invokeinterface
					(methodCP "toArray" "java.util.Set" () (array (class "java.lang.Object"))) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "toArray" "java.util.Set" ((array (class "java.lang.Object"))) (array (class "java.lang.Object"))) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (invokevirtual
					(methodCP "toString" "java.lang.Object" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (invokeinterface
					(methodCP "hashCode" "java.util.Set" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpeq 18)) ;;to TAG_0
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set")))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "equals" "java.util.Set" ((class "java.lang.Object")) boolean) 2)) 
                                      (15 (ifeq 22)) ;;to TAG_1
                                      (18 (iconst_1)) ;;at TAG_0
                                      (19 (goto 23))  ;;to TAG_2
                                      (22 (iconst_0)) ;;at TAG_1
                                      (23 (ireturn)) ;;at TAG_2
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "containsAll"
                              (parameters (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "containsAll" "java.util.Set" ((class "java.util.Collection")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeAll"
                              (parameters (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "removeAll" "java.util.Set" ((class "java.util.Collection")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "retainAll"
                              (parameters (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "retainAll" "java.util.Set" ((class "java.util.Collection")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "defaultReadObject" "java.io.ObjectInputStream" () void)))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "m" "java.util.Collections$SetFromMap" (class "java.util.Map"))))
                                      (9 (invokeinterface
					(methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1))
                                      (14 (putfield (fieldCP "s" "java.util.Collections$SetFromMap" (class "java.util.Set"))))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Set" "java.io.Serializable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$SetFromMap-class-table*
  (make-static-class-decls 
   *java.util.Collections$SetFromMap*))

(defconst *package-name-map* 
  ("java.util.Collections$SetFromMap" . "java.util"))

