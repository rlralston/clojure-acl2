; Provider$ServiceKey-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Provider$ServiceKey*
 (make-class-def
      '(class "java.security.Provider$ServiceKey"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "type" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1)
                        (field "algorithm" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1)
                        (field "originalAlgorithm" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") boolean)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 39)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (putfield (fieldCP "type" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (9 (aload_0)) 
                                      (10 (aload_2)) 
                                      (11 (putfield (fieldCP "originalAlgorithm" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (14 (aload_2)) 
                                      (15 (getstatic (fieldCP "ENGLISH" "java.util.Locale" (class "java.util.Locale")))) 
                                      (18 (invokevirtual (methodCP "toUpperCase" "java.lang.String" ((class "java.util.Locale")) (class "java.lang.String")))) 
                                      (21 (astore_2)) 
                                      (22 (aload_0)) 
                                      (23 (iload_3)) 
                                      (24 (ifeq 34))  ;;to TAG_0
                                      (27 (aload_2)) 
                                      (28 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (31 (goto 35)) ;;to TAG_1
                                      (34 (aload_2)) ;;at TAG_0
                                      (35 (putfield (fieldCP "algorithm" "java.security.Provider$ServiceKey" (class "java.lang.String")))) ;;at TAG_1
                                      (38 (return)) 
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "type" "java.security.Provider$ServiceKey" (class "java.lang.String"))))
                                      (4 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "algorithm" "java.security.Provider$ServiceKey" (class "java.lang.String"))))
                                      (11 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (14 (iadd))
                                      (15 (ireturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 55)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.security.Provider$ServiceKey"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.security.Provider$ServiceKey"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "type" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (25 (aload_2)) 
                                      (26 (getfield (fieldCP "type" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (29 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (32 (ifeq 53))  ;;to TAG_2
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "algorithm" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (39 (aload_2)) 
                                      (40 (getfield (fieldCP "algorithm" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (43 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (46 (ifeq 53))  ;;to TAG_2
                                      (49 (iconst_1)) 
                                      (50 (goto 54)) ;;to TAG_3
                                      (53 (iconst_0)) ;;at TAG_2
                                      (54 (ireturn)) ;;at TAG_3
                                      (endofcode 55))
                                   (Exceptions )
                                   (StackMap )))
                        (method "matches"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "type" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (4 (aload_1)) 
                                      (5 (if_acmpne 20))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "originalAlgorithm" "java.security.Provider$ServiceKey" (class "java.lang.String")))) 
                                      (12 (aload_2)) 
                                      (13 (if_acmpne 20))  ;;to TAG_0
                                      (16 (iconst_1)) 
                                      (17 (goto 21)) ;;to TAG_1
                                      (20 (iconst_0)) ;;at TAG_0
                                      (21 (ireturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") boolean (class "java.security.Provider$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.security.Provider$ServiceKey" ((class "java.lang.String") (class "java.lang.String") boolean) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Provider$ServiceKey-class-table*
  (make-static-class-decls 
   *java.security.Provider$ServiceKey*))

(defconst *package-name-map* 
  ("java.security.Provider$ServiceKey" . "java.security"))

