; Pattern$6-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$6*
 (make-class-def
      '(class "java.util.regex.Pattern$6"
            "java.util.regex.Pattern$CharProperty"
            (constant_pool)
            (fields
                        (field "val$lhs" (class "java.util.regex.Pattern$CharProperty") (accessflags  *class*  *final* ) -1)
                        (field "val$rhs" (class "java.util.regex.Pattern$CharProperty") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.regex.Pattern$CharProperty") (class "java.util.regex.Pattern$CharProperty"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$lhs" "java.util.regex.Pattern$6" (class "java.util.regex.Pattern$CharProperty"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$rhs" "java.util.regex.Pattern$6" (class "java.util.regex.Pattern$CharProperty"))))
                                      (10 (aload_0))
                                      (11 (aconst_null))
                                      (12 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$CharProperty" ((class "java.util.regex.Pattern$1")) void)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSatisfiedBy"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$lhs" "java.util.regex.Pattern$6" (class "java.util.regex.Pattern$CharProperty")))) 
                                      (4 (iload_1)) 
                                      (5 (invokevirtual (methodCP "isSatisfiedBy" "java.util.regex.Pattern$CharProperty" (int) boolean))) 
                                      (8 (ifeq 26))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "val$rhs" "java.util.regex.Pattern$6" (class "java.util.regex.Pattern$CharProperty")))) 
                                      (15 (iload_1)) 
                                      (16 (invokevirtual (methodCP "isSatisfiedBy" "java.util.regex.Pattern$CharProperty" (int) boolean))) 
                                      (19 (ifeq 26))  ;;to TAG_0
                                      (22 (iconst_1)) 
                                      (23 (goto 27)) ;;to TAG_1
                                      (26 (iconst_0)) ;;at TAG_0
                                      (27 (ireturn)) ;;at TAG_1
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Pattern$6-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$6*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$6" . "java.util.regex"))
