; HttpCookie$10-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.HttpCookie$10*
 (make-class-def
      '(class "java.net.HttpCookie$10"
            "java.lang.Object"
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "assign"
                              (parameters (class "java.net.HttpCookie") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_3)) ;;at TAG_1
                                      (1 (invokestatic (methodCP "parseInt" "java.lang.Integer" ((class "java.lang.String")) int))) 
                                      (4 (istore 4)) 
                                      (6 (aload_1)) 
                                      (7 (iload 4)) 
                                      (9 (invokevirtual (methodCP "setVersion" "java.net.HttpCookie" (int) void))) 
                                      (12 (goto 17)) ;;to TAG_0;;at TAG_2
                                      (15 (astore 4)) ;;at TAG_3
                                      (17 (return)) ;;at TAG_0
                                      (endofcode 18))
                                   (Exceptions 
                                     (handler 0 12  15 (class "java.lang.NumberFormatException")))
                                   (StackMap ))))
            (interfaces "java.net.HttpCookie$CookieAttributeAssignor")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *HttpCookie$10-class-table*
  (make-static-class-decls 
   *java.net.HttpCookie$10*))

(defconst *package-name-map* 
  ("java.net.HttpCookie$10" . "java.net"))
