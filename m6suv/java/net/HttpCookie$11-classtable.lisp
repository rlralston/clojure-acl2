; HttpCookie$11-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.HttpCookie$11*
 (make-class-def
      '(class "java.net.HttpCookie$11"
            "java.lang.Object"
            (constant_pool
                        (LONG -1))
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
                                   (max_stack . 4) (max_locals . 4) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getMaxAge" "java.net.HttpCookie" () long))) 
                                      (4 (ldc2_w 0)) ;; LONG:: "-1"
                                      (7 (lcmp)) 
                                      (8 (ifne 20))  ;;to TAG_0
                                      (11 (aload_1)) 
                                      (12 (aload_1)) 
                                      (13 (aload_3)) 
                                      (14 (invokestatic (methodCP "access$000" "java.net.HttpCookie" ((class "java.net.HttpCookie") (class "java.lang.String")) long))) 
                                      (17 (invokevirtual (methodCP "setMaxAge" "java.net.HttpCookie" (long) void))) 
                                      (20 (return)) ;;at TAG_0
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.net.HttpCookie$CookieAttributeAssignor")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *HttpCookie$11-class-table*
  (make-static-class-decls 
   *java.net.HttpCookie$11*))

(defconst *package-name-map* 
  ("java.net.HttpCookie$11" . "java.net"))

