; NetworkInterface$1subIFs-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.NetworkInterface$1subIFs*
 (make-class-def
      '(class "java.net.NetworkInterface$1subIFs"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" int (accessflags  *class*  *private* ) -1)
                        (field "this$0" (class "java.net.NetworkInterface") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.net.NetworkInterface"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.net.NetworkInterface$1subIFs" (class "java.net.NetworkInterface"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "i" "java.net.NetworkInterface$1subIFs" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.net.NetworkInterface"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "i" "java.net.NetworkInterface$1subIFs" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.net.NetworkInterface$1subIFs" (class "java.net.NetworkInterface")))) 
                                      (8 (invokestatic (methodCP "access$100" "java.net.NetworkInterface" ((class "java.net.NetworkInterface")) (array (class "java.net.NetworkInterface"))))) 
                                      (11 (arraylength)) 
                                      (12 (if_icmpge 35))  ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "this$0" "java.net.NetworkInterface$1subIFs" (class "java.net.NetworkInterface")))) 
                                      (19 (invokestatic (methodCP "access$100" "java.net.NetworkInterface" ((class "java.net.NetworkInterface")) (array (class "java.net.NetworkInterface"))))) 
                                      (22 (aload_0)) 
                                      (23 (dup)) 
                                      (24 (getfield (fieldCP "i" "java.net.NetworkInterface$1subIFs" int))) 
                                      (27 (dup_x1)) 
                                      (28 (iconst_1)) 
                                      (29 (iadd)) 
                                      (30 (putfield (fieldCP "i" "java.net.NetworkInterface$1subIFs" int))) 
                                      (33 (aaload)) 
                                      (34 (areturn)) 
                                      (35 (new (class "java.util.NoSuchElementException"))) ;;at TAG_0
                                      (38 (dup)) 
                                      (39 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (42 (athrow)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "i" "java.net.NetworkInterface$1subIFs" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "this$0" "java.net.NetworkInterface$1subIFs" (class "java.net.NetworkInterface")))) 
                                      (8 (invokestatic (methodCP "access$100" "java.net.NetworkInterface" ((class "java.net.NetworkInterface")) (array (class "java.net.NetworkInterface"))))) 
                                      (11 (arraylength)) 
                                      (12 (if_icmpge 19))  ;;to TAG_0
                                      (15 (iconst_1)) 
                                      (16 (goto 20)) ;;to TAG_1
                                      (19 (iconst_0)) ;;at TAG_0
                                      (20 (ireturn)) ;;at TAG_1
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "nextElement" "java.net.NetworkInterface$1subIFs" () (class "java.net.NetworkInterface"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *NetworkInterface$1subIFs-class-table*
  (make-static-class-decls 
   *java.net.NetworkInterface$1subIFs*))

(defconst *package-name-map* 
  ("java.net.NetworkInterface$1subIFs" . "java.net"))

