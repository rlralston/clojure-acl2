; NetworkInterface$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.NetworkInterface$1*
 (make-class-def
      '(class "java.net.NetworkInterface$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" int (accessflags  *class*  *private* ) -1)
                        (field "val$netifs" (array (class "java.net.NetworkInterface")) (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.net.NetworkInterface")))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface")))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "i" "java.net.NetworkInterface$1" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.net.NetworkInterface"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface"))))) 
                                      (4 (ifnull 38))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.net.NetworkInterface$1" int))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface"))))) 
                                      (15 (arraylength)) 
                                      (16 (if_icmpge 38))  ;;to TAG_0
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface"))))) 
                                      (23 (aload_0)) 
                                      (24 (dup)) 
                                      (25 (getfield (fieldCP "i" "java.net.NetworkInterface$1" int))) 
                                      (28 (dup_x1)) 
                                      (29 (iconst_1)) 
                                      (30 (iadd)) 
                                      (31 (putfield (fieldCP "i" "java.net.NetworkInterface$1" int))) 
                                      (34 (aaload)) 
                                      (35 (astore_1)) 
                                      (36 (aload_1)) 
                                      (37 (areturn)) 
                                      (38 (new (class "java.util.NoSuchElementException"))) ;;at TAG_0
                                      (41 (dup)) 
                                      (42 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (45 (athrow)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface"))))) 
                                      (4 (ifnull 23))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "i" "java.net.NetworkInterface$1" int))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "val$netifs" "java.net.NetworkInterface$1" (array (class "java.net.NetworkInterface"))))) 
                                      (15 (arraylength)) 
                                      (16 (if_icmpge 23))  ;;to TAG_0
                                      (19 (iconst_1)) 
                                      (20 (goto 24)) ;;to TAG_1
                                      (23 (iconst_0)) ;;at TAG_0
                                      (24 (ireturn)) ;;at TAG_1
                                      (endofcode 25))
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
					(methodCP "nextElement" "java.net.NetworkInterface$1" () (class "java.net.NetworkInterface"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *NetworkInterface$1-class-table*
  (make-static-class-decls 
   *java.net.NetworkInterface$1*))

(defconst *package-name-map* 
  ("java.net.NetworkInterface$1" . "java.net"))
