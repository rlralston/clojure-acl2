; ListResourceBundle-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.ListResourceBundle*
 (make-class-def
      '(class "java.util.ListResourceBundle"
            "java.util.ResourceBundle"
            (constant_pool)
            (fields
                        (field "lookup" (class "java.util.Map") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "handleGetObject"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (4 (ifnonnull 11))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "loadLookup" "java.util.ListResourceBundle" () void))) 
                                      (11 (aload_1)) ;;at TAG_0
                                      (12 (ifnonnull 23)) ;;to TAG_1
                                      (15 (new (class "java.lang.NullPointerException"))) 
                                      (18 (dup)) 
                                      (19 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_1
                                      (24 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (27 (aload_1)) 
                                      (28 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (33 (areturn)) 
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKeys"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (4 (ifnonnull 11)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "loadLookup" "java.util.ListResourceBundle" () void))) 
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (getfield (fieldCP "parent" "java.util.ListResourceBundle" (class "java.util.ResourceBundle")))) 
                                      (15 (astore_1)) 
                                      (16 (new (class "sun.util.ResourceBundleEnumeration"))) 
                                      (19 (dup)) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (24 (invokeinterface (methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (29 (aload_1)) 
                                      (30 (ifnull 40)) ;;to TAG_1
                                      (33 (aload_1)) 
                                      (34 (invokevirtual (methodCP "getKeys" "java.util.ResourceBundle" () (class "java.util.Enumeration")))) 
                                      (37 (goto 41))  ;;to TAG_2
                                      (40 (aconst_null)) ;;at TAG_1
                                      (41 (invokespecial (methodCP "<init>" "sun.util.ResourceBundleEnumeration" ((class "java.util.Set") (class "java.util.Enumeration")) void))) ;;at TAG_2
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "handleKeySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (4 (ifnonnull 11))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "loadLookup" "java.util.ListResourceBundle" () void))) 
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (15 (invokeinterface (methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (20 (areturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getContents"
                              (parameters )
                              (returntype . (array (array (class "java.lang.Object"))))
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "loadLookup"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 87)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (4 (ifnull 8)) ;;to TAG_0
                                      (7 (return)) 
                                      (8 (aload_0)) ;;at TAG_0
                                      (9 (invokevirtual (methodCP "getContents" "java.util.ListResourceBundle" () (array (array (class "java.lang.Object")))))) 
                                      (12 (astore_1)) 
                                      (13 (new (class "java.util.HashMap"))) 
                                      (16 (dup)) 
                                      (17 (aload_1)) 
                                      (18 (arraylength)) 
                                      (19 (invokespecial (methodCP "<init>" "java.util.HashMap" (int) void))) 
                                      (22 (astore_2)) 
                                      (23 (iconst_0)) 
                                      (24 (istore_3)) 
                                      (25 (iload_3)) ;;at TAG_4
                                      (26 (aload_1)) 
                                      (27 (arraylength)) 
                                      (28 (if_icmpge 81)) ;;to TAG_1
                                      (31 (aload_1)) 
                                      (32 (iload_3)) 
                                      (33 (aaload)) 
                                      (34 (iconst_0)) 
                                      (35 (aaload)) 
                                      (36 (checkcast (class "java.lang.String"))) 
                                      (39 (astore 4)) 
                                      (41 (aload_1)) 
                                      (42 (iload_3)) 
                                      (43 (aaload)) 
                                      (44 (iconst_1)) 
                                      (45 (aaload)) 
                                      (46 (astore 5)) 
                                      (48 (aload 4)) 
                                      (50 (ifnull 58))  ;;to TAG_2
                                      (53 (aload 5)) 
                                      (55 (ifnonnull 66)) ;;to TAG_3
                                      (58 (new (class "java.lang.NullPointerException"))) ;;at TAG_2
                                      (61 (dup)) 
                                      (62 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (65 (athrow)) 
                                      (66 (aload_2)) ;;at TAG_3
                                      (67 (aload 4)) 
                                      (69 (aload 5)) 
                                      (71 (invokevirtual (methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (74 (pop)) 
                                      (75 (iinc 3 1)) 
                                      (78 (goto 25)) ;;to TAG_4
                                      (81 (aload_0)) ;;at TAG_1
                                      (82 (aload_2)) 
                                      (83 (putfield (fieldCP "lookup" "java.util.ListResourceBundle" (class "java.util.Map")))) 
                                      (86 (return)) 
                                      (endofcode 87))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ListResourceBundle-class-table*
  (make-static-class-decls 
   *java.util.ListResourceBundle*))

(defconst *package-name-map* 
  ("java.util.ListResourceBundle" . "java.util"))
