; Short$ShortCache-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.Short$ShortCache*
 (make-class-def
      '(class "java.lang.Short$ShortCache"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "cache" (array (class "java.lang.Short")) (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
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
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 44)
                                   (parsedcode
                                      (0 (sipush 256)) 
                                      (3 (anewarray (class "java.lang.Short"))) 
                                      (6 (putstatic (fieldCP "cache" "java.lang.Short$ShortCache" (array (class "java.lang.Short"))))) 
                                      (9 (iconst_0)) 
                                      (10 (istore_0)) 
                                      (11 (iload_0)) ;;at TAG_1
                                      (12 (getstatic (fieldCP "cache" "java.lang.Short$ShortCache" (array (class "java.lang.Short"))))) 
                                      (15 (arraylength)) 
                                      (16 (if_icmpge 43))  ;;to TAG_0
                                      (19 (getstatic (fieldCP "cache" "java.lang.Short$ShortCache" (array (class "java.lang.Short"))))) 
                                      (22 (iload_0)) 
                                      (23 (new (class "java.lang.Short"))) 
                                      (26 (dup)) 
                                      (27 (iload_0)) 
                                      (28 (sipush 128)) 
                                      (31 (isub)) 
                                      (32 (i2s)) 
                                      (33 (invokespecial (methodCP "<init>" "java.lang.Short" (short) void))) 
                                      (36 (aastore)) 
                                      (37 (iinc 0 1)) 
                                      (40 (goto 11)) ;;to TAG_1
                                      (43 (return)) ;;at TAG_0
                                      (endofcode 44))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Short$ShortCache-class-table*
  (make-static-class-decls 
   *java.lang.Short$ShortCache*))

(defconst *package-name-map* 
  ("java.lang.Short$ShortCache" . "java.lang"))
