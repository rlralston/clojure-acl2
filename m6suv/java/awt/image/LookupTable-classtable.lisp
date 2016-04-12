; LookupTable-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.LookupTable*
 (make-class-def
      '(class "java.awt.image.LookupTable"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Offset must be greater than 0")
                        (STRING  "Number of components must  be at least 1"))
            (fields
                        (field "numComponents" int (accessflags  *class* ) -1)
                        (field "offset" int (accessflags  *class* ) -1)
                        (field "numEntries" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 44)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (iload_1)) 
                                      (5 (ifge 18))  ;;to TAG_0
                                      (8 (new (class "java.lang.IllegalArgumentException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 0)) ;;STRING:: "Offset must be greater than 0"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (iload_2)) ;;at TAG_0
                                      (19 (iconst_1)) 
                                      (20 (if_icmpge 33)) ;;to TAG_1
                                      (23 (new (class "java.lang.IllegalArgumentException"))) 
                                      (26 (dup)) 
                                      (27 (ldc 1)) ;;STRING:: "Number of components must  be at least 1"
                                      (29 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (32 (athrow)) 
                                      (33 (aload_0)) ;;at TAG_1
                                      (34 (iload_2)) 
                                      (35 (putfield (fieldCP "numComponents" "java.awt.image.LookupTable" int))) 
                                      (38 (aload_0)) 
                                      (39 (iload_1)) 
                                      (40 (putfield (fieldCP "offset" "java.awt.image.LookupTable" int))) 
                                      (43 (return)) 
                                      (endofcode 44))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNumComponents"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "numComponents" "java.awt.image.LookupTable" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOffset"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "offset" "java.awt.image.LookupTable" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lookupPixel"
                              (parameters (array int) (array int))
                              (returntype . (array int))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LookupTable-class-table*
  (make-static-class-decls 
   *java.awt.image.LookupTable*))

(defconst *package-name-map* 
  ("java.awt.image.LookupTable" . "java.awt.image"))
