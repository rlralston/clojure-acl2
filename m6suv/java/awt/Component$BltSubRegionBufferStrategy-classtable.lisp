; Component$BltSubRegionBufferStrategy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.Component$BltSubRegionBufferStrategy*
 (make-class-def
      '(class "java.awt.Component$BltSubRegionBufferStrategy"
            "java.awt.Component$BltBufferStrategy"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.awt.Component") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Component") int (class "java.awt.BufferCapabilities"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Component$BltSubRegionBufferStrategy" (class "java.awt.Component"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (iload_2))
                                      (8 (aload_3))
                                      (9 (invokespecial
					(methodCP "<init>" "java.awt.Component$BltBufferStrategy" ((class "java.awt.Component") int (class "java.awt.BufferCapabilities")) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "show"
                              (parameters int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (invokevirtual
					(methodCP "showSubRegion" "java.awt.Component$BltSubRegionBufferStrategy" (int int int int) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "showIfNotLost"
                              (parameters int int int int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "contentsLost" "java.awt.Component$BltSubRegionBufferStrategy" () boolean))) 
                                      (4 (ifne 29)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (iload_1)) 
                                      (9 (iload_2)) 
                                      (10 (iload_3)) 
                                      (11 (iload 4)) 
                                      (13 (invokevirtual (methodCP "showSubRegion" "java.awt.Component$BltSubRegionBufferStrategy" (int int int int) void))) 
                                      (16 (aload_0)) 
                                      (17 (invokevirtual (methodCP "contentsLost" "java.awt.Component$BltSubRegionBufferStrategy" () boolean))) 
                                      (20 (ifne 27)) ;;to TAG_1
                                      (23 (iconst_1)) 
                                      (24 (goto 28))  ;;to TAG_2
                                      (27 (iconst_0)) ;;at TAG_1
                                      (28 (ireturn)) ;;at TAG_2
                                      (29 (iconst_0)) ;;at TAG_0
                                      (30 (ireturn)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.awt.SubRegionShowable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Component$BltSubRegionBufferStrategy-class-table*
  (make-static-class-decls 
   *java.awt.Component$BltSubRegionBufferStrategy*))

(defconst *package-name-map* 
  ("java.awt.Component$BltSubRegionBufferStrategy" . "java.awt"))

