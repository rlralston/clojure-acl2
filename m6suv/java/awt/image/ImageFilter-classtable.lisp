; ImageFilter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.ImageFilter*
 (make-class-def
      '(class "java.awt.image.ImageFilter"
            "java.lang.Object"
            (constant_pool
                        (STRING  "filters"))
            (fields
                        (field "consumer" (class "java.awt.image.ImageConsumer") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "getFilterInstance"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . (class "java.awt.image.ImageFilter"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "clone" "java.awt.image.ImageFilter" () (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.awt.image.ImageFilter")))
                                      (7 (astore_2))
                                      (8 (aload_2))
                                      (9 (aload_1))
                                      (10 (putfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (13 (aload_2))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setDimensions"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (iload_1))
                                      (5 (iload_2))
                                      (6 (invokeinterface
					(methodCP "setDimensions" "java.awt.image.ImageConsumer" (int int) void) 3))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setProperties"
                              (parameters (class "java.util.Hashtable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 82)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "clone" "java.util.Hashtable" () (class "java.lang.Object")))) 
                                      (4 (checkcast (class "java.util.Hashtable"))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (ldc 0)) ;;STRING:: "filters"
                                      (11 (invokevirtual (methodCP "get" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (14 (astore_3)) 
                                      (15 (aload_3)) 
                                      (16 (ifnonnull 33))  ;;to TAG_0
                                      (19 (aload_2)) 
                                      (20 (ldc 0)) ;;STRING:: "filters"
                                      (22 (aload_0)) 
                                      (23 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (26 (invokevirtual (methodCP "put" "java.util.Hashtable" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (29 (pop)) 
                                      (30 (goto 71)) ;;to TAG_1
                                      (33 (aload_3)) ;;at TAG_0
                                      (34 (instanceof (class "java.lang.String"))) 
                                      (37 (ifeq 71)) ;;to TAG_1
                                      (40 (aload_2)) 
                                      (41 (ldc 0)) ;;STRING:: "filters"
                                      (43 (new (class "java.lang.StringBuilder"))) 
                                      (46 (dup)) 
                                      (47 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (50 (aload_3)) 
                                      (51 (checkcast (class "java.lang.String"))) 
                                      (54 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (57 (aload_0)) 
                                      (58 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (61 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (64 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (67 (invokevirtual (methodCP "put" "java.util.Hashtable" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (70 (pop)) 
                                      (71 (aload_0)) ;;at TAG_1
                                      (72 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (75 (aload_2)) 
                                      (76 (invokeinterface (methodCP "setProperties" "java.awt.image.ImageConsumer" ((class "java.util.Hashtable")) void) 2)) 
                                      (81 (return)) 
                                      (endofcode 82))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setColorModel"
                              (parameters (class "java.awt.image.ColorModel"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "setColorModel" "java.awt.image.ImageConsumer" ((class "java.awt.image.ColorModel")) void) 2))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setHints"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (iload_1))
                                      (5 (invokeinterface
					(methodCP "setHints" "java.awt.image.ImageConsumer" (int) void) 2))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 9) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (iload_1))
                                      (5 (iload_2))
                                      (6 (iload_3))
                                      (7 (iload 4))
                                      (9 (aload 5))
                                      (11 (aload 6))
                                      (13 (iload 7))
                                      (15 (iload 8))
                                      (17 (invokeinterface
					(methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array byte) int int) void) 9))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array int) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 9) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (iload_1))
                                      (5 (iload_2))
                                      (6 (iload_3))
                                      (7 (iload 4))
                                      (9 (aload 5))
                                      (11 (aload 6))
                                      (13 (iload 7))
                                      (15 (iload 8))
                                      (17 (invokeinterface
					(methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array int) int int) void) 9))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "imageComplete"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "consumer" "java.awt.image.ImageFilter" (class "java.awt.image.ImageConsumer"))))
                                      (4 (iload_1))
                                      (5 (invokeinterface
					(methodCP "imageComplete" "java.awt.image.ImageConsumer" (int) void) 2))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "resendTopDownLeftRight"
                              (parameters (class "java.awt.image.ImageProducer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (invokeinterface
					(methodCP "requestTopDownLeftRightResend" "java.awt.image.ImageProducer" ((class "java.awt.image.ImageConsumer")) void) 2))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (areturn)) ;;at TAG_1
                                      (5 (astore_1)) ;;at TAG_2
                                      (6 (new (class "java.lang.InternalError"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (13 (athrow)) 
                                      (endofcode 14))
                                   (Exceptions 
                                     (handler 0 4  5 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap ))))
            (interfaces "java.awt.image.ImageConsumer" "java.lang.Cloneable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ImageFilter-class-table*
  (make-static-class-decls 
   *java.awt.image.ImageFilter*))

(defconst *package-name-map* 
  ("java.awt.image.ImageFilter" . "java.awt.image"))

