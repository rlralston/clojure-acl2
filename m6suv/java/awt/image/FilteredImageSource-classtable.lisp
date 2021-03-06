; FilteredImageSource-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.FilteredImageSource*
 (make-class-def
      '(class "java.awt.image.FilteredImageSource"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "src" (class "java.awt.image.ImageProducer") (accessflags  *class* ) -1)
                        (field "filter" (class "java.awt.image.ImageFilter") (accessflags  *class* ) -1)
                        (field "proxies" (class "java.util.Hashtable") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.image.ImageProducer") (class "java.awt.image.ImageFilter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "src" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageProducer"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "filter" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageFilter"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addConsumer"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (4 (ifnonnull 18))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.util.Hashtable"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.util.Hashtable" () void))) 
                                      (15 (putfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "containsKey" "java.util.Hashtable" ((class "java.lang.Object")) boolean))) 
                                      (26 (ifne 58)) ;;to TAG_1
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "filter" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageFilter")))) 
                                      (33 (aload_1)) 
                                      (34 (invokevirtual (methodCP "getFilterInstance" "java.awt.image.ImageFilter" ((class "java.awt.image.ImageConsumer")) (class "java.awt.image.ImageFilter")))) 
                                      (37 (astore_2)) 
                                      (38 (aload_0)) 
                                      (39 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (42 (aload_1)) 
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "put" "java.util.Hashtable" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (47 (pop)) 
                                      (48 (aload_0)) 
                                      (49 (getfield (fieldCP "src" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageProducer")))) 
                                      (52 (aload_2)) 
                                      (53 (invokeinterface (methodCP "addConsumer" "java.awt.image.ImageProducer" ((class "java.awt.image.ImageConsumer")) void) 2)) 
                                      (58 (return)) ;;at TAG_1
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isConsumer"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (4 (ifnull 22))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (11 (aload_1)) 
                                      (12 (invokevirtual (methodCP "containsKey" "java.util.Hashtable" ((class "java.lang.Object")) boolean))) 
                                      (15 (ifeq 22))  ;;to TAG_0
                                      (18 (iconst_1)) 
                                      (19 (goto 23)) ;;to TAG_1
                                      (22 (iconst_0)) ;;at TAG_0
                                      (23 (ireturn)) ;;at TAG_1
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeConsumer"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (4 (ifnull 57))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (11 (aload_1)) 
                                      (12 (invokevirtual (methodCP "get" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (15 (checkcast (class "java.awt.image.ImageFilter"))) 
                                      (18 (astore_2)) 
                                      (19 (aload_2)) 
                                      (20 (ifnull 57))  ;;to TAG_0
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "src" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageProducer")))) 
                                      (27 (aload_2)) 
                                      (28 (invokeinterface (methodCP "removeConsumer" "java.awt.image.ImageProducer" ((class "java.awt.image.ImageConsumer")) void) 2)) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (37 (aload_1)) 
                                      (38 (invokevirtual (methodCP "remove" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (41 (pop)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (46 (invokevirtual (methodCP "isEmpty" "java.util.Hashtable" () boolean))) 
                                      (49 (ifeq 57))  ;;to TAG_0
                                      (52 (aload_0)) 
                                      (53 (aconst_null)) 
                                      (54 (putfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (57 (return)) ;;at TAG_0
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap )))
                        (method "startProduction"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (4 (ifnonnull 18))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.util.Hashtable"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.util.Hashtable" () void))) 
                                      (15 (putfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "get" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (26 (checkcast (class "java.awt.image.ImageFilter"))) 
                                      (29 (astore_2)) 
                                      (30 (aload_2)) 
                                      (31 (ifnonnull 53)) ;;to TAG_1
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "filter" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageFilter")))) 
                                      (38 (aload_1)) 
                                      (39 (invokevirtual (methodCP "getFilterInstance" "java.awt.image.ImageFilter" ((class "java.awt.image.ImageConsumer")) (class "java.awt.image.ImageFilter")))) 
                                      (42 (astore_2)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (47 (aload_1)) 
                                      (48 (aload_2)) 
                                      (49 (invokevirtual (methodCP "put" "java.util.Hashtable" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (52 (pop)) 
                                      (53 (aload_0)) ;;at TAG_1
                                      (54 (getfield (fieldCP "src" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageProducer")))) 
                                      (57 (aload_2)) 
                                      (58 (invokeinterface (methodCP "startProduction" "java.awt.image.ImageProducer" ((class "java.awt.image.ImageConsumer")) void) 2)) 
                                      (63 (return)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "requestTopDownLeftRightResend"
                              (parameters (class "java.awt.image.ImageConsumer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (4 (ifnull 31))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "proxies" "java.awt.image.FilteredImageSource" (class "java.util.Hashtable")))) 
                                      (11 (aload_1)) 
                                      (12 (invokevirtual (methodCP "get" "java.util.Hashtable" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (15 (checkcast (class "java.awt.image.ImageFilter"))) 
                                      (18 (astore_2)) 
                                      (19 (aload_2)) 
                                      (20 (ifnull 31))  ;;to TAG_0
                                      (23 (aload_2)) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "src" "java.awt.image.FilteredImageSource" (class "java.awt.image.ImageProducer")))) 
                                      (28 (invokevirtual (methodCP "resendTopDownLeftRight" "java.awt.image.ImageFilter" ((class "java.awt.image.ImageProducer")) void))) 
                                      (31 (return)) ;;at TAG_0
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.image.ImageProducer")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FilteredImageSource-class-table*
  (make-static-class-decls 
   *java.awt.image.FilteredImageSource*))

(defconst *package-name-map* 
  ("java.awt.image.FilteredImageSource" . "java.awt.image"))

