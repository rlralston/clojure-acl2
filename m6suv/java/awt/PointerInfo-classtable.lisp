; PointerInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.PointerInfo*
 (make-class-def
      '(class "java.awt.PointerInfo"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "device" (class "java.awt.GraphicsDevice") (accessflags  *class*  *private* ) -1)
                        (field "location" (class "java.awt.Point") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.GraphicsDevice") (class "java.awt.Point"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "device" "java.awt.PointerInfo" (class "java.awt.GraphicsDevice"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "location" "java.awt.PointerInfo" (class "java.awt.Point"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDevice"
                              (parameters )
                              (returntype . (class "java.awt.GraphicsDevice"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "device" "java.awt.PointerInfo" (class "java.awt.GraphicsDevice"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLocation"
                              (parameters )
                              (returntype . (class "java.awt.Point"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "location" "java.awt.PointerInfo" (class "java.awt.Point"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PointerInfo-class-table*
  (make-static-class-decls 
   *java.awt.PointerInfo*))

(defconst *package-name-map* 
  ("java.awt.PointerInfo" . "java.awt"))

