; Cursor$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.Cursor$1*
 (make-class-def
      '(class "java.awt.Cursor$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$fileName" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "val$fx" int (accessflags  *class*  *final* ) -1)
                        (field "val$fy" int (accessflags  *class*  *final* ) -1)
                        (field "val$flocalized" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") int int (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$fileName" "java.awt.Cursor$1" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "val$fx" "java.awt.Cursor$1" int)))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "val$fy" "java.awt.Cursor$1" int)))
                                      (15 (aload_0))
                                      (16 (aload 4))
                                      (18 (putfield (fieldCP "val$flocalized" "java.awt.Cursor$1" (class "java.lang.String"))))
                                      (21 (aload_0))
                                      (22 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 57)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getDefaultToolkit" "java.awt.Toolkit" () (class "java.awt.Toolkit"))))
                                      (3 (astore_1))
                                      (4 (aload_1))
                                      (5 (new (class "java.lang.StringBuilder")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (12 (invokestatic
					(methodCP "access$100" "java.awt.Cursor" () (class "java.lang.String"))))
                                      (15 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "val$fileName" "java.awt.Cursor$1" (class "java.lang.String"))))
                                      (22 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (25 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (28 (invokevirtual
					(methodCP "getImage" "java.awt.Toolkit" ((class "java.lang.String")) (class "java.awt.Image"))))
                                      (31 (astore_2))
                                      (32 (aload_1))
                                      (33 (aload_2))
                                      (34 (new (class "java.awt.Point")))
                                      (37 (dup))
                                      (38 (aload_0))
                                      (39 (getfield (fieldCP "val$fx" "java.awt.Cursor$1" int)))
                                      (42 (aload_0))
                                      (43 (getfield (fieldCP "val$fy" "java.awt.Cursor$1" int)))
                                      (46 (invokespecial
					(methodCP "<init>" "java.awt.Point" (int int) void)))
                                      (49 (aload_0))
                                      (50 (getfield (fieldCP "val$flocalized" "java.awt.Cursor$1" (class "java.lang.String"))))
                                      (53 (invokevirtual
					(methodCP "createCustomCursor" "java.awt.Toolkit" ((class "java.awt.Image") (class "java.awt.Point") (class "java.lang.String")) (class "java.awt.Cursor"))))
                                      (56 (areturn))
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedExceptionAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Cursor$1-class-table*
  (make-static-class-decls 
   *java.awt.Cursor$1*))

(defconst *package-name-map* 
  ("java.awt.Cursor$1" . "java.awt"))

