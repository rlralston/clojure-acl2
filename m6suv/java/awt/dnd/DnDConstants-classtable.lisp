; DnDConstants-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:25 CDT 2014.
;

(defconst *java.awt.dnd.DnDConstants*
 (make-class-def
      '(class "java.awt.dnd.DnDConstants"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 3)
                        (INT 1073741824))
            (fields
                        (field "ACTION_NONE" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "ACTION_COPY" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "ACTION_MOVE" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "ACTION_COPY_OR_MOVE" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "ACTION_LINK" int (accessflags  *class*  *final*  *public*  *static* ) 4)
                        (field "ACTION_REFERENCE" int (accessflags  *class*  *final*  *public*  *static* ) 4))
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
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DnDConstants-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DnDConstants*))

(defconst *package-name-map* 
  ("java.awt.dnd.DnDConstants" . "java.awt.dnd"))

