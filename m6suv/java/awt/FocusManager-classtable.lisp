; FocusManager-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.FocusManager*
 (make-class-def
      '(class "java.awt.FocusManager"
            "java.lang.Object"
            (constant_pool
                        (LONG 2491878825643557906))
            (fields
                        (field "focusRoot" (class "java.awt.Container") (accessflags  *class* ) -1)
                        (field "focusOwner" (class "java.awt.Component") (accessflags  *class* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
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
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FocusManager-class-table*
  (make-static-class-decls 
   *java.awt.FocusManager*))

(defconst *package-name-map* 
  ("java.awt.FocusManager" . "java.awt"))
