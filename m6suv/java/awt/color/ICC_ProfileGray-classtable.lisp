; ICC_ProfileGray-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.color.ICC_ProfileGray*
 (make-class-def
      '(class "java.awt.color.ICC_ProfileGray"
            "java.awt.color.ICC_Profile"
            (constant_pool
                        (LONG -1124721290732002649)
                        (INT 1800688195))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (lload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.awt.color.ICC_Profile" (long) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "sun.java2d.cmm.ProfileDeferralInfo"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.awt.color.ICC_Profile" ((class "sun.java2d.cmm.ProfileDeferralInfo")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMediaWhitePoint"
                              (parameters )
                              (returntype . (array float))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getMediaWhitePoint" "java.awt.color.ICC_Profile" () (array float))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getGamma"
                              (parameters )
                              (returntype . float)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (ldc 1))         ;;INT:: "1800688195"
                                      (3 (invokespecial
					(methodCP "getGamma" "java.awt.color.ICC_Profile" (int) float)))
                                      (6 (fstore_1))
                                      (7 (fload_1))
                                      (8 (freturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTRC"
                              (parameters )
                              (returntype . (array short))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (ldc 1))         ;;INT:: "1800688195"
                                      (3 (invokespecial
					(methodCP "getTRC" "java.awt.color.ICC_Profile" (int) (array short))))
                                      (6 (astore_1))
                                      (7 (aload_1))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ICC_ProfileGray-class-table*
  (make-static-class-decls 
   *java.awt.color.ICC_ProfileGray*))

(defconst *package-name-map* 
  ("java.awt.color.ICC_ProfileGray" . "java.awt.color"))
