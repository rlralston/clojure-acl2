; Array-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.reflect.Array*
 (make-class-def
      '(class "java.lang.reflect.Array"
            "java.lang.Object"
            (constant_pool)
            (fields)
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
                        (method "newInstance"
                              (parameters (class "java.lang.Class") int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokestatic
					(methodCP "newArray" "java.lang.reflect.Array" ((class "java.lang.Class") int) (class "java.lang.Object"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newInstance"
                              (parameters (class "java.lang.Class") (array int))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *static*  *transient* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokestatic
					(methodCP "multiNewArray" "java.lang.reflect.Array" ((class "java.lang.Class") (array int)) (class "java.lang.Object"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLength"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "get"
                              (parameters (class "java.lang.Object") int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getBoolean"
                              (parameters (class "java.lang.Object") int)
                              (returntype . boolean)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getByte"
                              (parameters (class "java.lang.Object") int)
                              (returntype . byte)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getChar"
                              (parameters (class "java.lang.Object") int)
                              (returntype . char)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getShort"
                              (parameters (class "java.lang.Object") int)
                              (returntype . short)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getInt"
                              (parameters (class "java.lang.Object") int)
                              (returntype . int)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getLong"
                              (parameters (class "java.lang.Object") int)
                              (returntype . long)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getFloat"
                              (parameters (class "java.lang.Object") int)
                              (returntype . float)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "getDouble"
                              (parameters (class "java.lang.Object") int)
                              (returntype . double)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "set"
                              (parameters (class "java.lang.Object") int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setBoolean"
                              (parameters (class "java.lang.Object") int boolean)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setByte"
                              (parameters (class "java.lang.Object") int byte)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setChar"
                              (parameters (class "java.lang.Object") int char)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setShort"
                              (parameters (class "java.lang.Object") int short)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setInt"
                              (parameters (class "java.lang.Object") int int)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setLong"
                              (parameters (class "java.lang.Object") int long)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setFloat"
                              (parameters (class "java.lang.Object") int float)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "setDouble"
                              (parameters (class "java.lang.Object") int double)
                              (returntype . void)
                              (accessflags  *class*  *native*  *public*  *static* )
                              (code))
                        (method "newArray"
                              (parameters (class "java.lang.Class") int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code))
                        (method "multiNewArray"
                              (parameters (class "java.lang.Class") (array int))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code)))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Array-class-table*
  (make-static-class-decls 
   *java.lang.reflect.Array*))

(defconst *package-name-map* 
  ("java.lang.reflect.Array" . "java.lang.reflect"))

