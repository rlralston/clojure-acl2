; SelectableChannel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.SelectableChannel*
 (make-class-def
      '(class "java.nio.channels.SelectableChannel"
            "java.nio.channels.spi.AbstractInterruptibleChannel"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.nio.channels.spi.AbstractInterruptibleChannel" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "provider"
                              (parameters )
                              (returntype . (class "java.nio.channels.spi.SelectorProvider"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "validOps"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isRegistered"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "keyFor"
                              (parameters (class "java.nio.channels.Selector"))
                              (returntype . (class "java.nio.channels.SelectionKey"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "register"
                              (parameters (class "java.nio.channels.Selector") int (class "java.lang.Object"))
                              (returntype . (class "java.nio.channels.SelectionKey"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "register"
                              (parameters (class "java.nio.channels.Selector") int)
                              (returntype . (class "java.nio.channels.SelectionKey"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (aconst_null))
                                      (4 (invokevirtual
					(methodCP "register" "java.nio.channels.SelectableChannel" ((class "java.nio.channels.Selector") int (class "java.lang.Object")) (class "java.nio.channels.SelectionKey"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "configureBlocking"
                              (parameters boolean)
                              (returntype . (class "java.nio.channels.SelectableChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isBlocking"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "blockingLock"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.channels.Channel")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SelectableChannel-class-table*
  (make-static-class-decls 
   *java.nio.channels.SelectableChannel*))

(defconst *package-name-map* 
  ("java.nio.channels.SelectableChannel" . "java.nio.channels"))
