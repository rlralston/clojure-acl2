; MembershipKey-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.MembershipKey*
 (make-class-def
      '(class "java.nio.channels.MembershipKey"
            "java.lang.Object"
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isValid"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drop"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "block"
                              (parameters (class "java.net.InetAddress"))
                              (returntype . (class "java.nio.channels.MembershipKey"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "unblock"
                              (parameters (class "java.net.InetAddress"))
                              (returntype . (class "java.nio.channels.MembershipKey"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "channel"
                              (parameters )
                              (returntype . (class "java.nio.channels.MulticastChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "group"
                              (parameters )
                              (returntype . (class "java.net.InetAddress"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "networkInterface"
                              (parameters )
                              (returntype . (class "java.net.NetworkInterface"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "sourceAddress"
                              (parameters )
                              (returntype . (class "java.net.InetAddress"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MembershipKey-class-table*
  (make-static-class-decls 
   *java.nio.channels.MembershipKey*))

(defconst *package-name-map* 
  ("java.nio.channels.MembershipKey" . "java.nio.channels"))

