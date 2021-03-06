; ProxySelector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.ProxySelector*
 (make-class-def
      '(class "java.net.ProxySelector"
            "java.lang.Object"
            (constant_pool
                        (STRING  "sun.net.spi.DefaultProxySelector"))
            (fields
                        (field "theProxySelector" (class "java.net.ProxySelector") (accessflags  *class*  *private*  *static* ) -1))
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
                        (method "getDefault"
                              (parameters )
                              (returntype . (class "java.net.ProxySelector"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (ifnull 15))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getstatic (fieldCP "GET_PROXYSELECTOR_PERMISSION" "sun.security.util.SecurityConstants" (class "java.net.NetPermission")))) 
                                      (12 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (15 (getstatic (fieldCP "theProxySelector" "java.net.ProxySelector" (class "java.net.ProxySelector")))) ;;at TAG_0
                                      (18 (areturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setDefault"
                              (parameters (class "java.net.ProxySelector"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_1)) 
                                      (4 (aload_1)) 
                                      (5 (ifnull 15))  ;;to TAG_0
                                      (8 (aload_1)) 
                                      (9 (getstatic (fieldCP "SET_PROXYSELECTOR_PERMISSION" "sun.security.util.SecurityConstants" (class "java.net.NetPermission")))) 
                                      (12 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (putstatic (fieldCP "theProxySelector" "java.net.ProxySelector" (class "java.net.ProxySelector")))) 
                                      (19 (return)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "select"
                              (parameters (class "java.net.URI"))
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "connectFailed"
                              (parameters (class "java.net.URI") (class "java.net.SocketAddress") (class "java.io.IOException"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 39)
                                   (parsedcode
                                      (0 (ldc 0)) ;;at TAG_2;;STRING:: "sun.net.spi.DefaultProxySelector"
                                      (2 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (5 (astore_0)) 
                                      (6 (aload_0)) 
                                      (7 (ifnull 30)) ;;to TAG_0
                                      (10 (ldc_w )) 
                                      (13 (aload_0)) 
                                      (14 (invokevirtual (methodCP "isAssignableFrom" "java.lang.Class" ((class "java.lang.Class")) boolean))) 
                                      (17 (ifeq 30)) ;;to TAG_0
                                      (20 (aload_0)) 
                                      (21 (invokevirtual (methodCP "newInstance" "java.lang.Class" () (class "java.lang.Object")))) 
                                      (24 (checkcast (class "java.net.ProxySelector"))) 
                                      (27 (putstatic (fieldCP "theProxySelector" "java.net.ProxySelector" (class "java.net.ProxySelector")))) 
                                      (30 (goto 38)) ;;to TAG_1;;at TAG_0
                                      (33 (astore_0)) ;;at TAG_3
                                      (34 (aconst_null)) 
                                      (35 (putstatic (fieldCP "theProxySelector" "java.net.ProxySelector" (class "java.net.ProxySelector")))) 
                                      (38 (return)) ;;at TAG_1
                                      (endofcode 39))
                                   (Exceptions 
                                     (handler 0 30  33 (class "java.lang.Exception")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ProxySelector-class-table*
  (make-static-class-decls 
   *java.net.ProxySelector*))

(defconst *package-name-map* 
  ("java.net.ProxySelector" . "java.net"))

