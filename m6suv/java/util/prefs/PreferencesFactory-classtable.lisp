; PreferencesFactory-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.prefs.PreferencesFactory*
 (make-class-def
      '(class "java.util.prefs.PreferencesFactory"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "systemRoot"
                              (parameters )
                              (returntype . (class "java.util.prefs.Preferences"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "userRoot"
                              (parameters )
                              (returntype . (class "java.util.prefs.Preferences"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PreferencesFactory-class-table*
  (make-static-class-decls 
   *java.util.prefs.PreferencesFactory*))

(defconst *package-name-map* 
  ("java.util.prefs.PreferencesFactory" . "java.util.prefs"))

