#|$ACL2s-Preamble$;
(include-book "../../mc/mc")

(include-book "../../mc/utilities")
(include-book "../big-step")

(include-book "../java/lang/Boolean")

;(include-book "lang/AFn")
;(include-book "lang/AFunction")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$not-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (DUP)
    (IFNULL 15)
    (GETSTATIC "java.lang.Boolean" "FALSE" NIL)
    (IF_ACMPEQ 10)
    (GETSTATIC "java.lang.Boolean" "FALSE" NIL)
    (GOTO 7)
    (POP)
    (GETSTATIC "java.lang.Boolean" "TRUE" NIL)
    (ARETURN)))  

(defconst *clojure.core$not*
  (make-class-decl
   "clojure.core$not"
   '(
     "clojure.lang.AFunction"
     "clojure.lang.AFn"
     "java.lang.Object"
    )
   '(
     ; clojure.lang.AFunction interfaces
     "clojure.lang.IObj" 
     "java.util.Comparator" 
     "clojure.lang.Fn" 
     "java.io.Serializable" 
     ; clojure.lang.AFn interfaces
     "clojure.lang.IFn"
    )
   '()
   '()
   '()
   (list
    *core$not-invoke*
   )
   '(REF -1)))

(defun |core$not|-loaded? (class-table heap)
  (and (|Boolean|-loaded? class-table heap)
       (loaded? class-table
                "clojure.core$not"
                *clojure.core$not*)))

(defthm |core$not|-dep 
  (implies
   (|core$not|-loaded? (class-table s) (heap s))
   (|Boolean|-loaded? (class-table s) (heap s)))
  :rule-classes :forward-chaining)

(defthm |core$not|-method
  (implies 
   (|core$not|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$not"
                         (class-table s))
          *core$not-invoke*)))

(in-theory (disable |core$not|-loaded?))

(defun |core$not|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$not" 
                 ref 
                 heap)))

(defun |core$not:invoke|-param1 (s)
  (top (stack (top-frame s))))

(defun |core$not:invoke|-poised (s)
  (and 
   (equal (next-inst s) 
          '(INVOKEINTERFACE "clojure.lang.IFn" 
                            "invoke" 
                            1))
   (|core$not|-p (top (pop (stack (top-frame s)))) 
                 (heap s))))

(local (in-theory (enable
    ->-execute-ACONST_NULL               
    ->-execute-ALOAD_1
    ->-execute-ARETURN
    ->-execute-ASTORE_1
    ->-execute-DUP
    ->-execute-GETSTATIC
    ->-execute-GOTO
    ->-execute-IF_ACMPEQ
    ->-execute-IFNULL
    ->-execute-INVOKEINTERFACE
    ->-execute-POP)))

(defthm invoke-null-is-true
  (implies
   (and (|core$not|-loaded? (class-table s) (heap s))
        (|core$not:invoke|-poised s)
        (nullrefp (|core$not:invoke|-param1 s)))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (|Boolean:TRUE|-get (heap s)
                                                (class-table s))
                            (popn 2 (stack (top-frame s))))))))
                      
(defthm invoke-false-is-true
  (implies
   (and (|core$not|-loaded? (class-table s) (heap s))
        (|core$not:invoke|-poised s)
        (equal (|core$not:invoke|-param1 s)
               (|Boolean:FALSE|-get (heap s)
                                    (class-table s))))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (|Boolean:TRUE|-get (heap s)
                                                (class-table s))
                            (popn 2 (stack (top-frame s))))))))

(defthm invoke-not-null-not-false-is-false
  (implies
   (and (|core$not|-loaded? (class-table s) (heap s))
        (|core$not:invoke|-poised s)
        (not (nullrefp (|core$not:invoke|-param1 s)))
        (not (equal (|core$not:invoke|-param1 s)
                    (|Boolean:FALSE|-get (heap s)
                                         (class-table s)))))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (|Boolean:FALSE|-get (heap s)
                                                 (class-table s))
                            (popn 2 (stack (top-frame s))))))))#|ACL2s-ToDo-Line|#
