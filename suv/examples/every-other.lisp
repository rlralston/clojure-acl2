#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../../mc/utilities")
(include-book "../big-step")

(include-book "../clojure/lang/Var")

(include-book "../clojure/core$first")
(include-book "../clojure/core$rest")
(include-book "../clojure/core$cons")
(include-book "../clojure/core$empty")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *examples$every_other-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (GETSTATIC "examples.core$every_other" "const__0" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (DUP)
    (IFNULL 13)
    (GETSTATIC "java.lang.Boolean" "FALSE" NIL)
    (IF_ACMPEQ 8)
    (ACONST_NULL)
    (GOTO 78)
    (POP)
    (GETSTATIC "examples.core$every_other" "const__1" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__2" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (GETSTATIC "examples.core$every_other" "const__3" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__4" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__4" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 2)
    (ARETURN)))
  
(defconst *examples$every_other*
  (make-class-decl
   "examples.core$every_other"  
   '("clojure.lang.AFunction")
   '()
   '()
   '("const__0"
     "const__1"
     "const__2"
     "const__3"
     "const__4")
   '((STRING "clojure.core")
     (STRING "empty?")
     (STRING "cons")
     (STRING "first")
     (STRING "examples.core")
     (STRING "every-other")
     (STRING "rest"))

   (list
    *examples$every_other-invoke*
   )
   '(REF -1)))

(defun |every_other:empty?|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__0"
   heap
   class-table))

(defun |every_other:cons|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__1"
   heap
   class-table))

(defun |every_other:first|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__2"
   heap
   class-table))

(defun |every_other:every_other|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__3"
   heap
   class-table))

(defun |every_other:rest|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__4"
   heap
   class-table))

(defun |every_other|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "examples.core$every_other"
                 ref 
                 heap)))

(defun |every_other|-loaded? (class-table heap)
  (let* ((first-var (|every_other:first|-get heap
                                             class-table))
         (rest-var (|every_other:rest|-get heap
                                           class-table))
         (cons-var (|every_other:cons|-get heap
                                           class-table))         
         (empty-var (|every_other:empty?|-get heap
                                              class-table))
         (every-var (|every_other:every_other|-get heap
                                                   class-table)))
  (and   
   (|Var|-loaded? class-table)
   (|Boolean|-loaded? class-table heap)
   (|core$first|-loaded? class-table heap)
   (|core$cons|-loaded? class-table heap)
   (|core$rest|-loaded? class-table heap)
   (|core$empty|-loaded? class-table heap)
   (loaded? class-table
            "examples.core$every_other" 
            *examples$every_other*)
   (|Var|-p first-var heap)
   (|Var|-p rest-var heap)
   (|Var|-p cons-var heap)
   (|Var|-p empty-var heap)
   (|Var|-p every-var heap)
   (|core$first|-p (|Var:root|-get first-var heap)
                   heap)
   (|core$rest|-p (|Var:root|-get rest-var heap)
                   heap)
   (|core$cons|-p (|Var:root|-get cons-var heap)
                  heap)
   (|core$empty|-p (|Var:root|-get empty-var heap)
                   heap)
   (|every_other|-p (|Var:root|-get every-var heap)
                    heap))))

(defthm |every_other|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (and (|Var|-loaded? (class-table s))
        (|Boolean|-loaded? (class-table s) (heap s))
        (|core$first|-loaded? (class-table s) (heap s))
        (|core$cons|-loaded? (class-table s) (heap s))        
        (|core$rest|-loaded? (class-table s) (heap s))
        (|core$empty|-loaded? (class-table s) (heap s))))
  :rule-classes :forward-chaining)

(defthm |every_other:Vars|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (and (|Var|-p (|every_other:first|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:cons|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:rest|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:empty?|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:every_other|-get (heap s) (class-table s))
                 (heap s))))
  :rule-classes :forward-chaining)

(defthm |every_other:first|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$first|-p (|Var:root|-get (|every_other:first|-get (heap s)
                                                            (class-table s))
                                   (heap s))
                   (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:rest|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$rest|-p (|Var:root|-get (|every_other:rest|-get (heap s)
                                                          (class-table s))
                                  (heap s))
                  (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:cons|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$cons|-p (|Var:root|-get (|every_other:cons|-get (heap s)
                                                          (class-table s))
                                  (heap s))
                  (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:empty?|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$empty|-p (|Var:root|-get (|every_other:empty?|-get (heap s)
                                                             (class-table s))
                                   (heap s))
                   (heap s)))
  :rule-classes (:forward-chaining :rewrite))

(defthm |every_other:every_other|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|every_other|-p (|Var:root|-get (|every_other:every_other|-get (heap s)
                                                                   (class-table s))
                                    (heap s))
                    (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other|-method
  (implies 
   (|every_other|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "examples.core$every_other"
                         (class-table s))
          *examples$every_other-invoke*)))

(in-theory (disable |core$empty|-loaded?))

(local (include-book "std/lists/update-nth" :dir :system))

(defun every-other (xs)
  (if (endp xs)
    nil
    (cons (car xs)
          (every-other (cdr (cdr xs))))))

(defun alloc (x coll heap)
  (if (null coll)
    (bind (len heap)
          (|PersistentList|-init (|PersistentList|-new) x)
          heap)
    (bind (len heap)
          (|Cons|-init (|Cons|-new) x coll)
          heap)))

; From isort
(defun all-smallp (heap max)
  (cond ((endp heap) t)
        (t (and (integerp (caar heap))
                (<= 0 (caar heap))
                (< (caar heap) max)
                (all-smallp (cdr heap) max)))))

(defthm alistp-alloc-heap
  (implies (alistp heap)
           (alistp (alloc y coll heap))))

;(defthm alistp-cons-heap
;  (implies (alistp heap)
;           (alistp (bind (len heap) y heap))))

(defthm len-bind
  (implies (case-split (alistp alist))
           (equal (len (bind key val alist))
                  (if (bound? key alist)
                      (len alist)
                    (+ 1 (len alist))))))

(defthm len-alloc-heap
  (implies (and (alistp heap)
                (all-smallp heap (len heap)))
           (equal (len (alloc x coll heap))
                  (+ 1 (len heap)))))

;(defthm len-cons-heap
;  (implies (and (alistp heap)
;                (all-smallp heap (len heap)))
;           (equal (len (bind (len heap) x heap))
;                  (+ 1 (len heap)))))

(defthm all-smallp-bind
  (implies (and (all-smallp alist max1)
                (<= max1 max2))
           (equal (all-smallp (bind key val alist) max2)
                  (and (integerp key)
                       (<= 0 key)
                       (< key max2)))))

(defthm all-smallp-alloc-heap
  (implies (and (alistp heap)
                (all-smallp heap (- max 1)))
           (equal (all-smallp (alloc x coll heap) max)
                  (< (len heap) max))))

;(defthm all-smallp-cons-heap
;  (implies (and (alistp heap)
;;                (all-smallp heap (- max 1)))
;           (equal (all-smallp (bind (len heap) y heap) max)
;                  (< (len heap) max))))

(defun alloc-list (xs heap)
  (if (endp xs)
    heap
    ;(let* ((x (car xs)))
    (let* ((x (seq-first (car xs) heap)))
      (if (endp (cdr xs))
        (alloc x nil heap)
        (alloc x
               (list 'REF (+ (len heap)
                             (len (cdr xs))
                             -1))
               (alloc-list (cdr xs) heap))))))

(defthm alistp-alloc-list
  (implies (alistp heap)
           (alistp (alloc-list xs heap))))

(defthm cons-loaded-after-alloc
  (implies 
   (and (|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|PersistentList|-loaded? class-table 
                        (alloc x coll heap)))
  :hints 
  (("Goal"
    :in-theory (enable |PersistentList|-loaded?))))

(defthm all-smallp-less-than-incr
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (< (len heap) (+ 1 (len heap)))))

(defthm all-smallp-alloc-list-heap
  (implies (and (alistp heap)
                (all-smallp heap (len heap)))
           (all-smallp (alloc-list xs heap) 
                       (len (alloc-list xs heap)))))           

(defthm len-alloc-list-heap
  (implies (and (alistp heap)
                (all-smallp heap (len heap)))
           (equal (len (alloc-list xs heap))
                  (+ (len xs) (len heap))))
  :hints 
  (("Goal"
    :in-theory (disable seq-first alloc))))

(defthm all-smallp-alloc-list-heap-2
  (implies (and (alistp heap)
                (all-smallp heap (len heap)))
           (all-smallp (alloc-list xs heap) (+ (len heap)
                                               (len xs))))           
           ;(equal (all-smallp (alloc-list xs heap) max)
           ;       (< (len heap) max)))
  :hints 
  (("Goal"
    :in-theory (disable seq-first alloc))))

(defthm PersistentList-loaded-after-alloc-2
  (implies 
   (and (|EmptyList|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|EmptyList|-p ref 
                  (alloc xs coll heap))))

(defthm EmptyList-persists-alloc-list
  (implies 
   (and (|EmptyList|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|EmptyList|-p ref 
                  (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|PersistentList|-loaded?) 
                    (

                     alloc 
                     seq-first
                     |PersistentList:EMPTY|-get
                     |EmptyList|-p)))))  

(defthm EmptyList-persists-after-alloc
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|PersistentList:EMPTY|-get (alloc xs coll heap) 
                                      class-table)
          (|PersistentList:EMPTY|-get heap class-table))))

(defthm |PL:EmptyList|-persists-after-alloc-list
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|PersistentList:EMPTY|-get (alloc-list xs heap) 
                                      class-table)
          (|PersistentList:EMPTY|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d (|PersistentList|-loaded?)
                    (alloc 
                     seq-first
                     |PersistentList:EMPTY|-get
                     |EmptyList|-p)))))

(defthm PersistentList-loaded-after-alloc-4
  (implies 
   (and (|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|PersistentList|-loaded? class-table 
                             (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|PersistentList|-loaded?) 
                    (alloc 
                     seq-first
                     |PersistentList:EMPTY|-get
                     |EmptyList|-p)))))

(defthm pl-loaded-after-alloc
  (implies 
   (and (|Cons|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Cons|-loaded? class-table 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|Cons|-loaded?)
                    (alloc)))))

(defthm RT-loaded-after-alloc
  (implies 
   (and (|RT|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|RT|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|RT|-loaded?)
                    (alloc)))))

(defthm core$rest-loaded-after-alloc
  (implies 
   (and (|core$rest|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$rest|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (enable |core$rest|-loaded?))))

(defthm core$first-loaded-after-alloc
  (implies 
   (and (|core$first|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$first|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (enable |core$first|-loaded?))))

(defthm Boolean-TRUE-Persists-after-alloc
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Boolean:TRUE|-get (alloc xs coll heap) 
                              class-table)
          (|Boolean:TRUE|-get heap class-table))))

(defthm Boolean-TRUE-Persists-after-alloc-list
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Boolean:TRUE|-get (alloc-list xs heap) 
                                      class-table)
          (|Boolean:TRUE|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |Boolean:TRUE|-get)))))

(defthm Boolean-FALSE-Persists-after-alloc
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Boolean:FALSE|-get (alloc xs coll heap) 
                              class-table)
          (|Boolean:FALSE|-get heap class-table))))

(defthm Boolean-FALSE-Persists-after-alloc-list
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Boolean:FALSE|-get (alloc-list xs heap) 
                                      class-table)
          (|Boolean:FALSE|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |Boolean:FALSE|-get)))))

(defthm Boolean-persists-after-alloc
  (implies 
   (and (|Boolean|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Boolean|-p ref 
                  (alloc xs coll heap))))

(defthm Boolean-persists-after-alloc-list
  (implies 
   (and (|Boolean|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Boolean|-p ref 
                  (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |Boolean|-p)))))  

(defthm Boolean-loaded-after-alloc
  (implies 
   (and (|Boolean|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Boolean|-loaded? class-table 
                      (alloc xs coll heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|Boolean|-loaded?)
                    (alloc
                     |Boolean|-p
                     |Boolean:TRUE|-get
                     |Boolean:FALSE|-get)))))

(defthm Boolean-loaded-after-alloc-list
  (implies 
   (and (|Boolean|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Boolean|-loaded? class-table 
                      (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|Boolean|-loaded?)
                    (alloc
                     |Boolean|-p
                     |Boolean:TRUE|-get
                     |Boolean:FALSE|-get)))))

(defthm core$not-loaded-after-alloc
  (implies 
   (and (|core$not|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$not|-loaded? class-table 
                        (alloc xs coll heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|core$not|-loaded?)
                    (alloc)))))

(defthm core$not-loaded-after-alloc-list
  (implies 
   (and (|core$not|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$not|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|core$not|-loaded?)
                    ()))))

(defthm core$not-persists-after-alloc
  (implies 
   (and (|core$not|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$not|-p ref 
                 (alloc xs coll heap))))

(defthm core$not-persists-after-alloc-list
  (implies 
   (and (|core$not|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$not|-p ref 
                 (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$not|-p)))))

(defthm core$seq-persists-after-alloc
  (implies 
   (and (|core$seq|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$seq|-p ref 
                 (alloc xs coll heap))))

(defthm core$seq-persists-after-alloc-list
  (implies 
   (and (|core$seq|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$seq|-p ref 
                 (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$seq|-p)))))

(defthm Var-persists-after-alloc
  (implies 
   (and (|Var|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Var|-p ref 
                 (alloc xs coll heap))))

(defthm Var-persists-after-alloc-list
  (implies 
   (and (|Var|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|Var|-p ref 
            (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |Var|-p)))))

(defthm core$seq-loaded-after-alloc-list
  (implies 
   (and (|core$seq|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$seq|-loaded? class-table 
                       (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|core$seq|-loaded?)
                    (alloc)))))

(defthm Var-Root-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Var:root|-get ref 
                          (alloc xs coll heap))
          (|Var:root|-get ref 
                          heap))))

(defthm Var-Root-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|Var:root|-get ref 
                          (alloc-list xs heap))
          (|Var:root|-get ref 
                          heap)))
  :hints (("Goal" :in-theory (disable |Var:root|-get 
                                      alloc))))

(defthm core$not-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|core$empty:core$not|-get (alloc xs coll heap) 
                                     class-table)
          (|core$empty:core$not|-get heap class-table))))

(defthm core$not-get-Persists-after-alloc-list
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|core$empty:core$not|-get (alloc-list xs heap) 
                                      class-table)
          (|core$empty:core$not|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |core$empty:core$not|-get)))))

(defthm core$seq-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|core$empty:core$seq|-get (alloc xs coll heap) 
                                     class-table)
          (|core$empty:core$seq|-get heap class-table))))

(defthm core$seq-get-Persists-after-alloc-list
  (implies 
   (and ;(|PersistentList|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|core$empty:core$seq|-get (alloc-list xs heap) 
                                      class-table)
          (|core$empty:core$seq|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |core$empty:core$seq|-get)))))

(defthm core$empty-loaded-after-alloc
  (implies 
   (and (|core$empty|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$empty|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|core$empty|-loaded?)
                    (|Var|-p
                     |core$not|-p
                     |core$seq|-p
                     |Var:root|-get)))))

(defthm first-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:first|-get (alloc xs coll heap) 
                                     class-table)
          (|every_other:first|-get heap class-table))))

(defthm first-get-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:first|-get (alloc-list xs heap) 
                                      class-table)
          (|every_other:first|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |every_other:first|-get)))))

(defthm rest-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:rest|-get (alloc xs coll heap) 
                                     class-table)
          (|every_other:rest|-get heap class-table))))

(defthm rest-get-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:rest|-get (alloc-list xs heap) 
                                      class-table)
          (|every_other:rest|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |every_other:rest|-get)))))

(defthm cons-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:cons|-get (alloc xs coll heap) 
                                     class-table)
          (|every_other:cons|-get heap class-table))))

(defthm cons-get-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:cons|-get (alloc-list xs heap) 
                                      class-table)
          (|every_other:cons|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |every_other:cons|-get)))))

(defthm empty-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:empty?|-get (alloc xs coll heap) 
                                     class-table)
          (|every_other:empty?|-get heap class-table))))

(defthm empty-get-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:empty?|-get (alloc-list xs heap) 
                                      class-table)
          (|every_other:empty?|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |every_other:empty?|-get)))))

(defthm eo-get-Persists-after-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:every_other|-get (alloc xs coll heap) 
                                     class-table)
          (|every_other:every_other|-get heap class-table))))

(defthm eo-get-Persists-after-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (equal (|every_other:every_other|-get (alloc-list xs heap) 
                                      class-table)
          (|every_other:every_other|-get heap class-table)))
  :hints 
  (("Goal"
    :in-theory (e/d ()
                    (alloc 
                     seq-first
                     |every_other:every_other|-get)))))

(defthm core$empty-loaded-after-alloc
  (implies 
   (and (|core$empty|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$empty|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d (|core$empty|-loaded?)
                    (|Var|-p
                     |core$not|-p
                     |core$seq|-p
                     |Var:root|-get)))))

(defthm core$cons-loaded-after-alloc
  (implies 
   (and (|core$cons|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$cons|-loaded? class-table 
                        (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (enable |core$cons|-loaded?))))

(defthm core$first-persists-after-alloc
  (implies 
   (and (|core$first|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$first|-p ref 
                   (alloc xs coll heap))))

(defthm core$first-persists-after-alloc-list
  (implies 
   (and (|core$first|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$first|-p ref 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$first|-p)))))

(defthm core$rest-persists-after-alloc
  (implies 
   (and (|core$rest|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$rest|-p ref 
                   (alloc xs coll heap))))

(defthm core$rest-persists-after-alloc-list
  (implies 
   (and (|core$rest|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$rest|-p ref 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$rest|-p)))))

(defthm core$cons-persists-after-alloc
  (implies 
   (and (|core$cons|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$cons|-p ref 
                   (alloc xs coll heap))))

(defthm core$cons-persists-after-alloc-list
  (implies 
   (and (|core$cons|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$cons|-p ref 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$cons|-p)))))

(defthm core$empty-persists-after-alloc
  (implies 
   (and (|core$empty|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$empty|-p ref 
                   (alloc xs coll heap))))

(defthm core$empty-persists-after-alloc-list
  (implies 
   (and (|core$empty|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|core$empty|-p ref 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |core$empty|-p)))))

(defthm every_other-persists-after-alloc
  (implies 
   (and (|every_other|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|every_other|-p ref 
                   (alloc xs coll heap))))

(defthm every_other-persists-after-alloc-list
  (implies 
   (and (|every_other|-p ref heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|every_other|-p ref 
                   (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (e/d () 
                    (alloc 
                     seq-first                     
                     |every_other|-p)))))

(defthm every_other-loaded-after-alloc
  (implies 
   (and (|every_other|-loaded? class-table heap)
        (alistp heap)
        (all-smallp heap (len heap)))
   (|every_other|-loaded? class-table 
                          (alloc-list xs heap)))
  :hints 
  (("Goal"
    :in-theory (enable |every_other|-loaded?))))

(defun seq-listp (xs heap class-table)
  (if (endp xs)
    (equal xs nil)
    (and (seq-p (car xs) heap)
         (not (|EmptyList|-p (car xs) heap))
         (if (endp (cdr xs))
           (equal (seq-more (car xs) heap class-table)
                  (|PersistentList:EMPTY|-get heap class-table))
           (equal (seq-more (car xs) heap class-table)
                  (cadr xs)))
         (seq-listp (cdr xs) heap class-table))))

(defun eo-base-frame (frame heap)
  (and (|every_other|-p (top (locals frame)) heap)
       (equal (pc frame) 0)
       (equal (stack frame) nil)
       (equal (cur-class frame) "clojure.lang.IFn")
       (equal (program frame) 
              (method-program *examples$every_other-invoke*))))  

(defun eo-start-state (frame xs s)
  (let* ((s1 (modify s :call-stack (push frame (call-stack s))))
         (xs-ref (if (endp xs) 
                   (|PersistentList:EMPTY|-get (heap s) 
                                               (class-table s)) 
                   (car xs))))
    (modify 
     s1
     :locals 
     (list (car (locals frame)) xs-ref))))

;(defun eo-recursive-state (frame xs s)
(defun prelude-end-state (frame xs s)
  (let* 
    ((s1 (modify 
          s 
          :call-stack 
          (push frame (call-stack s))))
     (xs-ref (car xs)))
    (modify 
     s1
     :pc 94
     :locals 
     (list (car (locals frame)) (nullref))
     :stack 
     (push 
; 84-89 (rest (rest xs))
      (seq-more (seq-more xs-ref 
                          (heap s) 
                          (class-table s))
                (heap s)
                (class-table s))
      (push (|Var:root|-get 
             (|every_other:every_other|-get (heap s)
                                            (class-table s))
             (heap s))
            (push (seq-first xs-ref (heap s))
                  (push (|Var:root|-get 
                         (|every_other:cons|-get (heap s)
                                                 (class-table s))
                         (heap s))
                        (stack frame))))))))

;(defun eo-post-recursive-state (frame xs s)
(defun postlude-start-state (frame xs s)
  (let* 
    ((s1 (modify 
          s 
          :call-stack 
          (push frame (call-stack s))))
     (xs-ref (car xs)))
    (modify 
     s1
     :pc 99
     :locals (list (car (locals frame)) (nullref))
     :stack 
     (push (seq-first xs-ref (heap s))
           (push (|Var:root|-get 
                  (|every_other:cons|-get (heap s)
                                          (class-table s))
                  (heap s))
                 (stack frame))))))

(defun eo-end-state (frame xs s)
  (let* ((s1 (modify s :call-stack (push frame (call-stack s))))
         (result (if (endp xs)
                   nil 
                   (every-other xs)))
         (result-ref (if (endp result) 
                       (nullref)
                       (list 'REF (+ (len (heap s)) 
                                     (len result) 
                                     -1))))
         (local-1 (if (endp result)
                    (|PersistentList:EMPTY|-get (heap s) 
                                                (class-table s))
                    (nullref))))
    (modify
     s1
     :pc 104
     :locals (list (car (locals frame)) local-1)
     :stack (push result-ref (stack frame))
     :heap (alloc-list result (heap s)))))

(defthm every_other-core$empty=core$empty
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__0" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:empty?|-get (heap s)
                                                      (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))

(defthm every_other-core$cons=core$cons
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__1" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:cons|-get (heap s)
                                                    (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))  

(defthm every_other-core$first=core$first
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__2" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:first|-get (heap s)
                                                     (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))  

(defthm every_other-every_other=every_other
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__3" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:every_other|-get (heap s)
                                                           (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))  

(defthm every_other-core$rest=core$rest
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__4" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:rest|-get (heap s)
                                                    (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))  

(defthmd |PersistentList:EMPTY|-is-not-null
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (not (equal (|PersistentList:EMPTY|-get heap class-table)
               '(REF -1))))
  :hints (("Goal" :in-theory (enable |EmptyList|-p
                                     |PersistentList|-loaded?))))

(defthmd |PersistentList:EMPTY|-is-EmptyList
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (|EmptyList|-p (|PersistentList:EMPTY|-get heap class-table)
                  heap))
  :rule-classes :type-prescription
  :hints (("Goal" :in-theory (enable |PersistentList|-loaded?))))

(defthm every-other-null-ends
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        (endp xs))
   (-> (eo-start-state frame xs s)
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))
    :in-theory 
    (e/d (|PersistentList:EMPTY|-is-not-null
          |PersistentList:EMPTY|-is-EmptyList
          ->-execute-ACONST_NULL
          ->-execute-ALOAD_1
          ->-execute-ARETURN
          ->-execute-ASTORE_1                          
          ->-execute-CHECKCAST
          ->-execute-DUP
          ->-execute-GETSTATIC
          ->-execute-GOTO
          ->-execute-IF_ACMPEQ
          ->-execute-IFNULL
          ->-execute-POP)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
             
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))

(defthm |empty?|-classname
  (implies 
   (and (|core$empty|-loaded? (class-table s) (heap s))
        (|core$empty|-p v (heap s)))
   (equal (class-name-of-ref v (heap s))
          "clojure.core$empty_QMARK_")))

(defthm |rest|-classname
  (implies 
   (and (|core$rest|-loaded? (class-table s) (heap s))
        (|core$rest|-p v (heap s)))
   (equal (class-name-of-ref v (heap s))
          "clojure.core$rest")))

(defthm seq-more-of-seq-is-seq
  (implies 
   (and (|Cons|-loaded? class-table heap)
        (|PersistentList|-loaded? class-table heap)
        (not (endp xs))
        (seq-listp xs heap class-table))
   (seq-p (seq-more (car xs) heap class-table) heap))
  :hints 
  (("Goal" 
    :in-theory                   
    (e/d (|PersistentList:EMPTY|-is-not-null
          |PersistentList:EMPTY|-is-EmptyList)
         (|PersistentList:EMPTY|-get
          |Cons|-p
          |PersistentList|-p
          |EmptyList|-p)))))

(defthm seq-more-more
  (implies 
   (and (|PersistentList|-loaded? class-table heap)
        (equal x (|PersistentList:EMPTY|-get heap class-table)))
   (seq-p (seq-more x heap class-table) heap))
  :hints 
  (("Goal" 
    :in-theory 
    (e/d (|PersistentList|-loaded?
          seq-p 
          seq-more)
         (|PersistentList:EMPTY|-get)))))

(defthm seq-more-more-2
  (implies 
   (and (|PersistentList|-loaded? class-table heap)
        (equal x (|PersistentList:EMPTY|-get heap class-table)))
   (seq-p (seq-more (seq-more x heap class-table)
                    heap
                    class-table)
          heap))
  :hints 
  (("Goal" 
    :in-theory 
    (e/d (|PersistentList|-loaded?
          seq-p 
          seq-more)
         (|PersistentList:EMPTY|-get)))))

(defthmd every-other-big-step-recursive-1
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        (not (endp xs))
        (not (endp (cdr xs)))
        (not (endp (cddr xs))))
   (-> (eo-start-state frame xs s)
       (prelude-end-state frame xs s)))
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))    
    :in-theory 
    (e/d (|PersistentList:EMPTY|-is-not-null
          |PersistentList:EMPTY|-is-EmptyList
          |every_other|-loaded?
          ->-execute-ACONST_NULL
          ->-execute-ALOAD_1
          ->-execute-ARETURN
          ->-execute-ASTORE_1                          
          ->-execute-CHECKCAST
          ->-execute-DUP
          ->-execute-GETSTATIC
          ->-execute-GOTO
          ->-execute-IF_ACMPEQ
          ->-execute-IFNULL
          ->-execute-POP)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          |every_other|-p
          seq-more
          seq-first     
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))

(defthmd every-other-big-step-recursive-2
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        (not (endp xs))
        (not (endp (cdr xs)))
        (endp (cddr xs)))
   (-> (eo-start-state frame xs s)
       (prelude-end-state frame xs s)))
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))    
    :in-theory 
    (e/d (|PersistentList:EMPTY|-is-not-null
          |PersistentList:EMPTY|-is-EmptyList
          |every_other|-loaded?
          ->-execute-ACONST_NULL
          ->-execute-ALOAD_1
          ->-execute-ARETURN
          ->-execute-ASTORE_1                          
          ->-execute-CHECKCAST
          ->-execute-DUP
          ->-execute-GETSTATIC
          ->-execute-GOTO
          ->-execute-IF_ACMPEQ
          ->-execute-IFNULL
          ->-execute-POP)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          |every_other|-p
          seq-more
          seq-first     
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))

(defthmd every-other-big-step-recursive-3
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))        
        (not (endp xs))
        (endp (cdr xs)))
   (-> (eo-start-state frame xs s)
       (prelude-end-state frame xs s)))
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))    
    :in-theory 
    (e/d (|PersistentList:EMPTY|-is-not-null
          |PersistentList:EMPTY|-is-EmptyList
          |every_other|-loaded?
          ->-execute-ACONST_NULL
          ->-execute-ALOAD_1
          ->-execute-ARETURN
          ->-execute-ASTORE_1                          
          ->-execute-CHECKCAST
          ->-execute-DUP
          ->-execute-GETSTATIC
          ->-execute-GOTO
          ->-execute-IF_ACMPEQ
          ->-execute-IFNULL
          ->-execute-POP)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          |every_other|-p
          seq-more
          seq-first     
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))

(defthm prelude-is-correct
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))        
        (not (endp xs)))
   (-> (eo-start-state frame xs s)
       (prelude-end-state frame xs s)))
  :hints 
  (("Goal"
    :in-theory 
    (e/d (every-other-big-step-recursive-1
          every-other-big-step-recursive-2
          every-other-big-step-recursive-3)
         (|EmptyList|-p
          seq-p
          |every_other|-loaded?
          eo-base-frame
          eo-start-state
          prelude-end-state))
    )))

(defthm |every_other|-classname
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (|every_other|-p v (heap s)))
   (equal (class-name-of-ref v (heap s))
          "examples.core$every_other")))

(defun recursive-start (frame xs s)
  (eo-start-state frame 
                  (cddr xs) 
                  (postlude-start-state frame xs s)))


(defthm step-every-other-recursive-pushes-start
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        (not (endp xs))
        
        (equal (car (locals frame))
               (|Var:root|-get 
                (|every_other:every_other|-get (heap s)
                                               (class-table s))
                (heap s)))
        
        )
   (-> (prelude-end-state frame xs s)
       (recursive-start frame xs s)))
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))    
    :in-theory 
    (e/d (->-execute-INVOKEINTERFACE)
         (|Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first             
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))     

(defun recursive-start-end (frame xs s)
  (let* ((recursive-state (postlude-start-state frame xs s))
         (s1 (eo-end-state frame (cdr (cdr xs)) recursive-state)))
    (modify 
     s1
     :locals 
     (list (|Var:root|-get 
            (|every_other:every_other|-get (heap s)
                                           (class-table s))
            (heap s))
           (cadr (locals (top-frame s1)))))))

(defthm bound-Cons-is-seq  
  (implies (and (integerp i)
                (>= i 0))
           
           (seq-p (list 'REF i) 
                  (bind i 
                        (|Cons|-init (|Cons|-new) x coll) 
                        new-heap))))

(defthm bound-PersistentList-is-seq 
  (implies 
   (and (integerp i)
        (>= i 0))           
   (seq-p (list 'REF i) 
          (bind i 
                (|PersistentList|-init (|PersistentList|-new) x) 
                new-heap))))

(defthm |cons|-classname
  (implies 
   (and (|core$cons|-loaded? (class-table s) (heap s))
        (|core$cons|-p v (heap s)))
   (equal (class-name-of-ref v (heap s))
          "clojure.core$cons")))

(defthm seq-p-PersistentList-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (seq-p (list 'REF (len heap))
          (bind (len heap) 
                (|PersistentList|-init (|PersistentList|-new) x) 
                heap))))

(defthm seq-p-Cons-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (seq-p (list 'REF (len heap))
          (bind (len heap) 
                (|Cons|-init (|Cons|-new) x coll) 
                heap))))

(defthm seq-p-alloc
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap)))
   (seq-p (list 'REF (len heap))
          (alloc x coll heap))) 
  :hints 
  (("Goal"
    :use (seq-p-Cons-alloc
          seq-p-PersistentList-alloc)
    :in-theory 
    (e/d ()
         (seq-p          
          |PersistentList|-init
          |PersistentList|-new
          |Cons|-init
          |Cons|-new)))))

(defthm seq-p-alloc-list
  (implies 
   (and (alistp heap)
        (all-smallp heap (len heap))
        (not (endp xs)))
   (seq-p (list 'REF 
                (+ (len heap)
                   (len xs)
                   -1))
          (alloc-list xs heap))) 
  :hints 
  (("Goal"
    :use ((:instance seq-p-alloc
           (heap (alloc-list (cdr xs) heap)))
          (:instance len-alloc-list-heap          
           (xs (cdr xs)))
          (:instance all-smallp-alloc-list-heap
           (xs (cdr xs))))
    :in-theory 
    (e/d ()
         (len-alloc-list-heap
          seq-p
          seq-first
          alloc)))   
   ))


(defthm every-other-recursive-ends
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)) 
        (not (endp xs))                              
        )
   (-> (eo-end-state frame 
                     (cddr xs)
                     (postlude-start-state frame xs s))
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use ((:instance seq-p-alloc-list
           (xs (every-other (cddr xs)))
           (heap (heap s))))
    :in-theory 
    (e/d (->-execute-ARETURN)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first 
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get                   
          ))
    )))

(defthm every-other-recursive-ends-on-nil
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)) 
        (not (endp xs))
        (endp (cdr xs))
        )
   (-> (eo-end-state frame 
                     nil
                     (postlude-start-state frame xs s))
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use ((:instance seq-p-alloc-list
           (xs (every-other (cddr xs)))
           (heap (heap s))))
    :in-theory 
    (e/d (->-execute-ARETURN)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first 
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get                   
          ))
    )))

(defun cdr-state-induction (xs frame s)
  (if (endp xs)
    (list xs frame s)
    (list (cdr-state-induction (cddr xs)
                               frame
                               (postlude-start-state frame xs s)))))

(defthm eo-post-recursive-persives-alistp
  (implies 
   (and (alistp (heap s))
        (eo-base-frame frame (heap s)))
   (alistp (heap (postlude-start-state frame xs s)))))

(defthm eo-post-recursive-persives-all-smallp
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s)))
   (all-smallp (heap (postlude-start-state frame xs s))
               (len (heap (postlude-start-state frame xs s))))))

(defthm eo-preserves-every_other-loaded?
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s))
        (|every_other|-loaded? (class-table s)
                               (heap s)))
   (|every_other|-loaded? (class-table (postlude-start-state frame xs s))
                          (heap (postlude-start-state frame xs s)))))

(defthm eo-preserves-eo-base-frame
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s)))
   (eo-base-frame frame
                  (heap (postlude-start-state frame xs s)))))

(defthm eo-preserves-every_other-get
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s)))
   (equal               
    (|Var:root|-get 
     (|every_other:every_other|-get (heap (postlude-start-state frame xs s))
                                    (class-table (postlude-start-state frame xs s)))
     (heap (postlude-start-state frame xs s)))    
    (|Var:root|-get 
           (|every_other:every_other|-get (heap s)
                                          (class-table s))
           (heap s)))))

(defthmd eo-preserves-seq-p
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)))
   (seq-listp (cddr xs) 
              (heap (postlude-start-state frame xs s)) 
              (class-table (postlude-start-state frame xs s)))))

(defthm every-other-is-every-other-internal
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))  
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        
        (equal (car (locals frame))
               (|Var:root|-get 
                (|every_other:every_other|-get (heap s)
                                               (class-table s))
                (heap s)))
        
        )
   (-> (eo-start-state frame xs s)
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :induct (cdr-state-induction xs
                                 frame
                                 s)
    :in-theory 
    (e/d (eo-preserves-seq-p)
         (|every_other|-loaded?
          eo-start-state
          prelude-end-state
          postlude-start-state
          eo-base-frame        
          eo-end-state         
          |Var:root|-get    
          |EmptyList|-p          
          |PersistentList:EMPTY|-get
          |every_other:every_other|-get
          seq-p
          seq-more
          seq-first))
    )))

(defun internal-frame (frame heap class-table)
  (make-frame 
   (pc frame)
   (list
    (|Var:root|-get 
     (|every_other:every_other|-get heap class-table) 
     heap))
   (stack frame)
   (program frame)
   (cur-class frame)))

(defthm internal-frame-car-locals-is-every_other
  (implies 
   (eo-base-frame frame (heap s))
   (equal 
    (car (locals (internal-frame frame (heap s) (class-table s))))
         (|Var:root|-get 
          (|every_other:every_other|-get (heap s)
                                         (class-table s))
          (heap s)))))

(defun eo-internal-start (frame xs s)
  (let* ((s1 (postlude-start-state frame xs s)))
  (eo-start-state (internal-frame frame (heap s1) (class-table s1))
                  (cddr xs) 
                  s1)))

(defthm ext-prelude->internal-start
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        (not (endp xs)))
   (-> (prelude-end-state frame xs s)
       (eo-internal-start frame xs s)))         
  :hints 
  (("Goal"
    :use (:instance TRUE-and-FALSE-Boolean
          (class-table (class-table s))
          (heap (heap s)))    
    :in-theory 
    (e/d (->-execute-INVOKEINTERFACE)
         (|Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first             
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get))
    )))#|ACL2s-ToDo-Line|#
     

(defthm internal-frame-is-base-frame
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s)))
   (eo-base-frame (internal-frame frame 
                                  (heap s) 
                                  (class-table s))
                  (heap s)))
  :hints 
  (("Goal" 
    :in-theory 
    (e/d (top)
         (|every_other|-loaded?
          |every_other|-p
          |Var:root|-get
          |every_other:every_other|-get)))))
         

(defthm every-other-is-every-other-internal-2
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))  
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))       
        )
   (-> (eo-start-state (internal-frame frame (heap s) (class-table s)) xs s)
       (eo-end-state (internal-frame frame (heap s) (class-table s)) xs s)))
  :hints 
  (("Goal"
    :in-theory 
    (e/d (eo-preserves-seq-p)
         (internal-frame 
          |every_other|-loaded?
          eo-start-state
          prelude-end-state
          postlude-start-state
          eo-base-frame        
          eo-end-state         
          |Var:root|-get    
          |EmptyList|-p          
          |PersistentList:EMPTY|-get
          |every_other:every_other|-get
          seq-p
          seq-more
          seq-first))
    )))

(defthmd eo-preserves-seq-p-2
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)))
   (seq-listp xs 
              (heap (postlude-start-state frame xs s)) 
              (class-table (postlude-start-state frame xs s)))))

(defthm every-other-recursive-external-end
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)) 
        (not (endp xs))                              
        )
   (-> (eo-end-state 
        (internal-frame frame
                        (heap (postlude-start-state frame xs s))
                        (class-table (postlude-start-state frame xs s)))
        (cddr xs)
        (postlude-start-state frame xs s))
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use ((:instance seq-p-alloc-list
           (xs (every-other (cddr xs)))
           (heap (heap s))))
    :in-theory 
    (e/d (->-execute-ARETURN)
         (|every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first 
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get                   
          ))
    )))

(defthm every-other-recursive-external-end-nil
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))
        
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s)) 
        (not (endp xs))
        (endp (cdr xs))
        )
   (-> (eo-end-state 
        (internal-frame frame
                        (heap (postlude-start-state frame xs s))
                        (class-table (postlude-start-state frame xs s)))
        nil
        (postlude-start-state frame xs s))
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use ((:instance seq-p-alloc-list
           (xs (every-other (cddr xs)))
           (heap (heap s))))
    :in-theory 
    (e/d (
          ;|PersistentList:EMPTY|-is-not-null
          ;|PersistentList:EMPTY|-is-EmptyList
          ->-execute-ARETURN
         )
         (;eo-base-frame
          ;alloc
          
          |every_other|-loaded?
          |Var|-p
          |Var:root|-get    
          |EmptyList|-p
          |Cons|-p
          |PersistentList|-p
          |Boolean|-loaded?
          |core$empty|-p
          |PersistentList:EMPTY|-get
          |Boolean:FALSE|-get
          |Boolean:TRUE|-get
          |core$first|-p
          |core$rest|-p
          |core$cons|-p
          seq-more
          seq-first 
          seq-p
          |core$empty:core$not|-get
          |core$empty:core$seq|-get                              
          |every_other:first|-get
          |every_other:rest|-get
          |every_other:empty?|-get
          |every_other:cons|-get
          |every_other:every_other|-get                   
          ))
    )))

(defthm every-other-is-every-other
  (implies 
   (and (alistp (heap s))
        (all-smallp (heap s) (len (heap s)))  
        (|every_other|-loaded? (class-table s) (heap s))
        (eo-base-frame frame (heap s))
        (seq-listp xs (heap s) (class-table s))
        )
   (-> (eo-start-state frame xs s)
       (eo-end-state frame xs s)))
  :hints 
  (("Goal"
    :use (:instance every-other-is-every-other-internal-2
          (xs (cddr xs))
          (s (postlude-start-state frame xs s)))
    :in-theory 
    (e/d (eo-preserves-seq-p
          eo-preserves-seq-p-2)
         (internal-frame
          |every_other|-loaded?
          eo-start-state
          prelude-end-state
          recursive-start
          postlude-start-state
          eo-base-frame        
          eo-end-state         
          |Var:root|-get    
          |EmptyList|-p          
          |PersistentList:EMPTY|-get
          |every_other:every_other|-get
          seq-p
          seq-more
          seq-first))
    )))



(defthmd |RT:cons|-Cons-allocs
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s))         
        (|RT:cons|-poised s)
        (seq-p coll (heap s))
        (not (null coll)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (alloc x coll (heap s))))))
    :hints 
    (("Goal"
          :in-theory 
    (e/d ()
         (->-execute-ACONST_NULL
          ->-execute-ALOAD_0
          ->-execute-ALOAD_1
          ->-execute-ALOAD_2
          ->-execute-ARETURN
          ->-execute-DUP
          ->-execute-GETFIELD
          ->-execute-GETSTATIC
          ->-execute-IADD
          ->-execute-ICONST_1
          ->-execute-ICONST_M1
          ->-execute-IFNONNULL
          ->-execute-INSTANCEOF
          ->-execute-INVOKEINTERFACE
          ->-execute-INVOKESPECIAL
          ->-execute-INVOKESTATIC
          ->-execute-INVOKEVIRTUAL
          ->-execute-IRETURN
          ->-execute-NEW
          ->-execute-PUTFIELD    
          ->-execute-RETURN)))))                            
  
(defthmd |RT:cons|-nullref-allocs
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s))         
        (|RT:cons|-poised s)
        (nullrefp coll))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (alloc x nil (heap s))))))
    :hints 
    (("Goal"
      :in-theory 
      (e/d ()
           (->-execute-ACONST_NULL
            ->-execute-ALOAD_0
            ->-execute-ALOAD_1
          ->-execute-ALOAD_2
          ->-execute-ARETURN
          ->-execute-DUP
          ->-execute-GETFIELD
          ->-execute-GETSTATIC
          ->-execute-IADD
          ->-execute-ICONST_1
          ->-execute-ICONST_M1
          ->-execute-IFNONNULL
          ->-execute-INSTANCEOF
          ->-execute-INVOKEINTERFACE
          ->-execute-INVOKESPECIAL
          ->-execute-INVOKESTATIC
          ->-execute-INVOKEVIRTUAL
          ->-execute-IRETURN
          ->-execute-NEW
          ->-execute-PUTFIELD    
          ->-execute-RETURN)))))

(defthm |RT:cons|-alloc
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s))                
        (|RT:cons|-poised s)
        (or (nullrefp coll)
            (and (seq-p coll (heap s))
                 (not (null coll)))))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap 
               (alloc x 
                      (if (nullrefp coll)
                        nil
                        coll)
                      (heap s))))))
    :hints 
    (("Goal"
      :use (|RT:cons|-Cons-allocs           
            |RT:cons|-nullref-allocs))))           