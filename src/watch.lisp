(defpackage :watch
  (:use :common-lisp)
  (:export
   #:watch
   #:onchange))

(in-package :watch)

(require :cffi)

(defparameter *instance* nil)
(defparameter *watch-descriptors* nil
  "Assoc list. ((wd . (var path callback)) ...)")
(defparameter *onchange-descriptors* nil
  "Assoc list: ((wd . (path callback)) ...)")

(defun init-watch ()
  (when (not *instance*)
    (start-watcher)
    (setf *instance* (inotify-init))))

(cffi:defbitfield event-flags
  (:in-access #x0001)
  :in-modify
  :in-attrib
  :in-close-write
  :in-close-nowrite
  :in-open
  :in-moved-from
  :in-moved-to
  :in-create
  :in-delete
  :in-delete-self
  :in-move-self)

(cffi:defcstruct inotify-event
  (wd :int32)
  (mask :uint32)
  (cookie :uint32)
  (len :uint32))

(cffi:defbitfield open-flags
  (:rdonly #x0000)
  :wronly               ;#x0001
  :rdwr                 ;&hellip;
  :nonblock
  :append
  (:creat  #x0200))

(cffi:defcfun "inotify_rm_watch" :int
  (fd :int)
  (wd :int))

(cffi:defcfun "inotify_add_watch" :int (fd :int) (pathname :string) (mask event-flags))
(cffi:defcfun "inotify_init" :int)

(defun deinit-inotify ()
  (loop for (wd var path) in *watch-descriptors*
        do (progn
             (setf (symbol-value var) nil)
             (inotify-rm-watch wd *instance*)))
  (if *instance*
    (sb-posix:close *instance*)
    (error "Inotify is not initialized."))
  (setf *instance* nil)
  (setf *watch-descriptors* nil))

(defun add-onchange-watch (pathname event-flags callback)
  (init-watch)
  (setf *onchange-descriptors*
        (acons
         (let ((descriptor (inotify-add-watch *instance* pathname event-flags)))
           (if (= descriptor -1)
               (error "Invalid file or incorrect flags.")
               descriptor))
         (list pathname callback) *watch-descriptors*)))

(defun add-watch (var pathname event-flags &optional callback)
  (unless (find-if (lambda (x) (eq var (cadr x))) *watch-descriptors*)
    (init-watch)
    (format t "Flags: ~a Value: ~a" event-flags (cffi:foreign-bitfield-value 'event-flags event-flags))
    (setf *watch-descriptors*
      (acons
          (let ((descriptor (inotify-add-watch *instance* pathname event-flags)))
              (if (= descriptor -1)
                  (error "Invalid file or incorret flags.")
                  descriptor))
          (list var pathname callback) *watch-descriptors*))))

(defun handle-next-event ()
  (when *instance*
    (cffi:with-foreign-pointer (buf 4096 sz)
     (let ((nbytes (sb-posix:read *instance* buf sz)))
      (when (> nbytes 0)
        (loop with event-size = (cffi:foreign-type-size '(:struct inotify-event)) ;; 16
                for offset from 0 below nbytes by event-size
                do (cffi:with-foreign-slots ((wd mask cookie len name) (cffi:inc-pointer buf offset) (:struct inotify-event))
                    (let ((found-list (assoc wd *watch-descriptors*)))
                     (when found-list
                       (setf (symbol-value (cadr found-list)) (uiop:read-file-string (caddr found-list)))
                       (when (fourth found-list)
                         (print found-list)
                         (funcall (fourth found-list)))))
                    (let ((found-list (assoc wd *onchange-descriptors*)))
                      (when found-list
                        (funcall (third found-list)))))))))))

(defun handle-events ()
  (loop while t do (handle-next-event)))

(defmacro watch (&rest triplets)
  "Loads the file into a string bound to the global
   variable var and reloads it whenever the file changes."
  (when triplets
    (let ((path (second (car triplets)))
          (callback (third (car triplets))))
      `(progn
         (add-watch ',(caar triplets) ,path '(:in-modify) ,callback)
         (defparameter ,(caar triplets) (uiop:read-file-string ,path))
         (watch ,@(cdr triplets))))))

(defmacro on-change (&rest pairs)
  (when pairs
    (let ((path (first pairs))
          (callback (second pairs)))
      `(progn
         (add-onchange-watch ,path '(:in-modify) ,callback)
         (on-change ,@(cddr pairs))))))

(defun start-watcher ()
  (sb-thread:make-thread #'handle-events :name "file watcher"))
;(start-watcher)
