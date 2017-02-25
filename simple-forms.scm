;; Copyright (c) 2017, Mario Domenech Goulart All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module simple-forms

(make-field
 field?
 field-var
 field-label
 field-widget
 field-checker
 field-error

 make-form
 form? 
 form-fields
 
 validate-form
 validate-form!
 invalid-fields
 render-form
 render-form/table
 list->form 
 field-values)

(import scheme chicken data-structures)
(use srfi-1)

(define-record field var label widget checker error)

(define %make-field make-field)

(define (make-field var label widget checker #!optional error)
  (%make-field var
               label
               widget
               checker
               error))

(define-record form fields)

(define (validate-form form)
  (make-form
   (map (lambda (field)
          (make-field (field-var field)
                      (field-label field)
                      (field-widget field)
                      (field-checker field)
                      (let ((checker (field-checker field)))
                        (and checker (checker field)))))
        (form-fields form))))

(define (validate-form! form)
  (for-each (lambda (field)
              (let ((checker (field-checker field)))
                (field-error-set! field (and checker (checker field)))))
            (form-fields form))
  form)

(define (invalid-fields form)
  (filter (lambda (field)
            (field-error field))
          (form-fields form)))

(define (render-form form #!optional (attributes '()) (sxml '()))
  `(form (@ ,@attributes)
         ,@(map (lambda (field)
                  `(div (label ,(field-label field)
                               ,((field-widget field) field))))
                (form-fields form))
         ,@sxml))

(define (render-form/table form #!optional (attributes '()) (sxml '()))
  `(table (@ ,@attributes)
          ,@(map (lambda (field)
                   `(tr
                     (td ,(field-label field))
                     (td ,((field-widget field) field))))
                 (form-fields form))
          ,@sxml))

(define (list->form lst)
  (make-form
   (map (lambda (field)
          (apply make-field field))
        lst)))

(define (field-values form)
  (map (lambda (field)
         (let ((var (field-var field)))
           (cons var (conc "$('#" var "').val()"))))
       (form-fields form)))
)
