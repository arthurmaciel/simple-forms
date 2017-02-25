(use test simple-forms)

(define invalid-request-vars
  '((foo . "")
    (bar . "abc")
    (baz . "")))

(define (get-value var)
  (alist-ref var invalid-request-vars))

(define (text-input field)
  (let ((var (field-var field))
        (error (field-error field)))
    `(input (@ (type "text")
               (id ,(->string var))
               (name ,(->string var))
               (value ,(or error ""))))))

;; If a checker succeeds, it returns #f 
(define (check-mandatory field)
  (and (equal? (->string (get-value (field-var field))) "")
       (conc (field-label field) " must be filled.")))

(define (check-number field)
  (and (not (string->number (get-value (field-var field))))
       (conc (field-label field) " must be a number.")))

;; The form object
(define my-form
  (list->form
   `((foo "Foo" ,text-input ,check-mandatory) ;; foo is mandatory
     (bar "Bar" ,text-input ,check-number) ;; bar is required to be a number
     (baz "Baz" ,text-input #f) ;; baz is optional
     )))

(test "list->form"
      #t
      (form? my-form))

(test "validate-form"
      '("Foo must be filled." "Bar must be a number." #f)
      (map (lambda (field)
             (field-error field))
           (form-fields (validate-form my-form))))

(test "validate-form!"
      '("Foo must be filled." "Bar must be a number." #f)
      (begin
        (validate-form! my-form)
        (map (lambda (field)
               (field-error field))
             (form-fields my-form))))

(test "invalid-fields"
      2
      (length (invalid-fields my-form)))

(test "render-form"
      '(form (@ (id "form"))
             (div (label "Foo"
                         (input (@ (type "text")
                                   (id "foo")
                                   (name "foo")
                                   (value "Foo must be filled.")))))
             (div (label "Bar"
                         (input (@ (type "text")
                                   (id "bar")
                                   (name "bar")
                                   (value "Bar must be a number.")))))
             (div (label "Baz"
                         (input (@ (type "text")
                                   (id "baz")
                                   (name "baz")
                                   (value ""))))))
      (render-form my-form '((id "form"))))

(test "render-form/table"
      '(table (@ (id "form"))
              (tr (td "Foo")
                  (td (input (@ (type "text")
                                (id "foo")
                                (name "foo")
                                (value "Foo must be filled.")))))
              (tr (td "Bar")
                  (td (input (@ (type "text")
                                (id "bar")
                                (name "bar")
                                (value "Bar must be a number.")))))
              (tr (td "Baz")
                  (td (input (@ (type "text")
                             (id "baz")
                             (name "baz")
                             (value "")))))
              (button (@ (type "submit"))
                      "Enviar"))
      (render-form/table my-form
                         '((id "form"))
                         '((button (@ (type "submit"))
                                   "Enviar"))))






