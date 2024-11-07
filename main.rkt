#lang racket/base

(require net/http-easy)
(require json)
(require (only-in racket/file file->string))
(require (only-in racket/contract/base listof))
(require (only-in racket/list
                  append-map
                  range))
(require racket/format)

;;TODO get rid of internals
(provide all-defined-out)

;; Base URL of the moodle server
(define current-base-url (make-parameter "urcourses.uregina.ca"))

;; Location of a text file containing your private Moodle access token
(define current-token (make-parameter (file->string "token.txt")))

;;Symbol for what field is used to map users to their Moodle IDs
;; e.g. if their handin username us their email address, this should be 'email
(define current-moodle-ident-field (make-parameter 'email))

;; Moodle Course ID for the course you're dealing with
(define current-courseid (make-parameter 33282))

;; Take a jsexpr? and turn it into a sequence of symbol-string pairs
;; that can be given as an argument to easy-http PUT.
;; We use concatMap to flatten objects/arrays
;; json->queryStrings : -> jsexpr? (Listof query-params/c)
(define (json->queryStrings lvalueStr js)
  (cond
    [((listof jsexpr?) js)
     (append-map (lambda (i)
                   (json->queryStrings (string-append lvalueStr "[" (~v i) "]") (list-ref js i)))
                 (range 0 (length js)))]
    [(hash? js)
     (append-map (lambda (i)
                   (json->queryStrings (string-append lvalueStr "[" (symbol->string i) "]") (hash-ref js i)))
                 (hash-keys js))]
    [(string? js) (list (cons (string->symbol lvalueStr) js))]
    ;; Otherwise just make the value-string pair
    [else (list (cons (string->symbol lvalueStr) (~v js)))]))

(define (json->queryString js)
  (cond [(hash? js)
         (append-map
          (lambda (i) (json->queryStrings (symbol->string i) (hash-ref js i)))
          (hash-keys js))]
        [(null? js) '()]
        [else (error "Query parameters must be JSON dictionary of param names and values")]))
 

;; Post the given moodle function name to the server,
;; with a jsonexp? for parameters
(define (moodlePost function params #:json [json '()])
  (response-json
   (post (string-append (current-base-url) "/webservice/rest/server.php")
         #:params
         `((moodlewsrestformat . "json")
           (wsfunction . ,function)
           (moodlewssettingfilter . "true")
           (moodlewssettingfileurl . "true")
           (wstoken . ,(current-token))
           . ,(json->queryString params))
         #:json json)))

;; Post the given moodle function name to the server,
;; in the context of the current course,
;; with a jsonexp? for parameters
;; (define (moodlePostCourse function params #:json [json '()])
;;   (response-json
;;    (post (string-append (current-base-url) "/webservice/rest/server.php")
;;          #:params
;;          `((moodlewsrestformat . "json")
;;            (wsfunction . ,function)
;;            (moodlewssettingfilter . "true")
;;            (moodlewssettingfileurl . "true")
;;            (wstoken . ,(current-token))
;;            (courseid . ,(current-courseid))
;;            . ,params)
;;          #:json json)))



(define (get-assignments)
  (moodlePost
   "mod_assign_get_assignments"
   '()
   ))




;; Mapping from email addresses to moodle user ID numbers.
;; Initially empty, populated by queryiung the server.
;; The parameter (current-moodle-ident-field)
;; determines which field of the user's profile is used to identify them, e.g. as their handin id.
;; By default it's their email address.

;; Hashof Email MoodleIdNum
(define current-user-info
  (make-hash))

;; make-userinfo-dict : -> Hashof StudentNumber MoodleId
;; Query the moodle server to get the list of users in the current course
(define (make-userinfo-dict)
  (define ulist (moodlePost
                 "core_enrol_get_enrolled_users"
                 (hasheq 'courseid (current-courseid))))
  (for ([rec ulist])
    (hash-set! current-user-info (hash-ref rec (current-moodle-ident-field))

               rec)))

;; Hashof Email MoodleIdNum
(define current-assigns-info
  (make-hash))


;; Query the moodle server to get the list of assignments for the current course
(define (make-assigninfo-dict)
  (define assignDict (moodlePost
   "mod_assign_get_assignments"
   (hasheq 'courseids (list (current-courseid)))
   ))
  (define assignsList (hash-ref (car (hash-ref assignDict 'courses)) 'assignments))
  (for ([rec assignsList])
    ;;Identify assignments by their cmid, e.g. the number in the URL
    ;; Not sure why this is different from the ID, but it is
    (hash-set! current-assigns-info (hash-ref rec 'cmid) rec)))

;; -> Ident MoodleIDNum
;; Map a student's hanin username to their moodle ID number,
;; querying the server if it's not found.
(define (ident->moodleId ident)
  ;; If the ID isn't found, then update the list of student IDs from the server
  (when (not (hash-has-key? current-user-info ident))
    (make-userinfo-dict))
  ;; Regardless, get the id
  ;; Something is broken if this fails
  (hash-ref
   (hash-ref current-user-info ident)
   'id))

;; -> AssignId MoodleIDNum
;; Map an assignment's cmid to it's id number.
;; No clue what the difference is between these IDs, but you need to do it
(define (cmid->internalId ident)
  ;; If the ID isn't found, then update the list of student IDs from the server
  (when (not (hash-has-key? current-assigns-info ident))
    (make-assigninfo-dict))
  ;; Regardless, get the id
  ;; Something is broken if this fails
  (hash-ref
   (hash-ref current-assigns-info ident)
   'id))

;; Upload the feedback file for the given user
(define (upload-grade #:user userIdent #:assignId assignId #:grade gradeValue feedbackStr)
  (moodlePost "mod_assign_save_grade"
          (hasheq
           'assignmentid (cmid->internalId assignId)
           'userid (ident->moodleId userIdent)
           'grade  (number->string gradeValue)
           'attemptnumber  -1
           'addattempt 0
           'workflowstate  "TODO"
           'applytoall  0
           'plugindata (hasheq 'assignfeedbackcomments_editor
                               (hasheq 'text feedbackStr 'format 0)) )))


;137211

;(upload-grade "eremondj+urstudent@uregina.ca" 137211 21 "This is a test")