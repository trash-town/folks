#lang racket/base

(require racket/file
         racket/match)

(define directory-to-read "/home/")

(define exceptions (list "possum"
                         "gemini"))

(define program-name "folks")

(define command-stdout "-stdout")
(define command-out "-o")
(define command-help-short "-h")
(define command-help-human "help")
(define command-help-long "--help")

(define indent "      ")
(define bullet-format "<li><a href=\"https://trash.town/~~~a/\">~a</a></li>")

(define header-title "folks")

(define header
  (format
    #<<string-block
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="style/style.css">
    <link rel="stylesheet" media="screen"
                           href="https://fontlibrary.org/face/fantasque-sans-mono"
                           type="text/css"/>
    <link rel="icon" type="image/png" href="images/favicon.png">
    <title>trash.town - relax.reuse.reboot</title>
  </head>
  <body>
    <img style="float:left" src="images/mascot.png" height=100/>
    <div style="padding-left:5em">
      <h1>trash.town</h1>
      <p><code>relax.reuse.reboot</code></p>
    </div>
    <nav style="clear:both">
      <a href="/">home</a> <>
      <a href="/news.html">news</a> <>
      <a href="/signup.html">signup</a> <>
      <a href="/wiki/">wiki</a> <>
      <a href="/contact.html">contact</a> <>
      <a href="/donate.html">donate</a>
    </nav>
    <h2>~a</h2>
    <ul>
string-block
header-title
))

(define footer
  #<<string-block
    </ul>
  </body>
</html>
string-block
)

(define (make-body)
  ;; the double tilde represents a literal tilde. (string formatting in
  ;; racket/scheme uses `~a`).
  (let* ([bullet-indented (string-append indent bullet-format)]
         [directory-paths (directory-list directory-to-read)]
         [directory-strings (map path->string directory-paths)]
         [folks (filter (lambda (x) (not (member x exceptions)))
                        directory-strings)]
         [folks-formatted (map (lambda (x) (format bullet-indented x x))
                               folks)])
    folks-formatted))

(define (out filename)
  (for ([i (list header (make-body) footer)])
    (if (list? i)
      (for ([x i])
        (display-to-file (string-append x "\n") filename #:exists 'append))
      (display-to-file (string-append i "\n") filename #:exists 'append))))

(define (stdout)
  (for ([i (list header (make-body) footer)])
    (if (list? i)
      (for ([x i])
        (displayln x))
      (displayln i))))

(define (help)
  (for ([line (list "Usage:"
                    (format "  ~a ~a - Print this help message." program-name command-help-human)
                    (format "  ~a ~a - Print HTML to stdout." program-name command-stdout)
                    (format "  ~a ~a <filename> - Write HTML to a file." program-name command-out))])
    (displayln line)))

(define (process-args args)
  (match args
    [(vector (== command-out) filename)
     (out filename)]

    [(vector (== command-stdout))
     (stdout)]

    [(or (vector (== command-help-short))
         (vector (== command-help-human))
         (vector (== command-help-long)))
     (help)]

    [_ (displayln (format "For usage, run '~a ~a'" program-name command-help-human))]))

(define (main args)
  (process-args args))

(main (current-command-line-arguments))
