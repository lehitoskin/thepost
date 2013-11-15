#!/usr/bin/env racket
#lang racket
; thepost.rkt
; simple cli nntp client
; tips taken from: http://download.racket-lang.org/docs/5.0/html/net/nntp.html
;
; Copyright (C) 2013 Lehi Toskin
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require net/nntp
         racket/include)
(include "config.rkt")

#|RACKET-Y DOCUMENTATION - scribble/manual|#

; print out the header/body contents in a sane manner
(define printer
  (λ (lst)
    (if (empty? lst)
        ; empty input list means we raise an error and STOP EVERYTHING
        (raise-argument-error 'printer "populated list" lst)
        ; otherwise print out the list, each element in a newline
        (cond ((empty? (rest lst)) (first lst))
              (else (printf "~a\n" (first lst)) (printer (rest lst)))))))

; shitty bounds checking
(define bounds?
  (λ (start end input)
    ; check numbers
    (cond [(number? input)
           (if (or (< input start) (> input end))
               #f
               #t)]
          ; check strings
          [(string? input)
           (if (or (string=? input start) (string=? input end))
               #t
               #f)]
          ; check symbols
          [(symbol? input)
           (if (or (eq? input start) (eq? input end))
               #t
               #f)]
          (else #f))))

(define nntp-client
  (λ ()
    ; menu - replace with command-line arguments?
    (displayln "Rudimentary NNTP Client!")
    (display "Server to connect to: ")
    (define server (symbol->string (read)))
    ; auth required?
    ; this is messy and ugly and I hate it
    (display "Server require auth? Y/N? ")
    (define auth (bounds? 'Y 'y (read)))
    (if (eq? auth #t)
        [(display "Username: ")
         (set! username (symbol->string (read)))
         (display "Password: ")
         (set! password (symbol->string (read)))]
        (displayln "Skipping..."))
    (display "Newsgroup to read: ")
    (define newsgroup (symbol->string (read)))
    (printf "\n\nsender: ~a\nreceiver: ~s\nserver: ~a\nnewsgroup: ~a\n"
            sender receiver server newsgroup)
    (let ((communicator (connect-to-server server #|port-number|#)))
      ; auth to server
      ; so messy!
      (if (eq? auth #t)
          (authenticate-user communicator username password)
          null)
      ; number range of articles in the newsgroup
      (define-values (total start finish) (open-news-group communicator
                                                           newsgroup))
      ; begin main program loop here!
      ; to add: send an article to the server
      (let loop ()
        (printf
         "\nWhat message would you like to read?\nIndex of ~a from ~a to ~a: "
         total start finish)
        (let [(index (read))]
          (if (bounds? start finish index)
              ; within bounds? continue with the program
              (display "Article found...\n")
              ; out of bounds? make sure we didn't say 'q
              (cond ((number? index)
                     ((printf "~s is out of bounds!\n" index)
                      (loop)))
                    ((bounds? 'q 'quit index)
                     [(display "Good bye!\n")
                      (disconnect-from-server communicator)
                      (exit)])
                    (else ((printf "I don't understand ~s\n" index) (loop)))))
          (display "Choices: header | body | quit: ")
          (let [(choice (read))]
            (if (bounds? 'q 'quit choice)
                [(display "Good bye!\n")
                 (disconnect-from-server communicator)
                 (exit)]
                ; print the header of the article
                (cond [(string=? (symbol->string choice) "header")
                       (let ((headers (head-of-message communicator index)))
                         (newline)
                         (printer headers))]
                      ; print the body of the article
                      [(string=? (symbol->string choice) "body")
                       (let ((bodies (body-of-message communicator index)))
                         (newline)
                         (printer bodies))]
                      ; complain about the input
                      (else (printf "I don't understand ~a" (symbol->string
                                                             choice)))))))
        (loop)))))

(nntp-client)
