
;; budget-custom.scm  0.01
;; (c) 2019 Benoit Blancard akabblanc-info@yahoo.fr
;; (c) 2009 Chris Dennis chris@starsoftanalysis.co.uk
;;  ©  2012 Dmitry Smirnov <onlyjob@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

; If you want to adapt this report privately:
; - copy the report to your .gnucash directory
; - specify a different module name below
; - refer to it from .gnucash/config.user
; (see http://wiki.gnucash.org/wiki/Custom_Reports )
(define-module (gnucash report budget-eguile))

(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)
(gnc:module-load "gnucash/engine" 0)

(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(use-modules (gnucash report eguile-utilities))
(use-modules (gnucash report eguile-html-utilities))
(use-modules (gnucash report eguile-gnc))

(use-modules (srfi srfi-13)) ; for extra string functions

(define reportname (N_ "Budget Report"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define all the options

; option pages
(define displaypage  (N_ "Display"))
; option names
; General options
(define optname-report-title    (N_ "Report title"))
(define optname-budget (N_ "Budget"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-use-budget-period-range
  (N_ "Report for range of budget periods"))
(define opthelp-use-budget-period-range
  (N_ "Create report for a budget period range instead of the entire budget."))
(define optname-budget-period-start-exact (N_ "Exact start period"))
(define opthelp-budget-period-start-exact
  (N_ "Select exact period that starts the reporting range."))
(define optname-budget-period-start (N_ "Range start"))
(define opthelp-budget-period-start
  (N_ "Select a budget period type that starts the reporting range."))
(define optname-budget-period-end (N_ "Range end"))
(define opthelp-budget-period-end
  (N_ "Select a budget period type that ends the reporting range."))
(define optname-budget-period-end-exact (N_ "Exact end period"))
(define opthelp-budget-period-end-exact
  (N_ "Select exact period that ends the reporting range."))
(define optname-period-collapse-before (N_ "Include collapsed periods before selected."))
(define opthelp-period-collapse-before (N_ "Include in report previous periods as single collapsed column (one for all periods before starting)"))
(define optname-period-collapse-after (N_ "Include collapsed periods after selected."))
(define opthelp-period-collapse-after (N_ "Include in report further periods as single collapsed column (one for all periods after ending and to the end of budget range)"))

; Display options
(define optname-template-file   (N_ "Template file"))
(define optname-css-file        (N_ "CSS stylesheet file"))
(define optname-show-budget (N_ "Show Budget"))
(define opthelp-show-budget (N_ "Display a column for the budget values."))
(define optname-show-actual (N_ "Show Actual"))
(define opthelp-show-actual (N_ "Display a column for the actual values."))
(define optname-show-difference (N_ "Show Difference"))
(define opthelp-show-difference (N_ "Display the difference as budget - actual."))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals."))
(define optname-rollup-budget (N_ "Roll up budget amounts to parent"))
(define opthelp-rollup-budget (N_ "If parent account does not have its own budget value, use the sum of the child account budget values."))
(define optname-show-zb-accounts (N_ "Include accounts with zero total balances and budget values"))
(define opthelp-show-zb-accounts (N_ "Include accounts with zero total (recursive) balances and budget values in this report."))

;; Account options
(define optname-display-depth    (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))
(define optname-bottom-behavior (N_ "Flatten list to depth limit"))
(define opthelp-bottom-behavior (N_ "Displays accounts which exceed the depth limit at the depth limit."))


;; unused
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-select-columns (N_ "Select Columns"))


;;List of common helper functions, that is not bound only to options generation or report evaluation
(define (set-option-enabled options page opt-name enabled) 
        (gnc-option-db-set-option-selectable-by-name
          options page opt-name enabled))

;; Generation Options
(define (options-generator)
  (define report-options (gnc:new-options))
  (define (add-option new-option)
    (gnc:register-option report-options new-option))

  (define period-options
    (list (list->vector
           (list 'first
                 (N_ "First")
                 (N_ "The first period of the budget")))
          (list->vector
           (list 'previous
                 (N_ "Previous")
                 (N_ "Budget period was before current period, according to report evaluation date")))
          (list->vector
           (list 'current
                 (N_ "Current")
                 (N_ "Current period, according to report evaluation date")))
          (list->vector
           (list 'next
                 (N_ "Next")
                 (N_ "Next period, according to report evaluation date")))
          (list->vector
           (list 'last
                 (N_ "Last")
                 (N_ "Last budget period")))
          (list->vector
           (list 'manual
                 (N_ "Manual period selection")
                 (N_ "Explicitly select period valud with spinner below")))))

  (define ui-use-periods #f)
  (define ui-start-period-type 'current)
  (define ui-end-period-type 'next)

  ;; General options
  (add-option
     (gnc:make-budget-option
        gnc:pagename-general optname-budget
        "a" (N_ "Budget to use.")))

  ;; date interval
    ;;(gnc:options-add-date-interval!
    ;; options gnc:pagename-general
    ;; optname-from-date optname-to-date "a")

  (gnc:options-add-price-source!
     report-options gnc:pagename-general optname-price-source "c" 'pricedb-nearest)

  ;;(gnc:register-option
  ;; options
  ;; (gnc:make-simple-boolean-option
  ;;  gnc:pagename-general optname-show-rates
  ;;  "d" (N_ "Show the exchange rates used") #f))

  (add-option
   (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts).") #t))

  (add-option
    (gnc:make-complex-boolean-option
      gnc:pagename-general
      optname-use-budget-period-range
      "f"
      opthelp-use-budget-period-range
      #f
      #f
      ;; Make period only option widgets
      ;; selectable only when we are running the report for a budget period
      ;; range.
      (lambda (value)
          (let (
                (enabler (lambda (target-opt enabled) 
                      (set-option-enabled report-options gnc:pagename-general target-opt enabled)))
               )
            (for-each (lambda (target-opt) 
                      (enabler target-opt value))
                (list optname-budget-period-start optname-budget-period-end 
                        optname-period-collapse-before optname-period-collapse-after)
            )
            (enabler optname-budget-period-start-exact 
                     (and value 
                          (eq? 'manual ui-start-period-type)))
            (enabler optname-budget-period-end-exact 
                     (and value 
                          (eq? 'manual ui-end-period-type)))
            (set! ui-use-periods value)
        ))))

  (add-option
    (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-start
      "g1.1" opthelp-budget-period-start 'current
      period-options
      #f
      (lambda (new-val)
              (set-option-enabled report-options gnc:pagename-general 
                                  optname-budget-period-start-exact 
                                  (and ui-use-periods (eq? 'manual new-val)))
              (set! ui-start-period-type new-val)
      )    
      ))

  (add-option
    (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-start-exact
      "g1.2" opthelp-budget-period-start-exact
      ;; FIXME: It would be nice if the max number of budget periods (60) was
      ;; defined globally somewhere so we could reference it here.  However, it
      ;; only appears to be defined currently in src/gnome/glade/budget.glade.
      1 1 60 0 1))

  (add-option
    (gnc:make-multichoice-callback-option
      gnc:pagename-general optname-budget-period-end
      "g2.1" opthelp-budget-period-end 'next
      period-options
      #f
      (lambda (new-val)
              (set-option-enabled report-options gnc:pagename-general 
                                  optname-budget-period-end-exact 
                                  (and ui-use-periods (eq? 'manual new-val)))
              (set! ui-end-period-type new-val)
              )      
      ))

  (add-option
    (gnc:make-number-range-option
      gnc:pagename-general optname-budget-period-end-exact
      "g2.2" opthelp-budget-period-end-exact
      ;; FIXME: It would be nice if the max number of budget periods (60) was
      ;; defined globally somewhere so we could reference it here.  However, it
      ;; only appears to be defined currently in src/gnome/glade/budget.glade.
      1 1 60 0 1))

  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-general optname-period-collapse-before
      "g3" opthelp-period-collapse-before #t))

  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-general optname-period-collapse-after
      "g4" opthelp-period-collapse-after #t))

  ;; Accounts options
  (gnc:options-add-account-selection!
     report-options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type
        (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY ACCT-TYPE-INCOME
                          ACCT-TYPE-EXPENSE)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)
  
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior #f))

  ;; Display options
  (add-option (gnc:make-string-option displaypage optname-template-file "a" 
    (N_ "The file name of the eguile template part of this report. This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
    "budget-custom.eguile.scm"))
  (add-option (gnc:make-string-option displaypage optname-css-file "b" 
    (N_ "The file name of the CSS stylesheet to use with this report. This file should either be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.") 
    "taxinvoice.css"))

  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-budget
      "s1" opthelp-show-budget #t))
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-actual
      "s2" opthelp-show-actual #t))
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-difference
      "s3" opthelp-show-difference #f))
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-totalcol
      "s4" opthelp-show-totalcol #f))
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-rollup-budget
      "s4" opthelp-rollup-budget #f))
  (add-option
    (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-zb-accounts
      "s5" opthelp-show-zb-accounts #t))


  ;; Set the general page as default option tab
  (gnc:options-set-default-section
    report-options gnc:pagename-general)

  report-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-specific routines

(define (get-val alist key)
        (let ((lst (assoc-ref alist key)))
          (if lst (car lst) lst))
)

(define (selected-accounts accounts show-subaccts?)
  (let ((all-accounts accounts))
    (if (and (not (null? accounts)) show-subaccts?)
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts all-accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account all-accounts))
                  (set! all-accounts (append accounts sub-accounts))))
            sub-accounts)))

    (sort all-accounts account-full-name<?)
  ))

;; Determines the budget period relative to current period. Budget period is current if
;; it start time <= current time and end time >= current time
;; When period is found it's passed to adjuster that is responsible for final calculation of period.
;; 
;; If budget in future then first period of bundget is returned, if it in past, then the last period is returned
;; if adjuster produced period number that is less then first period or greater than last period, the same rules apply.
;;
;; Parameters:
;;   budget - budget to use
;;   adjuster - function that is used for calculation of period relative to current
(define (find-period-relative-to-current budget adjuster)
    (let* ((now (current-time))
          (total-periods (gnc-budget-get-num-periods budget) )
          (last-period (- total-periods 1))
          (period-start (lambda (x) (gnc-budget-get-period-start-date budget x)))
          (period-end (lambda (x) (gnc-budget-get-period-end-date budget x)))
         )
        (cond ((< now (period-start 0)) 1)
              ((> now (period-end last-period)) total-periods)
              ( else (let ((found-period 
                                (find (lambda (period)
                                              (and (>= now (period-start period)) 
                                                   (<= now (period-end period))))
                                      (iota total-periods))
                          ))
                          (gnc:debug "current period =" found-period)
                          (if found-period 
                              (let ((adjusted (adjuster found-period)))
                                    (cond ((< adjusted 0) 0) ((> adjusted last-period) last-period) (else adjusted))
                              )
                              #f)
             ))
        )
    )
);;end of find-period-relative-to-current

;; Maps type of user selected period to concrete period number, if user not selected to use range false is returned
(define (calc-user-period budget
          use-ranges? period-type period-exact-val )
        (if (not use-ranges?)
            #f
            (cond
                ((eq? 'first period-type) 0)
                ((eq? 'last period-type) (- (gnc-budget-get-num-periods budget) 1))
                ((eq? 'manual period-type) (- period-exact-val 1))
                ((eq? 'previous period-type) 
                    (find-period-relative-to-current budget (lambda (period) (- period 1))))
                ((eq? 'current period-type) 
                    (find-period-relative-to-current budget (lambda (period) period )))
                ((eq? 'next period-type)
                    (find-period-relative-to-current budget (lambda (period) ( + period 1))))
            )
        )
);;end of calc-user-period budget

;; Performs calculation of periods list. If list element is a list itself, it means that 
;; elements of this sublist should be presented as summed value.
;; If user required a total column calculation a quoted total val appended to the end
;; For example if function produced list ( (0 1 2 3 4) 5 6 7 (8 9) 'total) then budget report will 
;; have 6 columns:
;; -- first column is a sum of values for periods 0..4
;; -- second .. forth columns is a values for periods 5,6,7
;; -- fifth is a sum of value for periods 8, 9
;; -- sixth a column with total of all columns
;;
;; Total is calculated only for selected periods. So if the list resulted in (3 4 'total), total column
;; will contain the sum of values for periods 3,4
(define (calc-periods 
          budget user-start user-end collapse-before? collapse-after?)
                 
            (define (range start end)
              (define (int-range current end step lst)
                    (if (>=  current end)
                        lst
                        (int-range (+ current step) end step (cons current lst))))
              (reverse (int-range (if (number? start) start 0) end 1 '()))
            )
  
            (let* ((num-periods (gnc-budget-get-num-periods budget))
                   (range-start (if user-start user-start 0))
                   (range-end (if user-end (+ 1 user-end) num-periods))
                   (fold-before-start 0)
                   (fold-before-end (if collapse-before? range-start 0))
                   (fold-after-start (if collapse-after? range-end num-periods))
                   (fold-after-end num-periods)
                   )
                  (map (lambda (x) (if (and (list? x) (= 1 (length x))) (car x) x))
                       (filter (lambda (x) (not (null? x)))
                                (append (list (range fold-before-start fold-before-end))
                                        (range range-start range-end)
                                        (list (range fold-after-start fold-after-end))
                                        (list 'total)
                    )))
            )
        );;end of define calc-periods

;; 
;; 
;; 
;;
;; Parameters:
;;   budget - budget to use
;;   accounts - list of selected accounts by the user
;;   column-info-list - list of budget periods to use
;;   display-depth - 
;;   rollup-budget?
;;   acct-table-env
;;
;; Return value:
;;   A list of list, one for each account with all information for display
(define 
  (build-accounts-desc-list budget accounts column-info-list display-depth rollup-budget? acct-table-env)
      
      ;; Calculate the value to use for the budget of an account for a specific set of periods.
      ;; If there is 1 period, use that period's budget value.  Otherwise, sum the budgets for
      ;; all of the periods.
      ;;
      ;; Parameters:
      ;;   budget - budget to use
      ;;   acct - account
      ;;   periodlist - list of budget periods to use
      ;;
      ;; Return value:
      ;;   Budget sum
      (define (get-account-periodlist-budget-value budget acct periodlist)
        (cond
         ((= (length periodlist) 1) (gnc:get-account-period-rolledup-budget-value budget acct (car periodlist)))
         (else (gnc-numeric-add (gnc:get-account-period-rolledup-budget-value budget acct (car periodlist))
                                (get-account-periodlist-budget-value budget acct (cdr periodlist))
                                GNC-DENOM-AUTO GNC-RND-ROUND))
         )
        )

      ;; Calculate the value to use for the actual of an account for a specific set of periods.
      ;; This is the sum of the actuals for each of the periods.
      ;;
      ;; Parameters:
      ;;   budget - budget to use
      ;;   acct - account
      ;;   periodlist - list of budget periods to use
      ;;
      ;; Return value:
      ;;   actual sum
      (define (get-account-periodlist-actual-value budget acct periodlist)
        (cond
         ((= (length periodlist) 1)
          (gnc-budget-get-account-period-actual-value budget acct (car periodlist)))
         (else
          (gnc-numeric-add
           (gnc-budget-get-account-period-actual-value budget acct (car periodlist))
           (get-account-periodlist-actual-value budget acct (cdr periodlist))
           GNC-DENOM-AUTO GNC-RND-ROUND))
         )
        )

      ;; Returns a pair composed of the budget and actual amounts for the given
      ;; account and given period.
      ;;
      ;; Parameters:
      ;;   budget - budget to use
      ;;   acct - account being displayed
      ;;   period-list - a list that can contain one or more period
      ;;
      ;; Return value:
      ;;   (budget value . actual value)
      (define (budget-actual-values-for-period
                 budget acct period-list)
        (let* (
            ;; budgeted amount
            (bgt-numeric-val (get-account-periodlist-budget-value budget acct period-list))
  
            ;; actual amount
            (reverse-balance? (gnc-reverse-balance acct))
            (act-numeric-abs (get-account-periodlist-actual-value budget acct period-list))
            (act-numeric-val
              (if reverse-balance?
                (gnc-numeric-neg act-numeric-abs)
                act-numeric-abs))
            )

            (cons bgt-numeric-val act-numeric-val)
        )
      )

      ;; Returns a list of pairs containing budget and actual values for each period
      ;; for given account and period.
      ;;
      ;; Parameters:
      ;;   budget - budget to use
      ;;   acct - account being displayed
      ;;   period-list - a list that can contain one or more period
      ;;
      ;; Return value:
      ;;   a list of pairs
      (define (build-account-values budget acct period-list)
        (let* (
                (bgt-total (gnc-numeric-zero))
                (act-total (gnc-numeric-zero))
              )

              ;; Apply calculations for each period of the list
              (map 
                (lambda (period) 
                  (cond
                    ((equal? period 'total)
                      (list 'total bgt-total act-total))
                    (else
                      (let* ((period-as-list (if (list? period) period (list period)))
                            (bdg-act-values (budget-actual-values-for-period budget acct period-as-list))
                            )
                        ;; Update total values
                        (if (not (gnc-numeric-zero-p (car bdg-act-values)))
                          (set! bgt-total (gnc-numeric-add bgt-total (car bdg-act-values) GNC-DENOM-AUTO GNC-RND-ROUND))
                        )
                        (set! act-total (gnc-numeric-add act-total (cdr bdg-act-values) GNC-DENOM-AUTO GNC-RND-ROUND))
                        
                        (list period (car bdg-act-values) (cdr bdg-act-values)))
                    )
                  )
                )
                period-list)
        )
      )

      ;; Constructs a list with all informations for display for an account
      ;;
      ;; Parameters:
      ;;   budget - budget to use
      ;;   acct-env - account info as return by
      ;;   period-list - list of periods to use
      (define (build-account-line budget acct-env period-list)
        (let* (
            (acct (get-val acct-env 'account))
            (logical-depth (get-val acct-env 'logical-depth))
            (display-depth (get-val acct-env 'display-depth))
            (display-tree-depth (get-val acct-env 'display-tree-depth))
            ;; budgeted, actuals and total amounts
            (values-list (build-account-values budget acct period-list))
          )
          
          (list
            (list
              (list 'account acct)
              (list 'account-name (get-val acct-env 'account-name))
              (list 'logical-depth logical-depth)
              (list 'display-depth display-depth)
              (list 'commodity (xaccAccountGetCommodity acct))
              (list 'acct-values values-list)
              (list 'account-guid (get-val acct-env 'account-guid))))
        )
      );;end of define build-account-line

    ;; Main process
    (let* (
          (acct-table
            (gnc:make-html-acct-table/env/accts acct-table-env accounts))
          (num-rows (gnc:html-acct-table-num-rows acct-table))
          (rownum 0)
          (account-list '())
        )
      
        (while (< rownum num-rows)
            (set! account-list
                          (append
                            account-list
                            (build-account-line
                              budget
                              (gnc:html-acct-table-get-row-env acct-table rownum)
                              column-info-list)))
            (set! rownum (+ rownum 1)) ;; increment rownum
        )
        account-list
    )
) ;; end of build-accounts-desc-list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the report

(define (report-renderer report-obj)
  ;; Create and return the report as either an HTML string 
  ;; or an <html-document>
  (define (opt-value section name)
    ; wrapper for option routines
    (define (get-opt section name)
      (gnc:lookup-option (gnc:report-options report-obj) section name))
    (gnc:option-value (get-opt section name)))

  (gnc:report-starting reportname)

  ; Get all the options
  (let* ((document   (gnc:make-html-document))
        (budget    (opt-value gnc:pagename-general optname-budget))
        (accounts
                   (selected-accounts 
                        (opt-value gnc:pagename-accounts optname-accounts)
                        (opt-value gnc:pagename-accounts optname-show-subaccounts))))
    (cond
      ((null? accounts)
        ;; No accounts selected.
        (begin 
        (gnc:html-document-add-object! 
          document 
          (gnc:html-make-no-account-warning reportname (gnc:report-id report-obj)))
        document))
      ((null? budget)
        ;; No budget selected.
        (begin 
        (gnc:html-document-add-object!
          document (gnc:html-make-generic-budget-warning reportname))
        document))
      (else (begin
        (let* ((opt-template-file  (find-file 
                                      (opt-value displaypage optname-template-file)))
               (html #f)
               (report-name (opt-value gnc:pagename-general
                                              gnc:optname-reportname))
               (use-ranges? (opt-value gnc:pagename-general optname-use-budget-period-range))
               (include-collapse-before? (if use-ranges?
                                             (opt-value gnc:pagename-general optname-period-collapse-before) #f))
               (include-collapse-after? (if use-ranges?
                                             (opt-value gnc:pagename-general optname-period-collapse-after) #f))
               (display-depth (opt-value gnc:pagename-accounts
                                    optname-display-depth))
                (rollup-budget? (opt-value gnc:pagename-display
                                     optname-rollup-budget))
            
               (to-period-val (lambda (v) (inexact->exact (truncate (opt-value gnc:pagename-general v)))))
               (column-info-list 
                  (calc-periods budget 
                    (calc-user-period budget
                                      use-ranges?
                                      (opt-value gnc:pagename-general optname-budget-period-start)
                                      (to-period-val optname-budget-period-start-exact)
                                      )
                    (calc-user-period budget
                                      use-ranges?
                                      (opt-value gnc:pagename-general optname-budget-period-end)
                                      (to-period-val optname-budget-period-end-exact)
                                      )
                    include-collapse-before?
                    include-collapse-after?
                  ))
                (acct-table-env 
                  (list 
                     (list 'start-date (gnc:budget-get-start-date budget))
                     (list 'end-date (gnc:budget-get-end-date budget))
                     (list 'display-tree-depth 
                              (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))
                     (list 'depth-limit-behavior 
                           (if (opt-value gnc:pagename-accounts optname-bottom-behavior) 
                              'flatten 'summarize))
                     (list 'zero-balance-mode 
                           (if (opt-value gnc:pagename-display
                                     optname-show-zb-accounts) 'show-leaf-acct 'omit-leaf-acct))
                     (list 'report-budget budget))
                )
                ;; The list of accounts, balances and budget values to display
                (account-desc-list (build-accounts-desc-list 
                                budget
                                accounts
                                column-info-list
                                display-depth
                                rollup-budget?
                                acct-table-env))
               )

              (set! html (eguile-file-to-string
                            opt-template-file
                            (the-environment)))

              (gnc:report-finished)

              html)
          )
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the report

(gnc:define-report
  'version 1
  'name reportname
  'report-guid "0769e242be474010b4acf264a5512e34"
  'menu-name (N_ "My Budget")
  'menu-tip (N_ "Display a budget report")
  'menu-path (list gnc:menuname-utility)
  'options-generator options-generator
  'renderer report-renderer)
