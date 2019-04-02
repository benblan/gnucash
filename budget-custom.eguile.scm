<!doctype html>

<?scm
;; budget-custom.eguile.scm  0.01
;; (c) 2019 Benoit Blancard akabblanc-info@yahoo.fr
;;
;; This file is a mixture of HTML and Guile --
;; see eguile-gnc.scm for details.
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

(let* ((x 42) ; only here to allow (define)s
              ; i.e. to avoid "Bad define placement" error
      (show-actual?     (opt-value gnc:pagename-display optname-show-actual))
      (show-budget?     (opt-value gnc:pagename-display optname-show-budget))
      (show-difference? (opt-value gnc:pagename-display optname-show-difference))
      (show-totalcol?   (opt-value gnc:pagename-display optname-show-totalcol))
      (col-span (+ (if show-budget? 1 0)
                   (if show-actual? 1 0)
                   (if show-difference? 1 0)))
      (current-row 0)

      (period-to-date-string (lambda (p) (qof-print-date (gnc-budget-get-period-start-date budget p))))

      (display-monetary-budget (lambda (num-val comm)
        (if (gnc-numeric-zero-p num-val)
          "."
          (gnc:monetary->string (gnc:make-gnc-monetary comm num-val)))))
      
      (display-monetary-actual (lambda (num-val comm)
        (gnc:monetary->string (gnc:make-gnc-monetary comm num-val))))

      (display-monetary-diff (lambda (bgt-numeric-val act-numeric-val diff-numeric-val comm)
        (if (and (gnc-numeric-zero-p bgt-numeric-val) (gnc-numeric-zero-p act-numeric-val))
          "."
          (gnc:monetary->string (gnc:make-gnc-monetary comm diff-numeric-val)))))

      (period-header->string (lambda (period)
        (if (list? period)
            (string-append 
              (period-to-date-string (car period)) 
              " &ndash; " 
              (period-to-date-string (car (reverse period))))
            (period-to-date-string period))))

      (display-cell-style (lambda (total? border-incr)
        (let ((style (if total? "number-cell total" "number-cell"))
              )
            
              (string-append style
                (cond 
                  ((equal? col-span 1) " one-cell")
                  ((equal? col-span 2) (if (equal? border-incr 1) " left-cell" " right-cell"))
                  (else (if (equal? border-incr 1) " left-cell" (if (equal? border-incr 2) " middle-cell" " right-cell")))
                )
              )
        )))

      (display-value-style (lambda (acct-type diff? val)
        (if diff?
            (if (gnc-numeric-negative-p val) " class=\"neg\"" "")
            (if (gnc-numeric-negative-p val)
              (if (equal? acct-type ACCT-TYPE-EXPENSE) ; for an expense a negative is a good thing
                " class=\"pos\"" " class=\"neg\"")
              "")
        )
      ))


      (get-diff-val (lambda (budget-val actual-val acct-type)
        (if (equal? acct-type ACCT-TYPE-INCOME)
          ; for income we want to have a minus if actual is less than expected
          (gnc-numeric-sub
              actual-val budget-val
              GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))
          (gnc-numeric-sub
              budget-val actual-val
              GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER)))
      ))

      (get-tooltip (lambda (account-name period)
        (if (not (equal? period 'total))
          (string-append "title=\"" account-name " - " (period-header->string period) "\"")
          "")
      ))

      )

      (define (custom:html-make-nbsps n)
          (if (> n 0)
            (string-append "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" (custom:html-make-nbsps (- n 1)))
            "")
      )
?>

<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

    <title><?scm:d report-name ?> : <?scm:d (gnc-budget-get-name budget) ?></title>

    <style type="text/css">
      body { font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
      h3 { font-size: 15pt; font-weight: bold;  }
      table.budget { border-collapse: separate; border-spacing: 1px 2px }
      tr.alternate-row { background: #fce94f }
      tr { page-break-inside: avoid !important;}
      th.period-header { text-align: center; font-size: 12pt; font-weight: bold; border-style: solid; border-width: 2px; }
      th.values-header { text-align: center; font-weight: bold;  }
      .col-period { background-color: white; }
      .col-total { background-color: #ffe8d4; }

      tr.depth-0 { background-color: #ccccff; }
      tr.depth-1 { background-color: #FCF19F; }
      tr.depth-2 { background-color: white; }
      tr.depth-3 { background-color: white; }
      tr.depth-4 { background-color: white; }
      tr.account:hover { background-color: #e0e0d1; }

      td { white-space: nowrap; }
      td.number-cell { text-align: right; }
      td.total { font-weight: bold; }
      td.anchor-cell { text-align: left; font-style: italic;}

      td.one-cell {
        border-style: none solid none solid; border-width: 2px }
      td.left-cell {
        border-style: none none none solid; border-width: 2px }
      td.middle-cell {
        border-style: none none none none; border-width: 2px }
      td.right-cell { 
        border-style: none solid none none; border-width: 2px }

      .neg { color: red;  }
      .pos { color: green;  }

    </style>
  </head>
  
  <body>

    <div class="main">
    <h3><?scm:d report-name ?> : <?scm:d (gnc-budget-get-name budget) ?></h3>

    <table class="budget">
      <colgroup>
        <col>
        <col>
        <col>
        <?scm (for period in column-info-list do ?>
        <?scm (cond
                ((equal? period 'total)
                    (if show-totalcol? (begin ?><col span="3" class="col-total"><?scm )))
                (else (begin ?><col span="3" class="col-period"><?scm ))) ?>
        <?scm ) ?>
      </colgroup>

      <thead>
        <tr><!-- Row for periods -->
          <th> </th>
          <th> </th>
          <th> </th>
          <?scm (for period in column-info-list do ?>
          <?scm (cond
                  ((equal? period 'total)
                      (if show-totalcol? (begin ?><th class="period-header" colspan="<?scm:d col-span ?>"><?scm:d (_ "Total") ?></td><?scm )))
                  (else (begin ?>
                   <th class="period-header" colspan="<?scm:d col-span ?>"><?scm:d (period-header->string period) ?></th>
                <?scm ))) ?>
          <?scm ) ?>
        </tr>

        <tr><!-- Row for Budget / Actual / Diff : headers -->
          <th> </th>
          <th> </th>
          <th> </th>
          <?scm (for period in column-info-list do
            (if (or (not (equal? period 'total)) show-totalcol?) (begin ?>
          <?scm (if show-budget? (begin ?><th class="values-header"><?scm:d (_ "Bgt") ?></th> <?scm )) ?>
          <?scm (if show-actual? (begin ?><th class="values-header"><?scm:d (_ "Act") ?></th> <?scm )) ?>
          <?scm (if show-difference? (begin ?><th class="values-header"><?scm:d (_ "Diff") ?></th> <?scm )) ?>
                
          <?scm ))) ?>
        </tr>
      </thead>

      <tbody>
        <!-- Row for each account -->
        <?scm (for acct-desc in account-desc-list do 
                (let* ((name (get-val acct-desc 'account-name))
                      (acct (get-val acct-desc 'account))
                      (display-depth (get-val acct-desc 'display-depth))
                      (comm (xaccAccountGetCommodity acct))
                      (acct-type (xaccAccountGetType acct))
                      (values-list (get-val acct-desc 'acct-values))) ?>
        <tr class="account depth-<?scm:d display-depth ?>">
          <td class="anchor-cell"><?scm:d (custom:html-make-nbsps display-depth) ?><a href="gnc-register:acct-guid=<?scm:d (get-val acct 'account-guid) ?>#"><?scm:d name ?></a></td>
          <td></td>
          <td></td>
          <?scm (for value in values-list do
                  ; display if category is not total or if total can be shown
                  (if (or (not (equal? (car value) 'total)) show-totalcol?)
                  (let* ((total? (equal? (car value) 'total))
                        (budget-val (cadr value))
                        (actual-val (caddr value))
                        (diff-val
                          (get-diff-val budget-val actual-val acct-type))
                        (border-incr 0)) ?>
          <?scm (if show-budget? (begin (set! border-incr (+ border-incr 1)) ?>
            <td class="<?scm:d (display-cell-style total? border-incr) ?>" <?scm:d (get-tooltip name (car value)) ?>>
              <span <?scm:d (display-value-style acct-type #f budget-val) ?>><?scm:d (display-monetary-budget budget-val comm) ?></span></td><?scm )) ?>
          <?scm (if show-actual? (begin (set! border-incr (+ border-incr 1)) ?>
            <td class="<?scm:d (display-cell-style total? border-incr) ?>" <?scm:d (get-tooltip name (car value)) ?>>
              <span <?scm:d (display-value-style acct-type #f actual-val) ?>><?scm:d (display-monetary-actual actual-val comm) ?></span></td><?scm )) ?>
          <?scm (if show-difference? (begin (set! border-incr (+ border-incr 1)) ?>
            <td class="<?scm:d (display-cell-style total? border-incr) ?>" <?scm:d (get-tooltip name (car value)) ?>>
              <span <?scm:d (display-value-style acct-type #t diff-val) ?>><?scm:d (display-monetary-diff budget-val actual-val diff-val comm) ?></span></td><?scm )) ?>
          <?scm ))) ?>
        </tr>
        <?scm )) ?>
      </tbody>

    </table>
    
    </div>
  </body>
</html>
<?scm
) ; end of enclosing let
?>
