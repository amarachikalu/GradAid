(define-map loans
    { borrower: principal }
    { amount: uint, interest: uint, due-date: uint, lender: (optional principal), repaid: bool })

(define-data-var last-loan-request 
    (tuple (borrower principal) (amount uint) (interest uint) (due-date uint))
    (tuple (borrower 'SP000000000000000000002Q6VF78) (amount u0) (interest u0) (due-date u0)))

(define-data-var last-loan-funded
    (tuple (lender principal) (borrower principal) (amount uint))
    (tuple (lender 'SP000000000000000000002Q6VF78) (borrower 'SP000000000000000000002Q6VF78) (amount u0)))

(define-data-var last-loan-repaid
    (tuple (borrower principal) (lender principal) (amount uint))
    (tuple (borrower 'SP000000000000000000002Q6VF78) (lender 'SP000000000000000000002Q6VF78) (amount u0)))

(define-constant ERR_NOT_BORROWER 1001)
(define-constant ERR_LOAN_ALREADY_FUNDED 1002)
(define-constant ERR_LOAN_NOT_FUNDED 1003)
(define-constant ERR_LOAN_ALREADY_REPAID 1004)
(define-constant ERR_NOT_ENOUGH_FUNDS 1005)

(define-public (request-loan (amount uint) (interest uint) (due-date uint))
    (begin
        (asserts! (> amount u0) (err ERR_NOT_ENOUGH_FUNDS))
        (map-insert loans
            { borrower: tx-sender }
            { amount: amount, interest: interest, due-date: due-date, lender: none, repaid: false })
        (var-set last-loan-request (tuple (borrower tx-sender) (amount amount) (interest interest) (due-date due-date)))
        (ok true)
    )
)

(define-public (fund-loan (borrower principal))
    (let ((loan (map-get? loans { borrower: borrower })))
        (match loan
            loan-data
            (if (is-none (get lender loan-data))
                (let ((amount (get amount loan-data)))
                    (try! (stx-transfer? amount tx-sender borrower))
                    (map-set loans { borrower: borrower } 
                        (merge loan-data { lender: (some tx-sender) }))
                    (var-set last-loan-funded (tuple (lender tx-sender) (borrower borrower) (amount amount)))
                    (ok true))
                (err ERR_LOAN_ALREADY_FUNDED))
            (err ERR_NOT_BORROWER))
    )
)

(define-public (repay-loan (lender principal))
    (let ((loan (map-get? loans { borrower: tx-sender })))
        (match loan
            loan-data
            (if (and (is-some (get lender loan-data)) (not (get repaid loan-data)))
                (let ((amount (+ (get amount loan-data) (get interest loan-data))))
                    (try! (stx-transfer? amount tx-sender lender))
                    (map-set loans { borrower: tx-sender } 
                        (merge loan-data { repaid: true }))
                    (var-set last-loan-repaid (tuple (borrower tx-sender) (lender lender) (amount amount)))
                    (ok true))
                (err ERR_LOAN_ALREADY_REPAID))
            (err ERR_LOAN_NOT_FUNDED))
    )
)