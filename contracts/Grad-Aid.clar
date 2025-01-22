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
(define-constant ERR_TRANSFER_FAILED 1006)
(define-constant ERR_INVALID_AMOUNT 1007)
(define-constant ERR_INVALID_DUE_DATE 1008)
(define-constant ERR_LOAN_NOT_FOUND 1009)

(define-public (request-loan (amount uint) (interest uint) (due-date uint))
    (begin
        ;; Validate inputs
        (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
        (asserts! (> interest u0) (err ERR_INVALID_AMOUNT))
        (asserts! (> due-date u0) (err ERR_INVALID_DUE_DATE))

        ;; Insert the loan request into the loans map
        (map-insert loans
            { borrower: tx-sender }
            { amount: amount, interest: interest, due-date: due-date, lender: none, repaid: false })
        (var-set last-loan-request (tuple (borrower tx-sender) (amount amount) (interest interest) (due-date due-date)))
        (ok true)
    )
)

(define-public (fund-loan (borrower principal))
    (let 
        (
            (loan-data (unwrap! (map-get? loans { borrower: borrower }) (err ERR_LOAN_NOT_FOUND)))
            (loan-amount (get amount loan-data))
        )
        ;; Ensure the loan is not already funded
        (asserts! (is-none (get lender loan-data)) (err ERR_LOAN_ALREADY_FUNDED))
        (match (stx-transfer? loan-amount tx-sender borrower)
            success 
            (begin
                ;; Update the loan with lender details
                (map-set loans 
                    { borrower: borrower }
                    (merge loan-data { lender: (some tx-sender) })
                )
                (var-set last-loan-funded (tuple (lender tx-sender) (borrower borrower) (amount loan-amount)))
                (ok true)
            )
            error (err ERR_TRANSFER_FAILED)
        )
    )
)

(define-public (repay-loan (lender principal))
    (let 
        (
            (loan-data (unwrap! (map-get? loans { borrower: tx-sender }) (err ERR_LOAN_NOT_FUNDED)))
            (repayment-amount (+ (get amount loan-data) (get interest loan-data)))
        )
        ;; Ensure the loan is funded and not already repaid
        (asserts! (and (is-some (get lender loan-data)) (not (get repaid loan-data))) (err ERR_LOAN_ALREADY_REPAID))
        (match (stx-transfer? repayment-amount tx-sender lender)
            success 
            (begin
                ;; Mark the loan as repaid
                (map-set loans 
                    { borrower: tx-sender }
                    (merge loan-data { repaid: true })
                )
                (var-set last-loan-repaid (tuple (borrower tx-sender) (lender lender) (amount repayment-amount)))
                (ok true)
            )
            error (err ERR_TRANSFER_FAILED)
        )
    )
)

