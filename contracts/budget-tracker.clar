
;; title: Municipal Budget Tracker
;; version: 1.0.0
;; summary: A transparent government spending platform for municipal budget management
;; description: Enables budget allocation, expenditure tracking, contract monitoring, and public reporting

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-DEPARTMENT-NOT-FOUND (err u103))
(define-constant ERR-CONTRACT-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))

;; Data Variables
(define-data-var total-budget uint u0)
(define-data-var total-spent uint u0)
(define-data-var contract-counter uint u0)

;; Data Maps
(define-map departments 
  { dept-id: uint }
  { 
    name: (string-ascii 50),
    allocated-budget: uint,
    spent-amount: uint,
    manager: principal,
    active: bool
  }
)

(define-map expenditures
  { expenditure-id: uint }
  {
    dept-id: uint,
    amount: uint,
    description: (string-ascii 200),
    recipient: principal,
    approved: bool,
    timestamp: uint,
    approver: (optional principal)
  }
)

(define-map government-contracts
  { contract-id: uint }
  {
    dept-id: uint,
    contractor: principal,
    total-value: uint,
    paid-amount: uint,
    description: (string-ascii 200),
    status: (string-ascii 20),
    start-date: uint,
    end-date: uint
  }
)

(define-map authorized-officials principal bool)

;; Public Functions

;; Initialize a new department with budget allocation
(define-public (create-department (dept-id uint) (name (string-ascii 50)) (budget uint) (manager principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> budget u0) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? departments { dept-id: dept-id })) ERR-ALREADY-EXISTS)
    
    (map-set departments 
      { dept-id: dept-id }
      {
        name: name,
        allocated-budget: budget,
        spent-amount: u0,
        manager: manager,
        active: true
      }
    )
    
    (var-set total-budget (+ (var-get total-budget) budget))
    (ok dept-id)
  )
)

;; Record a new expenditure (requires approval)
(define-public (record-expenditure (dept-id uint) (amount uint) (description (string-ascii 200)) (recipient principal))
  (let 
    (
      (dept (unwrap! (map-get? departments { dept-id: dept-id }) ERR-DEPARTMENT-NOT-FOUND))
      (expenditure-id (+ (var-get contract-counter) u1))
    )
    (begin
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get manager dept))) ERR-NOT-AUTHORIZED)
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (asserts! (<= (+ (get spent-amount dept) amount) (get allocated-budget dept)) ERR-INSUFFICIENT-FUNDS)
      
      (map-set expenditures
        { expenditure-id: expenditure-id }
        {
          dept-id: dept-id,
          amount: amount,
          description: description,
          recipient: recipient,
          approved: false,
          timestamp: stacks-block-height,
          approver: none
        }
      )
      
      (var-set contract-counter expenditure-id)
      (ok expenditure-id)
    )
  )
)

;; Approve an expenditure
(define-public (approve-expenditure (expenditure-id uint))
  (let 
    (
      (expenditure (unwrap! (map-get? expenditures { expenditure-id: expenditure-id }) ERR-CONTRACT-NOT-FOUND))
      (dept (unwrap! (map-get? departments { dept-id: (get dept-id expenditure) }) ERR-DEPARTMENT-NOT-FOUND))
    )
    (begin
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                   (default-to false (map-get? authorized-officials tx-sender))) ERR-NOT-AUTHORIZED)
      (asserts! (not (get approved expenditure)) ERR-ALREADY-EXISTS)
      
      ;; Update expenditure as approved
      (map-set expenditures
        { expenditure-id: expenditure-id }
        (merge expenditure { approved: true, approver: (some tx-sender) })
      )
      
      ;; Update department spent amount
      (map-set departments
        { dept-id: (get dept-id expenditure) }
        (merge dept { spent-amount: (+ (get spent-amount dept) (get amount expenditure)) })
      )
      
      ;; Update total spent
      (var-set total-spent (+ (var-get total-spent) (get amount expenditure)))
      
      (ok true)
    )
  )
)

;; Create a government contract
(define-public (create-contract (dept-id uint) (contractor principal) (total-value uint) 
                               (description (string-ascii 200)) (start-date uint) (end-date uint))
  (let 
    (
      (dept (unwrap! (map-get? departments { dept-id: dept-id }) ERR-DEPARTMENT-NOT-FOUND))
      (contract-id (+ (var-get contract-counter) u1))
    )
    (begin
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get manager dept))) ERR-NOT-AUTHORIZED)
      (asserts! (> total-value u0) ERR-INVALID-AMOUNT)
      (asserts! (< start-date end-date) ERR-INVALID-AMOUNT)
      
      (map-set government-contracts
        { contract-id: contract-id }
        {
          dept-id: dept-id,
          contractor: contractor,
          total-value: total-value,
          paid-amount: u0,
          description: description,
          status: "active",
          start-date: start-date,
          end-date: end-date
        }
      )
      
      (var-set contract-counter contract-id)
      (ok contract-id)
    )
  )
)

;; Make payment for a contract
(define-public (make-contract-payment (contract-id uint) (amount uint))
  (let 
    (
      (contract (unwrap! (map-get? government-contracts { contract-id: contract-id }) ERR-CONTRACT-NOT-FOUND))
      (dept (unwrap! (map-get? departments { dept-id: (get dept-id contract) }) ERR-DEPARTMENT-NOT-FOUND))
    )
    (begin
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get manager dept))) ERR-NOT-AUTHORIZED)
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (asserts! (<= (+ (get paid-amount contract) amount) (get total-value contract)) ERR-INSUFFICIENT-FUNDS)
      (asserts! (<= (+ (get spent-amount dept) amount) (get allocated-budget dept)) ERR-INSUFFICIENT-FUNDS)
      
      ;; Update contract paid amount
      (map-set government-contracts
        { contract-id: contract-id }
        (merge contract { paid-amount: (+ (get paid-amount contract) amount) })
      )
      
      ;; Update department spent amount
      (map-set departments
        { dept-id: (get dept-id contract) }
        (merge dept { spent-amount: (+ (get spent-amount dept) amount) })
      )
      
      ;; Update total spent
      (var-set total-spent (+ (var-get total-spent) amount))
      
      (ok true)
    )
  )
)

;; Authorize an official for expenditure approvals
(define-public (authorize-official (official principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-officials official true)
    (ok true)
  )
)

;; Read-only Functions

;; Get department information
(define-read-only (get-department (dept-id uint))
  (map-get? departments { dept-id: dept-id })
)

;; Get expenditure details
(define-read-only (get-expenditure (expenditure-id uint))
  (map-get? expenditures { expenditure-id: expenditure-id })
)

;; Get contract details
(define-read-only (get-contract (contract-id uint))
  (map-get? government-contracts { contract-id: contract-id })
)

;; Get overall budget summary
(define-read-only (get-budget-summary)
  {
    total-budget: (var-get total-budget),
    total-spent: (var-get total-spent),
    remaining-budget: (- (var-get total-budget) (var-get total-spent)),
    utilization-rate: (if (> (var-get total-budget) u0)
                         (* (/ (var-get total-spent) (var-get total-budget)) u100)
                         u0)
  }
)

;; Check if principal is authorized
(define-read-only (is-authorized (principal principal))
  (or (is-eq principal CONTRACT-OWNER)
      (default-to false (map-get? authorized-officials principal)))
)

;; Get department utilization
(define-read-only (get-department-utilization (dept-id uint))
  (match (map-get? departments { dept-id: dept-id })
    dept (let 
           (
             (allocated (get allocated-budget dept))
             (spent (get spent-amount dept))
           )
           (some {
             allocated: allocated,
             spent: spent,
             remaining: (- allocated spent),
             utilization-rate: (if (> allocated u0)
                                  (* (/ spent allocated) u100)
                                  u0)
           })
         )
    none
  )
)

