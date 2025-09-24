;; ChainTrace - Supply Chain Transparency Platform
;; A simple smart contract for product tracking and authenticity verification

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-product (err u104))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data structures
(define-map products
  { product-id: (string-ascii 64) }
  {
    manufacturer: principal,
    digital-fingerprint: (string-ascii 128),
    creation-timestamp: uint,
    current-location: (string-ascii 64),
    status: (string-ascii 32),
    carbon-footprint: uint,
    verified: bool
  }
)

(define-map supply-chain-events
  { event-id: uint }
  {
    product-id: (string-ascii 64),
    event-type: (string-ascii 32),
    location: (string-ascii 64),
    timestamp: uint,
    stakeholder: principal,
    verified: bool
  }
)

(define-map stakeholders
  { stakeholder-address: principal }
  {
    role: (string-ascii 32),
    verified: bool,
    reputation-score: uint
  }
)

;; Data variables
(define-data-var event-counter uint u0)
(define-data-var total-products uint u0)

;; Read-only functions
(define-read-only (get-product (product-id (string-ascii 64)))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-supply-chain-event (event-id uint))
  (map-get? supply-chain-events { event-id: event-id })
)

(define-read-only (get-stakeholder (stakeholder-address principal))
  (map-get? stakeholders { stakeholder-address: stakeholder-address })
)

(define-read-only (get-total-products)
  (var-get total-products)
)

(define-read-only (get-event-counter)
  (var-get event-counter)
)

;; Private functions
(define-private (is-authorized-stakeholder (stakeholder principal))
  (match (map-get? stakeholders { stakeholder-address: stakeholder })
    stakeholder-data (get verified stakeholder-data)
    false
  )
)

;; Public functions

;; Register a new stakeholder
(define-public (register-stakeholder (stakeholder-address principal) (role (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? stakeholders { stakeholder-address: stakeholder-address })) err-already-exists)
    (map-set stakeholders
      { stakeholder-address: stakeholder-address }
      {
        role: role,
        verified: true,
        reputation-score: u100
      }
    )
    (ok true)
  )
)

;; Create a new product
(define-public (create-product 
  (product-id (string-ascii 64))
  (digital-fingerprint (string-ascii 128))
  (initial-location (string-ascii 64))
  (initial-carbon-footprint uint))
  (begin
    (asserts! (is-authorized-stakeholder tx-sender) err-unauthorized)
    (asserts! (is-none (map-get? products { product-id: product-id })) err-already-exists)
    (map-set products
      { product-id: product-id }
      {
        manufacturer: tx-sender,
        digital-fingerprint: digital-fingerprint,
        creation-timestamp: block-height,
        current-location: initial-location,
        status: "created",
        carbon-footprint: initial-carbon-footprint,
        verified: false
      }
    )
    (var-set total-products (+ (var-get total-products) u1))
    (ok true)
  )
)

;; Verify a product's authenticity
(define-public (verify-product (product-id (string-ascii 64)))
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) err-not-found)))
    (asserts! (is-authorized-stakeholder tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      (merge product-data { verified: true })
    )
    (ok true)
  )
)

;; Add a supply chain event
(define-public (add-supply-chain-event
  (product-id (string-ascii 64))
  (event-type (string-ascii 32))
  (location (string-ascii 64)))
  (let ((current-event-id (+ (var-get event-counter) u1)))
    (asserts! (is-authorized-stakeholder tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? products { product-id: product-id })) err-not-found)
    (map-set supply-chain-events
      { event-id: current-event-id }
      {
        product-id: product-id,
        event-type: event-type,
        location: location,
        timestamp: block-height,
        stakeholder: tx-sender,
        verified: true
      }
    )
    (var-set event-counter current-event-id)
    ;; Update product location if it's a movement event
    (if (is-eq event-type "moved")
      (let ((product-data (unwrap! (map-get? products { product-id: product-id }) err-not-found)))
        (map-set products
          { product-id: product-id }
          (merge product-data { current-location: location })
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Update product status
(define-public (update-product-status 
  (product-id (string-ascii 64))
  (new-status (string-ascii 32)))
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) err-not-found)))
    (asserts! (is-authorized-stakeholder tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      (merge product-data { status: new-status })
    )
    (ok true)
  )
)

;; Update carbon footprint
(define-public (update-carbon-footprint 
  (product-id (string-ascii 64))
  (additional-footprint uint))
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) err-not-found)))
    (asserts! (is-authorized-stakeholder tx-sender) err-unauthorized)
    (map-set products
      { product-id: product-id }
      (merge product-data { 
        carbon-footprint: (+ (get carbon-footprint product-data) additional-footprint) 
      })
    )
    (ok true)
  )
)

;; Update stakeholder reputation
(define-public (update-stakeholder-reputation 
  (stakeholder-address principal)
  (new-score uint))
  (let ((stakeholder-data (unwrap! (map-get? stakeholders { stakeholder-address: stakeholder-address }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set stakeholders
      { stakeholder-address: stakeholder-address }
      (merge stakeholder-data { reputation-score: new-score })
    )
    (ok true)
  )
)

;; Emergency function to revoke stakeholder access
(define-public (revoke-stakeholder (stakeholder-address principal))
  (let ((stakeholder-data (unwrap! (map-get? stakeholders { stakeholder-address: stakeholder-address }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set stakeholders
      { stakeholder-address: stakeholder-address }
      (merge stakeholder-data { verified: false })
    )
    (ok true)
  )
)