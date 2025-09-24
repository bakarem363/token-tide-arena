;; Token Tide Arena - Core Smart Contract
;; A cross-game NFT ecosystem with asset evolution and staking

;; Define the NFT trait
(define-non-fungible-token game-asset uint)

;; Define fungible token for rewards
(define-fungible-token arena-token)

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))

;; Data variables
(define-data-var last-token-id uint u0)
(define-data-var total-staked uint u0)

;; Asset data structure
(define-map asset-data 
  { token-id: uint }
  { 
    owner: principal,
    asset-type: (string-ascii 32),
    level: uint,
    experience: uint,
    rarity-score: uint,
    game-origins: (list 5 (string-ascii 32)),
    is-staked: bool,
    stake-start-block: uint
  }
)

;; Staking pools
(define-map staking-pool
  { owner: principal }
  {
    total-staked: uint,
    last-claim-block: uint,
    pending-rewards: uint
  }
)

;; Game registry
(define-map registered-games
  { game-id: (string-ascii 32) }
  {
    is-active: bool,
    reward-multiplier: uint
  }
)

;; Asset rental marketplace
(define-map rental-listings
  { token-id: uint }
  {
    owner: principal,
    renter: principal,
    price-per-block: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool
  }
)

;; Read-only functions

;; Get asset details
(define-read-only (get-asset-data (token-id uint))
  (map-get? asset-data { token-id: token-id })
)

;; Get staking info
(define-read-only (get-staking-info (owner principal))
  (map-get? staking-pool { owner: owner })
)

;; Get last token ID
(define-read-only (get-last-token-id)
  (var-get last-token-id)
)

;; Check if game is registered
(define-read-only (is-game-registered (game-id (string-ascii 32)))
  (match (map-get? registered-games { game-id: game-id })
    game-info (get is-active game-info)
    false
  )
)

;; Get rental listing
(define-read-only (get-rental-listing (token-id uint))
  (map-get? rental-listings { token-id: token-id })
)

;; Public functions

;; Mint new asset
(define-public (mint-asset (recipient principal) (asset-type (string-ascii 32)) (rarity-score uint))
  (let 
    (
      (new-token-id (+ (var-get last-token-id) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (nft-mint? game-asset new-token-id recipient))
    (map-set asset-data 
      { token-id: new-token-id }
      {
        owner: recipient,
        asset-type: asset-type,
        level: u1,
        experience: u0,
        rarity-score: rarity-score,
        game-origins: (list asset-type),
        is-staked: false,
        stake-start-block: u0
      }
    )
    (var-set last-token-id new-token-id)
    (ok new-token-id)
  )
)

;; Transfer asset
(define-public (transfer-asset (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (nft-get-owner? game-asset token-id)) ERR-NOT-FOUND)
    (try! (nft-transfer? game-asset token-id sender recipient))
    (match (map-get? asset-data { token-id: token-id })
      asset-info 
      (map-set asset-data 
        { token-id: token-id }
        (merge asset-info { owner: recipient })
      )
      false
    )
    (ok true)
  )
)

;; Upgrade asset through gameplay
(define-public (upgrade-asset (token-id uint) (experience-gained uint) (game-id (string-ascii 32)))
  (let
    (
      (asset-info (unwrap! (map-get? asset-data { token-id: token-id }) ERR-NOT-FOUND))
      (current-owner (get owner asset-info))
      (new-experience (+ (get experience asset-info) experience-gained))
      (new-level (+ (get level asset-info) (/ new-experience u100)))
    )
    (asserts! (is-eq tx-sender current-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-game-registered game-id) ERR-NOT-AUTHORIZED)
    (map-set asset-data
      { token-id: token-id }
      (merge asset-info 
        { 
          experience: new-experience,
          level: new-level,
          game-origins: (match (as-max-len? (append (get game-origins asset-info) game-id) u5)
                          new-list new-list
                          (get game-origins asset-info))
        }
      )
    )
    (ok true)
  )
)

;; Stake asset for rewards
(define-public (stake-asset (token-id uint))
  (let
    (
      (asset-info (unwrap! (map-get? asset-data { token-id: token-id }) ERR-NOT-FOUND))
      (owner (get owner asset-info))
      (staking-info (default-to 
        { total-staked: u0, last-claim-block: block-height, pending-rewards: u0 }
        (map-get? staking-pool { owner: owner })
      ))
    )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-staked asset-info)) ERR-ALREADY-EXISTS)
    
    ;; Update asset data
    (map-set asset-data
      { token-id: token-id }
      (merge asset-info 
        { 
          is-staked: true,
          stake-start-block: block-height
        }
      )
    )
    
    ;; Update staking pool
    (map-set staking-pool
      { owner: owner }
      (merge staking-info
        {
          total-staked: (+ (get total-staked staking-info) u1),
          last-claim-block: block-height
        }
      )
    )
    
    (var-set total-staked (+ (var-get total-staked) u1))
    (ok true)
  )
)

;; Unstake asset
(define-public (unstake-asset (token-id uint))
  (let
    (
      (asset-info (unwrap! (map-get? asset-data { token-id: token-id }) ERR-NOT-FOUND))
      (owner (get owner asset-info))
      (staking-info (unwrap! (map-get? staking-pool { owner: owner }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (asserts! (get is-staked asset-info) ERR-NOT-FOUND)
    
    ;; Calculate rewards
    (let
      (
        (blocks-staked (- block-height (get stake-start-block asset-info)))
        (reward-amount (* blocks-staked (get rarity-score asset-info)))
      )
      
      ;; Update asset data
      (map-set asset-data
        { token-id: token-id }
        (merge asset-info 
          { 
            is-staked: false,
            stake-start-block: u0
          }
        )
      )
      
      ;; Update staking pool
      (map-set staking-pool
        { owner: owner }
        (merge staking-info
          {
            total-staked: (- (get total-staked staking-info) u1),
            pending-rewards: (+ (get pending-rewards staking-info) reward-amount)
          }
        )
      )
      
      (var-set total-staked (- (var-get total-staked) u1))
      (ok reward-amount)
    )
  )
)

;; Claim staking rewards
(define-public (claim-rewards)
  (let
    (
      (staking-info (unwrap! (map-get? staking-pool { owner: tx-sender }) ERR-NOT-FOUND))
      (rewards (get pending-rewards staking-info))
    )
    (asserts! (> rewards u0) ERR-INSUFFICIENT-BALANCE)
    
    ;; Mint reward tokens
    (try! (ft-mint? arena-token rewards tx-sender))
    
    ;; Reset pending rewards
    (map-set staking-pool
      { owner: tx-sender }
      (merge staking-info { pending-rewards: u0 })
    )
    
    (ok rewards)
  )
)

;; List asset for rental
(define-public (list-for-rental (token-id uint) (price-per-block uint) (duration uint))
  (let
    (
      (asset-info (unwrap! (map-get? asset-data { token-id: token-id }) ERR-NOT-FOUND))
      (owner (get owner asset-info))
    )
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-staked asset-info)) ERR-NOT-AUTHORIZED)
    (asserts! (> price-per-block u0) ERR-INVALID-AMOUNT)
    
    (map-set rental-listings
      { token-id: token-id }
      {
        owner: owner,
        renter: owner, ;; Initially set to owner
        price-per-block: price-per-block,
        start-block: u0,
        end-block: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Rent an asset
(define-public (rent-asset (token-id uint) (duration uint))
  (let
    (
      (listing-info (unwrap! (map-get? rental-listings { token-id: token-id }) ERR-NOT-FOUND))
      (total-cost (* (get price-per-block listing-info) duration))
    )
    (asserts! (get is-active listing-info) ERR-NOT-FOUND)
    (asserts! (not (is-eq tx-sender (get owner listing-info))) ERR-NOT-AUTHORIZED)
    
    ;; Transfer payment (simplified - in real implementation would use STX or other token)
    (map-set rental-listings
      { token-id: token-id }
      (merge listing-info
        {
          renter: tx-sender,
          start-block: block-height,
          end-block: (+ block-height duration),
          is-active: false
        }
      )
    )
    (ok true)
  )
)

;; Admin function to register games
(define-public (register-game (game-id (string-ascii 32)) (reward-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set registered-games
      { game-id: game-id }
      {
        is-active: true,
        reward-multiplier: reward-multiplier
      }
    )
    (ok true)
  )
)

;; Initialize contract
(begin
  ;; Register initial game
  (map-set registered-games
    { game-id: "arena-core" }
    { is-active: true, reward-multiplier: u1 }
  )
)