;; Memory Card Game - Test your memory and earn STX
;; Players reveal cards and try to match pairs

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_GAME_NOT_FOUND (err u401))
(define-constant ERR_GAME_FINISHED (err u402))
(define-constant ERR_INVALID_POSITION (err u403))
(define-constant ERR_CARD_ALREADY_REVEALED (err u404))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u405))
(define-constant ERR_GAME_NOT_ACTIVE (err u406)) ;; NEW

(define-constant GAME_FEE u500000) ;; 0.5 STX in microSTX
(define-constant BOARD_SIZE u16) ;; 4x4 grid

(define-data-var game-counter uint u0)

(define-map games
  { game-id: uint }
  {
    player: principal,
    moves: uint,
    matches: uint,
    status: (string-ascii 10),
    start-time: uint,
    end-time: (optional uint), ;; NEW - track when game ends
    revealed-cards: (list 16 uint),
    card-positions: (list 16 uint)
  }
)

(define-map game-rewards
  { game-id: uint }
  { reward-amount: uint, claimed: bool }
)

;; NEW FUNCTION: Forfeit a game and get partial refund
(define-public (forfeit-game (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR_GAME_NOT_FOUND))
      (refund-amount (/ GAME_FEE u2)) ;; 50% refund
    )
    (asserts! (is-eq tx-sender (get player game)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status game) "active") ERR_GAME_NOT_ACTIVE)
    
    (map-set games
      { game-id: game-id }
      (merge game { 
        status: "forfeited",
        end-time: (some block-height)
      })
    )
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get player game))))
    (ok refund-amount)
  )
)

