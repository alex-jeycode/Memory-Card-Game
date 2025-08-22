;; Memory Card Game - Test your memory and earn STX
;; Players reveal cards and try to match pairs

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_GAME_NOT_FOUND (err u401))
(define-constant ERR_GAME_FINISHED (err u402))
(define-constant ERR_INVALID_POSITION (err u403))
(define-constant ERR_CARD_ALREADY_REVEALED (err u404))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u405))
(define-constant ERR_GAME_NOT_ACTIVE (err u406))

(define-constant GAME_FEE u500000) ;; 0.5 STX in microSTX
(define-constant BOARD_SIZE u16) ;; 4x4 grid

(define-data-var game-counter uint u0)
(define-data-var total-rewards-paid uint u0)

(define-map games
  { game-id: uint }
  {
    player: principal,
    moves: uint,
    matches: uint,
    status: (string-ascii 10),
    start-time: uint,
    end-time: (optional uint),
    revealed-cards: (list 16 uint),
    card-positions: (list 16 uint)
  }
)

(define-map game-rewards
  { game-id: uint }
  { reward-amount: uint, claimed: bool }
)

(define-map player-stats
  { player: principal }
  {
    total-games: uint,
    completed-games: uint,
    total-moves: uint,
    best-moves: uint,
    total-rewards: uint
  }
)

(define-map leaderboard
  { position: uint }
  { player: principal, best-score: uint }
)

;; Start a new game
(define-public (start-game)
  (let
    (
      (game-id (+ (var-get game-counter) u1))
      (shuffled-deck (list u1 u1 u2 u2 u3 u3 u4 u4 u5 u5 u6 u6 u7 u7 u8 u8))
    )
    (try! (stx-transfer? GAME_FEE tx-sender (as-contract tx-sender)))
    (var-set game-counter game-id)
    (map-set games
      { game-id: game-id }
      {
        player: tx-sender,
        moves: u0,
        matches: u0,
        status: "active",
        start-time: block-height,
        end-time: none,
        revealed-cards: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0),
        card-positions: shuffled-deck
      }
    )
    (update-player-game-start tx-sender)
    (ok game-id)
  )
)

;; Reveal a card at a specific position
(define-public (reveal-card (game-id uint) (position uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR_GAME_NOT_FOUND))
      (revealed-list (get revealed-cards game))
      (card-value (unwrap! (element-at (get card-positions game) position) ERR_INVALID_POSITION))
    )
    (asserts! (is-eq tx-sender (get player game)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status game) "active") ERR_GAME_FINISHED)
    (asserts! (< position BOARD_SIZE) ERR_INVALID_POSITION)
    (asserts! (is-eq (unwrap-panic (element-at revealed-list position)) u0) ERR_CARD_ALREADY_REVEALED)
    
    (let
      (
        (new-revealed (map replace-at-index revealed-list position u1))
        (new-moves (+ (get moves game) u1))
      )
      (map-set games
        { game-id: game-id }
        (merge game {
          moves: new-moves,
          revealed-cards: new-revealed
        })
      )
      (ok { card: card-value, position: position })
    )
  )
)

;; Check if two revealed cards match
(define-public (check-match (game-id uint) (pos1 uint) (pos2 uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR_GAME_NOT_FOUND))
      (card1 (unwrap-panic (element-at (get card-positions game) pos1)))
      (card2 (unwrap-panic (element-at (get card-positions game) pos2)))
      (is-match (is-eq card1 card2))
      (new-matches (if is-match (+ (get matches game) u1) (get matches game)))
      (game-complete (is-eq new-matches u8))
      (new-status (if game-complete "completed" "active"))
    )
    (asserts! (is-eq tx-sender (get player game)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status game) "active") ERR_GAME_FINISHED)
    
    (map-set games
      { game-id: game-id }
      (merge game {
        matches: new-matches,
        status: new-status,
        end-time: (if game-complete (some block-height) none)
      })
    )
    
    (if game-complete
      (let
        (
          (efficiency-bonus (/ u10000000 (get moves game)))
          (base-reward u1000000)
          (total-reward (+ base-reward efficiency-bonus))
        )
        (map-set game-rewards
          { game-id: game-id }
          { reward-amount: total-reward, claimed: false }
        )
        (update-player-game-complete tx-sender (get moves game) total-reward)
        (ok { match: is-match, completed: true, reward: total-reward })
      )
      (ok { match: is-match, completed: false, reward: u0 })
    )
  )
)

;; Forfeit a game and get partial refund
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

;; Claim reward for a completed game
(define-public (claim-reward (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR_GAME_NOT_FOUND))
      (reward-info (unwrap! (map-get? game-rewards { game-id: game-id }) ERR_GAME_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get player game)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status game) "completed") ERR_GAME_FINISHED)
    (asserts! (not (get claimed reward-info)) ERR_UNAUTHORIZED)
    
    (map-set game-rewards
      { game-id: game-id }
      (merge reward-info { claimed: true })
    )
    
    (var-set total-rewards-paid (+ (var-get total-rewards-paid) (get reward-amount reward-info)))
    (try! (as-contract (stx-transfer? (get reward-amount reward-info) tx-sender (get player game))))
    (ok (get reward-amount reward-info))
  )
)

;; Helper function to replace element at index
(define-private (replace-at-index (lst (list 16 uint)) (index uint) (new-val uint))
  (map update-list-element 
    lst 
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15)
    (list index index index index index index index index index index index index index index index index)
    (list new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val new-val)
  )
)

;; Helper for replace-at-index
(define-private (update-list-element (current-val uint) (current-index uint) (target-index uint) (new-val uint))
  (if (is-eq current-index target-index) new-val current-val)
)

;; Update player stats when starting a game
(define-private (update-player-game-start (player principal))
  (let
    (
      (current-stats (get-player-stats player))
    )
    (map-set player-stats
      { player: player }
      (merge current-stats {
        total-games: (+ (get total-games current-stats) u1)
      })
    )
  )
)

;; Update player stats when completing a game
(define-private (update-player-game-complete (player principal) (moves uint) (reward uint))
  (let
    (
      (current-stats (get-player-stats player))
      (current-best (get best-moves current-stats))
      (new-best (if (or (is-eq current-best u0) (< moves current-best)) moves current-best))
    )
    (map-set player-stats
      { player: player }
      (merge current-stats {
        completed-games: (+ (get completed-games current-stats) u1),
        total-moves: (+ (get total-moves current-stats) moves),
        best-moves: new-best,
        total-rewards: (+ (get total-rewards current-stats) reward)
      })
    )
  )
)

;; Read-only functions

;; Get game information
(define-read-only (get-game (game-id uint))
  (map-get? games { game-id: game-id })
)

;; Get current game counter
(define-read-only (get-current-game-id)
  (var-get game-counter)
)

;; Get player statistics
(define-read-only (get-player-stats (player principal))
  (default-to 
    { total-games: u0, completed-games: u0, total-moves: u0, best-moves: u0, total-rewards: u0 }
    (map-get? player-stats { player: player })
  )
)

;; Get reward information for a game
(define-read-only (get-game-reward (game-id uint))
  (map-get? game-rewards { game-id: game-id })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-games: (var-get game-counter),
    total-rewards-paid: (var-get total-rewards-paid),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)

;; Get specific player games (simplified approach)
(define-read-only (get-player-games (player principal) (start-id uint) (end-id uint))
  (let
    (
      (game-1 (if (and (>= start-id u1) (<= u1 end-id)) (check-player-game player u1) none))
      (game-2 (if (and (>= start-id u2) (<= u2 end-id)) (check-player-game player u2) none))
      (game-3 (if (and (>= start-id u3) (<= u3 end-id)) (check-player-game player u3) none))
      (game-4 (if (and (>= start-id u4) (<= u4 end-id)) (check-player-game player u4) none))
      (game-5 (if (and (>= start-id u5) (<= u5 end-id)) (check-player-game player u5) none))
    )
    {
      game-1: game-1,
      game-2: game-2,
      game-3: game-3,
      game-4: game-4,
      game-5: game-5
    }
  )
)

;; Helper to check if a game belongs to a player
(define-private (check-player-game (player principal) (game-id uint))
  (match (map-get? games { game-id: game-id })
    game-data (if (is-eq (get player game-data) player) (some game-id) none)
    none
  )
)

;; Get leaderboard (simplified)
(define-read-only (get-leaderboard)
  {
    position-1: (map-get? leaderboard { position: u1 }),
    position-2: (map-get? leaderboard { position: u2 }),
    position-3: (map-get? leaderboard { position: u3 }),
    position-4: (map-get? leaderboard { position: u4 }),
    position-5: (map-get? leaderboard { position: u5 })
  }
)

;; Update leaderboard position
(define-public (update-leaderboard (position uint) (player principal) (score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set leaderboard
      { position: position }
      { player: player, best-score: score }
    )
    (ok true)
  )
)