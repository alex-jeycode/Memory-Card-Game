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


;; NEW DATA MAP: Track player statistics
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

;; MODIFIED: Update start-game to track player stats
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
    ;; NEW: Update player stats
    (update-player-game-start tx-sender)
    (ok game-id)
  )
)

;; MODIFIED: Update check-match to track completion stats
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
        ;; NEW: Update player stats for completed game
        (update-player-game-complete tx-sender (get moves game) total-reward)
        (ok { match: is-match, completed: true, reward: total-reward })
      )
      (ok { match: is-match, completed: false, reward: u0 })
    )
  )
)

;; NEW FUNCTION: Get player statistics
(define-read-only (get-player-stats (player principal))
  (default-to 
    { total-games: u0, completed-games: u0, total-moves: u0, best-moves: u0, total-rewards: u0 }
    (map-get? player-stats { player: player })
  )
)

;; NEW HELPER: Update player stats when starting a game
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

;; NEW HELPER: Update player stats when completing a game
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

;; NEW FUNCTION: Get games by player (returns last 10 games)
(define-read-only (get-player-games (player principal))
  (let
    (
      (total-games (var-get game-counter))
      (start-search (if (> total-games u10) (- total-games u10) u1))
    )
    (filter-player-games player start-search total-games (list))
  )
)

;; NEW HELPER: Filter games by player
(define-private (filter-player-games (player principal) (current-id uint) (end-id uint) (acc (list 10 uint)))
  (if (<= current-id end-id)
    (let
      (
        (game-opt (map-get? games { game-id: current-id }))
      )
      (match game-opt
        game-data 
        (if (is-eq (get player game-data) player)
          (filter-player-games player (+ current-id u1) end-id (unwrap-panic (as-max-len? (append acc current-id) u10)))
          (filter-player-games player (+ current-id u1) end-id acc)
        )
        (filter-player-games player (+ current-id u1) end-id acc)
      )
    )
    acc
  )
)

;; NEW DATA VARIABLE: Track total rewards paid out
(define-data-var total-rewards-paid uint u0)

;; MODIFIED: Update claim-reward to track total payouts
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
    
    ;; NEW: Track total rewards paid
    (var-set total-rewards-paid (+ (var-get total-rewards-paid) (get reward-amount reward-info)))
    (try! (as-contract (stx-transfer? (get reward-amount reward-info) tx-sender (get player game))))
    (ok (get reward-amount reward-info))
  )
)

;; NEW FUNCTION: Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-games: (var-get game-counter),
    total-rewards-paid: (var-get total-rewards-paid),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)

;; NEW DATA MAP: Leaderboard tracking
(define-map leaderboard
  { position: uint }
  { player: principal, best-score: uint }
)

;; NEW FUNCTION: Get top players leaderboard
(define-read-only (get-leaderboard (count uint))
  (let
    (
      (max-count (if (> count u10) u10 count)) ;; Limit to 10 entries
    )
    (get-leaderboard-entries u1 max-count (list))
  )
)

;; NEW HELPER: Get leaderboard entries
(define-private (get-leaderboard-entries (position uint) (max-position uint) (acc (list 10 { player: principal, score: uint })))
  (if (<= position max-position)
    (match (map-get? leaderboard { position: position })
      entry (get-leaderboard-entries 
        (+ position u1) 
        max-position 
        (unwrap-panic (as-max-len? (append acc { player: (get player entry), score: (get best-score entry) }) u10))
      )
      (get-leaderboard-entries (+ position u1) max-position acc)
    )
    acc
  )
)
