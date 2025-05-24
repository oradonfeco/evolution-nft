;; Rewards & Social Features Contract

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INVALID-AMOUNT (err u400))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))

;; Achievement IDs
(define-constant ACHIEVEMENT-FIRST-EVOLUTION u1)
(define-constant ACHIEVEMENT-WEEK-STREAK u2)
(define-constant ACHIEVEMENT-MONTH-STREAK u3)
(define-constant ACHIEVEMENT-SOCIAL-BUTTERFLY u4)
(define-constant ACHIEVEMENT-COLLECTOR u5)

;; Reward amounts (in micro-STX)
(define-constant REWARD-FIRST-EVOLUTION u1000000) ;; 1 STX
(define-constant REWARD-WEEK-STREAK u2000000)     ;; 2 STX
(define-constant REWARD-MONTH-STREAK u5000000)    ;; 5 STX
(define-constant REWARD-SOCIAL-BUTTERFLY u3000000) ;; 3 STX
(define-constant REWARD-COLLECTOR u10000000)      ;; 10 STX

;; Data Variables
(define-data-var total-rewards-distributed uint u0)
(define-data-var contract-balance uint u0)

;; Data Maps
(define-map user-stats principal {
    total-evolutions: uint,
    current-streak: uint,
    last-evolution-block: uint,
    social-interactions: uint,
    nfts-owned: uint,
    total-rewards-earned: uint
})

(define-map user-achievements { user: principal, achievement-id: uint } {
    unlocked-at: uint,
    reward-claimed: bool
})

(define-map daily-streaks principal {
    current-streak: uint,
    last-activity-day: uint,
    longest-streak: uint
})

(define-map social-interactions { from: principal, to: principal } {
    interaction-type: (string-ascii 20),
    timestamp: uint,
    nft-id: uint
})

(define-map user-followers principal (list 100 principal))
(define-map user-following principal (list 100 principal))

;; Read-only functions
(define-read-only (get-user-stats (user principal))
    (default-to 
        { total-evolutions: u0, current-streak: u0, last-evolution-block: u0, 
          social-interactions: u0, nfts-owned: u0, total-rewards-earned: u0 }
        (map-get? user-stats user)
    )
)

(define-read-only (get-user-achievement (user principal) (achievement-id uint))
    (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

(define-read-only (get-daily-streak (user principal))
    (default-to 
        { current-streak: u0, last-activity-day: u0, longest-streak: u0 }
        (map-get? daily-streaks user)
    )
)

(define-read-only (get-total-rewards-distributed)
    (var-get total-rewards-distributed)
)

(define-read-only (get-contract-balance)
    (var-get contract-balance)
)

(define-read-only (get-followers (user principal))
    (default-to (list) (map-get? user-followers user))
)

(define-read-only (get-following (user principal))
    (default-to (list) (map-get? user-following user))
)

;; Private functions
(define-private (get-current-day)
    (/ stacks-block-height u144) ;; Assuming ~144 blocks per day
)

(define-private (unlock-achievement (achievement-id uint) (reward-amount uint))
    (let (
        (current-stats (get-user-stats tx-sender))
        (new-total-rewards (+ (get total-rewards-earned current-stats) reward-amount))
    )
        ;; Record achievement
        (map-set user-achievements 
            { user: tx-sender, achievement-id: achievement-id }
            { unlocked-at: stacks-block-height, reward-claimed: false }
        )
        
        ;; Update user stats
        (map-set user-stats tx-sender
            (merge current-stats { total-rewards-earned: new-total-rewards })
        )
        
        ;; Update contract totals
        (var-set total-rewards-distributed 
            (+ (var-get total-rewards-distributed) reward-amount)
        )
        
        (ok achievement-id)
    )
)

;; Fixed streak achievement checking function
(define-private (check-streak-achievements (streak uint))
    (let (
        (week-achievement-exists (is-some (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-WEEK-STREAK })))
        (month-achievement-exists (is-some (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-MONTH-STREAK })))
    )
        (begin
            ;; Check week streak achievement
            (if (and (>= streak u7) (not week-achievement-exists))
                (unwrap-panic (unlock-achievement ACHIEVEMENT-WEEK-STREAK REWARD-WEEK-STREAK))
                u0
            )
            
            ;; Check month streak achievement
            (if (and (>= streak u30) (not month-achievement-exists))
                (unwrap-panic (unlock-achievement ACHIEVEMENT-MONTH-STREAK REWARD-MONTH-STREAK))
                u0
            )
            
            (ok streak)
        )
    )
)

(define-private (update-daily-streak (user principal))
    (let (
        (current-day (get-current-day))
        (streak-data (get-daily-streak user))
        (last-day (get last-activity-day streak-data))
        (current-streak (get current-streak streak-data))
        (longest-streak (get longest-streak streak-data))
    )
        (if (is-eq current-day (+ last-day u1))
            ;; Consecutive day - increment streak
            (let (
                (new-streak (+ current-streak u1))
                (new-longest (if (> new-streak longest-streak) new-streak longest-streak))
            )
                (map-set daily-streaks user {
                    current-streak: new-streak,
                    last-activity-day: current-day,
                    longest-streak: new-longest
                })
                (unwrap-panic (check-streak-achievements new-streak))
                (ok new-streak)
            )
            ;; Not consecutive - reset or start streak
            (if (is-eq current-day last-day)
                ;; Same day - no change
                (ok current-streak)
                ;; New streak starts
                (begin
                    (map-set daily-streaks user {
                        current-streak: u1,
                        last-activity-day: current-day,
                        longest-streak: longest-streak
                    })
                    (ok u1)
                )
            )
        )
    )
)

;; Public functions
(define-public (record-evolution (nft-id uint))
    (let (
        (current-stats (get-user-stats tx-sender))
        (new-evolutions (+ (get total-evolutions current-stats) u1))
        (new-nfts-owned (+ (get nfts-owned current-stats) u1))
    )
        ;; Update user stats
        (map-set user-stats tx-sender
            (merge current-stats {
                total-evolutions: new-evolutions,
                last-evolution-block: stacks-block-height,
                nfts-owned: new-nfts-owned
            })
        )
        
        ;; Update daily streak
        (unwrap-panic (update-daily-streak tx-sender))
        
        ;; Check for first evolution achievement
        (if (and (is-eq new-evolutions u1) 
                 (is-none (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-FIRST-EVOLUTION })))
            (unwrap-panic (unlock-achievement ACHIEVEMENT-FIRST-EVOLUTION REWARD-FIRST-EVOLUTION))
            u0
        )
        
        ;; Check for collector achievement (10+ NFTs)
        (if (and (>= new-nfts-owned u10)
                 (is-none (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-COLLECTOR })))
            (unwrap-panic (unlock-achievement ACHIEVEMENT-COLLECTOR REWARD-COLLECTOR))
            u0
        )
        
        (ok new-evolutions)
    )
)

(define-public (follow-user (user-to-follow principal))
    (let (
        (current-following (get-following tx-sender))
        (current-followers (get-followers user-to-follow))
        (current-stats (get-user-stats tx-sender))
    )
        (asserts! (not (is-eq tx-sender user-to-follow)) ERR-INVALID-AMOUNT)
        
        ;; Add to following list
        (map-set user-following tx-sender 
            (unwrap! (as-max-len? (append current-following user-to-follow) u100) ERR-INVALID-AMOUNT)
        )
        
        ;; Add to followers list
        (map-set user-followers user-to-follow
            (unwrap! (as-max-len? (append current-followers tx-sender) u100) ERR-INVALID-AMOUNT)
        )
        
        ;; Update social interactions count
        (map-set user-stats tx-sender
            (merge current-stats {
                social-interactions: (+ (get social-interactions current-stats) u1)
            })
        )
        
        ;; Record social interaction
        (map-set social-interactions { from: tx-sender, to: user-to-follow } {
            interaction-type: "follow",
            timestamp: stacks-block-height,
            nft-id: u0
        })
        
        ;; Check for social butterfly achievement (10+ interactions)
        (if (and (>= (+ (get social-interactions current-stats) u1) u10)
                 (is-none (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-SOCIAL-BUTTERFLY })))
            (unwrap-panic (unlock-achievement ACHIEVEMENT-SOCIAL-BUTTERFLY REWARD-SOCIAL-BUTTERFLY))
            u0
        )
        
        (ok true)
    )
)

(define-public (like-nft (owner principal) (nft-id uint))
    (let (
        (current-stats (get-user-stats tx-sender))
    )
        ;; Record social interaction
        (map-set social-interactions { from: tx-sender, to: owner } {
            interaction-type: "like",
            timestamp: stacks-block-height,
            nft-id: nft-id
        })
        
        ;; Update social interactions count
        (map-set user-stats tx-sender
            (merge current-stats {
                social-interactions: (+ (get social-interactions current-stats) u1)
            })
        )
        
        ;; Check for social butterfly achievement
        (if (and (>= (+ (get social-interactions current-stats) u1) u10)
                 (is-none (map-get? user-achievements { user: tx-sender, achievement-id: ACHIEVEMENT-SOCIAL-BUTTERFLY })))
            (unwrap-panic (unlock-achievement ACHIEVEMENT-SOCIAL-BUTTERFLY REWARD-SOCIAL-BUTTERFLY))
            u0
        )
        
        (ok true)
    )
)

(define-public (claim-achievement-reward (achievement-id uint))
    (let (
        (achievement-data (unwrap! (map-get? user-achievements { user: tx-sender, achievement-id: achievement-id }) ERR-NOT-FOUND))
        (reward-amount (if (is-eq achievement-id ACHIEVEMENT-FIRST-EVOLUTION) REWARD-FIRST-EVOLUTION
                      (if (is-eq achievement-id ACHIEVEMENT-WEEK-STREAK) REWARD-WEEK-STREAK
                      (if (is-eq achievement-id ACHIEVEMENT-MONTH-STREAK) REWARD-MONTH-STREAK
                      (if (is-eq achievement-id ACHIEVEMENT-SOCIAL-BUTTERFLY) REWARD-SOCIAL-BUTTERFLY
                      (if (is-eq achievement-id ACHIEVEMENT-COLLECTOR) REWARD-COLLECTOR
                      u0))))))
    )
        (asserts! (not (get reward-claimed achievement-data)) ERR-ALREADY-EXISTS)
        (asserts! (> reward-amount u0) ERR-INVALID-AMOUNT)
        
        ;; Mark reward as claimed
        (map-set user-achievements { user: tx-sender, achievement-id: achievement-id }
            (merge achievement-data { reward-claimed: true })
        )
        
        ;; Transfer reward (in a real implementation, you'd transfer STX here)
        ;; For now, we'll just update the contract balance
        (var-set contract-balance (- (var-get contract-balance) reward-amount))
        
        (ok reward-amount)
    )
)

;; Admin functions
(define-public (fund-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set contract-balance (+ (var-get contract-balance) u100000000)) ;; Add 100 STX
        (ok true)
    )
)

(define-public (withdraw-funds (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get contract-balance)) ERR-INSUFFICIENT-BALANCE)
        (var-set contract-balance (- (var-get contract-balance) amount))
        (ok amount)
    )
)