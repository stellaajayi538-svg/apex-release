;; Apex Release - Progressive Impact Release Platform

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-PROJECT-NOT-FOUND     (err u101))
(define-constant ERR-MILESTONE-NOT-FOUND   (err u102))
(define-constant ERR-INVALID-STATE         (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS    (err u104))
(define-constant ERR-ALREADY-VERIFIED      (err u105))
(define-constant ERR-INVALID-SCORE         (err u106))
(define-constant ERR-VALIDATOR-EXISTS      (err u107))
(define-constant ERR-NOT-VALIDATOR         (err u108))
(define-constant ERR-PROPOSAL-NOT-FOUND    (err u109))
(define-constant ERR-ALREADY-VOTED         (err u110))
(define-constant ERR-VOTING-CLOSED         (err u111))
(define-constant ERR-CHALLENGE-NOT-FOUND   (err u112))
(define-constant ERR-TRANSFER-FAILED       (err u113))
(define-constant ERR-INVALID-PARAM         (err u114))

;; Project status codes
(define-constant STATUS-ACTIVE     u1)
(define-constant STATUS-COMPLETED  u2)
(define-constant STATUS-CANCELLED  u3)
(define-constant STATUS-DISPUTED   u4)

;; Milestone verification status
(define-constant VERIFY-PENDING   u0)
(define-constant VERIFY-APPROVED  u1)
(define-constant VERIFY-REJECTED  u2)

;; Scoring constants (0-100 scale)
(define-constant MAX-SCORE         u100)
(define-constant MIN-PASS-SCORE    u60)

;; Governance voting window in blocks (roughly 7 days at 10 min/block)
(define-constant VOTING-WINDOW     u1008)

;; ============================================================
;; DATA VARIABLES
;; ============================================================

(define-data-var project-nonce    uint u0)
(define-data-var milestone-nonce  uint u0)
(define-data-var proposal-nonce   uint u0)
(define-data-var challenge-nonce  uint u0)
(define-data-var nft-nonce        uint u0)

;; Platform treasury (holds protocol fees)
(define-data-var treasury-balance uint u0)

;; Platform fee in basis points (e.g., 200 = 2%)
(define-data-var platform-fee-bps uint u200)

;; ============================================================
;; FUNGIBLE TOKEN - APEX
;; ============================================================

(define-fungible-token apex-token)

;; ============================================================
;; NON-FUNGIBLE TOKEN - IMPACT NFT
;; ============================================================
;; Each NFT records contributor engagement with a project milestone.

(define-non-fungible-token impact-nft uint)

;; ============================================================
;; MAPS
;; ============================================================

;; --- Projects ---
(define-map projects
    { project-id: uint }
    {
        creator:         principal,
        title:           (string-ascii 128),
        impact-vertical: (string-ascii 64),  ;; e.g. "environmental" | "education" | "healthcare" | "economic"
        total-funding:   uint,               ;; total STX pledged into escrow
        released-amount: uint,               ;; STX released so far
        status:          uint,               ;; STATUS-* constants
        reputation-score: uint,              ;; creator reputation snapshot at creation
        created-at:      uint                ;; block height
    }
)

;; --- Milestones ---
(define-map milestones
    { milestone-id: uint }
    {
        project-id:      uint,
        description:     (string-ascii 256),
        release-amount:  uint,               ;; STX to release on approval
        verify-status:   uint,               ;; VERIFY-* constants
        impact-score:    uint,               ;; Dynamic Impact Score (0-100)
        community-votes: uint,               ;; count of community approvals
        community-rejects: uint,             ;; count of community rejections
        ai-score:        uint,               ;; AI assessment score (0-100)
        ngo-score:       uint,               ;; NGO credibility score (0-100)
        created-at:      uint
    }
)

;; Tracks which validators have voted on a milestone
(define-map milestone-validator-votes
    { milestone-id: uint, validator: principal }
    { approved: bool }
)

;; --- Validators ---
;; Validators participate in community verification
(define-map validators
    { validator: principal }
    {
        active:          bool,
        total-validations: uint,
        correct-validations: uint  ;; used for validator reputation
    }
)

;; --- Creator Reputation ---
(define-map creator-reputation
    { creator: principal }
    {
        score:              uint,   ;; 0-100
        projects-completed: uint,
        projects-created:   uint,
        total-impact-score: uint
    }
)

;; --- Funders ---
;; Records how much each address funded into a project
(define-map project-funders
    { project-id: uint, funder: principal }
    { amount: uint }
)

;; --- APEX Token Balances ---
;; Tracks APEX governance token balances (mirrors FT but allows on-chain queries)
(define-map apex-balances
    { holder: principal }
    { balance: uint }
)

;; --- Governance Proposals ---
(define-map governance-proposals
    { proposal-id: uint }
    {
        proposer:       principal,
        description:    (string-ascii 256),
        param-key:      (string-ascii 64),   ;; which parameter to change
        new-value:      uint,
        votes-for:      uint,
        votes-against:  uint,
        open-until:     uint,                ;; block height deadline
        executed:       bool
    }
)

;; Tracks which addresses have voted on a proposal
(define-map proposal-votes
    { proposal-id: uint, voter: principal }
    { voted: bool }
)

;; --- Community Challenges ---
(define-map challenges
    { challenge-id: uint }
    {
        creator:     principal,
        description: (string-ascii 256),
        reward:      uint,        ;; STX reward pool
        status:      uint,        ;; 1=open 2=closed
        winner:      (optional principal),
        created-at:  uint
    }
)

;; --- Impact NFT Metadata ---
(define-map nft-metadata
    { token-id: uint }
    {
        project-id:   uint,
        milestone-id: uint,
        contributor:  principal,
        impact-score: uint,
        minted-at:    uint
    }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Compute the Dynamic Impact Score as a weighted average:
;;   40% community vote ratio, 35% AI score, 25% NGO score
(define-private (compute-impact-score
    (community-votes uint)
    (community-rejects uint)
    (ai-score uint)
    (ngo-score uint))
    (let (
        (total-votes (+ community-votes community-rejects))
        (community-ratio (if (> total-votes u0)
            (/ (* community-votes u100) total-votes)
            u0))
        (weighted-community (/ (* community-ratio u40) u100))
        (weighted-ai        (/ (* ai-score u35) u100))
        (weighted-ngo       (/ (* ngo-score u25) u100))
    )
    (+ weighted-community (+ weighted-ai weighted-ngo)))
)

;; Calculate platform fee in STX
(define-private (calc-fee (amount uint))
    (/ (* amount (var-get platform-fee-bps)) u10000)
)

;; ============================================================
;; PROJECT MANAGEMENT
;; ============================================================

;; Create a new project and deposit initial escrow funding
(define-public (create-project
    (title           (string-ascii 128))
    (impact-vertical (string-ascii 64))
    (funding-amount  uint))
    (let (
        (project-id (+ (var-get project-nonce) u1))
        (creator    tx-sender)
        (rep        (default-to { score: u50, projects-completed: u0, projects-created: u0, total-impact-score: u0 }
                        (map-get? creator-reputation { creator: creator })))
    )
    (asserts! (> funding-amount u0) ERR-INVALID-PARAM)
    ;; Transfer STX from creator to contract escrow
    (try! (stx-transfer? funding-amount creator (as-contract tx-sender)))
    ;; Store project
    (map-set projects
        { project-id: project-id }
        {
            creator:          creator,
            title:            title,
            impact-vertical:  impact-vertical,
            total-funding:    funding-amount,
            released-amount:  u0,
            status:           STATUS-ACTIVE,
            reputation-score: (get score rep),
            created-at:       block-height
        }
    )
    ;; Update creator stats
    (map-set creator-reputation
        { creator: creator }
        (merge rep { projects-created: (+ (get projects-created rep) u1) })
    )
    (var-set project-nonce project-id)
    (ok project-id))
)

;; Add more funding to an existing active project
(define-public (fund-project (project-id uint) (amount uint))
    (let (
        (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (funder  tx-sender)
        (existing (default-to { amount: u0 }
                    (map-get? project-funders { project-id: project-id, funder: funder })))
    )
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATE)
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    (try! (stx-transfer? amount funder (as-contract tx-sender)))
    (map-set projects
        { project-id: project-id }
        (merge project { total-funding: (+ (get total-funding project) amount) })
    )
    (map-set project-funders
        { project-id: project-id, funder: funder }
        { amount: (+ (get amount existing) amount) }
    )
    (ok true))
)

;; ============================================================
;; MILESTONE MANAGEMENT
;; ============================================================

;; Register a new milestone for a project (creator only)
(define-public (create-milestone
    (project-id  uint)
    (description (string-ascii 256))
    (release-amount uint))
    (let (
        (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (milestone-id (+ (var-get milestone-nonce) u1))
    )
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATE)
    (asserts! (<= release-amount
        (- (get total-funding project) (get released-amount project))) ERR-INSUFFICIENT-FUNDS)
    (map-set milestones
        { milestone-id: milestone-id }
        {
            project-id:        project-id,
            description:       description,
            release-amount:    release-amount,
            verify-status:     VERIFY-PENDING,
            impact-score:      u0,
            community-votes:   u0,
            community-rejects: u0,
            ai-score:          u0,
            ngo-score:         u0,
            created-at:        block-height
        }
    )
    (var-set milestone-nonce milestone-id)
    (ok milestone-id))
)

;; ============================================================
;; THREE-TIER VERIFICATION
;; ============================================================

;; Tier 1: Community validator vote on a milestone
(define-public (validator-vote (milestone-id uint) (approve bool))
    (let (
        (milestone  (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
        (val-info   (unwrap! (map-get? validators { validator: tx-sender }) ERR-NOT-VALIDATOR))
        (vote-key   { milestone-id: milestone-id, validator: tx-sender })
    )
    (asserts! (get active val-info) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get verify-status milestone) VERIFY-PENDING) ERR-INVALID-STATE)
    (asserts! (is-none (map-get? milestone-validator-votes vote-key)) ERR-ALREADY-VERIFIED)
    ;; Record vote
    (map-set milestone-validator-votes vote-key { approved: approve })
    (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone {
            community-votes:   (if approve
                (+ (get community-votes milestone) u1)
                (get community-votes milestone)),
            community-rejects: (if approve
                (get community-rejects milestone)
                (+ (get community-rejects milestone) u1))
        })
    )
    (ok true))
)

;; Tier 2: AI oracle submits impact score (contract owner acts as oracle relay)
(define-public (submit-ai-score (milestone-id uint) (score uint))
    (let (
        (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= score MAX-SCORE) ERR-INVALID-SCORE)
    (asserts! (is-eq (get verify-status milestone) VERIFY-PENDING) ERR-INVALID-STATE)
    (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { ai-score: score })
    )
    (ok true))
)

;; Tier 3: NGO partner submits credibility score (contract owner as relay)
(define-public (submit-ngo-score (milestone-id uint) (score uint))
    (let (
        (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= score MAX-SCORE) ERR-INVALID-SCORE)
    (asserts! (is-eq (get verify-status milestone) VERIFY-PENDING) ERR-INVALID-STATE)
    (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { ngo-score: score })
    )
    (ok true))
)

;; Finalize verification: compute Dynamic Impact Score and release funds if passing
(define-public (finalize-milestone (milestone-id uint))
    (let (
        (milestone  (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
        (project    (unwrap! (map-get? projects { project-id: (get project-id milestone) }) ERR-PROJECT-NOT-FOUND))
        (dis        (compute-impact-score
                        (get community-votes milestone)
                        (get community-rejects milestone)
                        (get ai-score milestone)
                        (get ngo-score milestone)))
        (project-id (get project-id milestone))
    )
    ;; Only contract owner can finalize (acts as orchestrator)
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get verify-status milestone) VERIFY-PENDING) ERR-INVALID-STATE)
    (if (>= dis MIN-PASS-SCORE)
        ;; Approved: release funds to creator
        (let (
            (release      (get release-amount milestone))
            (fee          (calc-fee release))
            (net-release  (- release fee))
            (creator      (get creator project))
        )
        ;; Transfer net amount to creator
        (try! (as-contract (stx-transfer? net-release tx-sender creator)))
        ;; Accumulate fee into treasury
        (var-set treasury-balance (+ (var-get treasury-balance) fee))
        ;; Update milestone status and score
        (map-set milestones
            { milestone-id: milestone-id }
            (merge milestone { verify-status: VERIFY-APPROVED, impact-score: dis })
        )
        ;; Update project released amount
        (map-set projects
            { project-id: project-id }
            (merge project { released-amount: (+ (get released-amount project) release) })
        )
        ;; Mint an Impact NFT for the creator as proof of achievement
        (try! (mint-impact-nft project-id milestone-id creator dis))
        ;; Update creator reputation
        (update-creator-reputation creator dis)
        (ok dis))
        ;; Rejected: mark milestone as rejected
        (begin
            (map-set milestones
                { milestone-id: milestone-id }
                (merge milestone { verify-status: VERIFY-REJECTED, impact-score: dis })
            )
            (ok dis))
    ))
)

;; ============================================================
;; IMPACT NFT
;; ============================================================

;; Mint an Impact NFT to a contributor (internal)
(define-private (mint-impact-nft
    (project-id   uint)
    (milestone-id uint)
    (recipient    principal)
    (score        uint))
    (let (
        (token-id (+ (var-get nft-nonce) u1))
    )
    (try! (nft-mint? impact-nft token-id recipient))
    (map-set nft-metadata
        { token-id: token-id }
        {
            project-id:   project-id,
            milestone-id: milestone-id,
            contributor:  recipient,
            impact-score: score,
            minted-at:    block-height
        }
    )
    (var-set nft-nonce token-id)
    (ok token-id))
)

;; Public wrapper: funders can claim an Impact NFT upon a project milestone approval
(define-public (claim-impact-nft (project-id uint) (milestone-id uint))
    (let (
        (funder-entry (unwrap! (map-get? project-funders { project-id: project-id, funder: tx-sender })
                        ERR-NOT-AUTHORIZED))
        (milestone    (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
    )
    (asserts! (is-eq (get project-id milestone) project-id) ERR-INVALID-PARAM)
    (asserts! (is-eq (get verify-status milestone) VERIFY-APPROVED) ERR-INVALID-STATE)
    (mint-impact-nft project-id milestone-id tx-sender (get impact-score milestone))
    )
)

;; ============================================================
;; VALIDATOR REGISTRY
;; ============================================================

;; Register as a community validator (contract owner approves)
(define-public (register-validator (candidate principal))
    (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? validators { validator: candidate })) ERR-VALIDATOR-EXISTS)
    (map-set validators
        { validator: candidate }
        { active: true, total-validations: u0, correct-validations: u0 }
    )
    (ok true))
)

;; Deactivate a validator
(define-public (deactivate-validator (validator principal))
    (let (
        (val-info (unwrap! (map-get? validators { validator: validator }) ERR-NOT-VALIDATOR))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set validators
        { validator: validator }
        (merge val-info { active: false })
    )
    (ok true))
)

;; ============================================================
;; REPUTATION SYSTEM
;; ============================================================

;; Update creator reputation after a milestone finalization (internal)
(define-private (update-creator-reputation (creator principal) (impact-score uint))
    (let (
        (rep (default-to { score: u50, projects-completed: u0, projects-created: u0, total-impact-score: u0 }
                (map-get? creator-reputation { creator: creator })))
        (new-total    (+ (get total-impact-score rep) impact-score))
        (completions  (+ (get projects-completed rep) u1))
        ;; Rolling average reputation score
        (new-score    (/ new-total completions))
        (capped-score (if (> new-score MAX-SCORE) MAX-SCORE new-score))
    )
    (map-set creator-reputation
        { creator: creator }
        (merge rep {
            score:              capped-score,
            projects-completed: completions,
            total-impact-score: new-total
        })
    )
    true)
)

;; ============================================================
;; APEX GOVERNANCE TOKEN
;; ============================================================

;; Mint APEX tokens (contract owner only - initial distribution)
(define-public (mint-apex (recipient principal) (amount uint))
    (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (try! (ft-mint? apex-token amount recipient))
    (let (
        (existing (default-to { balance: u0 } (map-get? apex-balances { holder: recipient })))
    )
    (map-set apex-balances { holder: recipient } { balance: (+ (get balance existing) amount) }))
    (ok true))
)

;; Transfer APEX tokens
(define-public (transfer-apex (amount uint) (sender principal) (recipient principal))
    (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (try! (ft-transfer? apex-token amount sender recipient))
    (ok true))
)

;; ============================================================
;; GOVERNANCE PROPOSALS
;; ============================================================

;; Submit a governance proposal to change a platform parameter
(define-public (create-proposal
    (description (string-ascii 256))
    (param-key   (string-ascii 64))
    (new-value   uint))
    (let (
        (proposal-id (+ (var-get proposal-nonce) u1))
        (bal         (ft-get-balance apex-token tx-sender))
    )
    ;; Require minimum 100 APEX to propose
    (asserts! (>= bal u100) ERR-NOT-AUTHORIZED)
    (map-set governance-proposals
        { proposal-id: proposal-id }
        {
            proposer:     tx-sender,
            description:  description,
            param-key:    param-key,
            new-value:    new-value,
            votes-for:    u0,
            votes-against: u0,
            open-until:   (+ block-height VOTING-WINDOW),
            executed:     false
        }
    )
    (var-set proposal-nonce proposal-id)
    (ok proposal-id))
)

;; Cast a vote on a governance proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
    (let (
        (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (voter    tx-sender)
        (bal      (ft-get-balance apex-token voter))
        (vote-key { proposal-id: proposal-id, voter: voter })
    )
    (asserts! (<= block-height (get open-until proposal)) ERR-VOTING-CLOSED)
    (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-VOTED)
    (asserts! (> bal u0) ERR-NOT-AUTHORIZED)
    (map-set proposal-votes vote-key { voted: true })
    (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal {
            votes-for:     (if support (+ (get votes-for proposal) bal) (get votes-for proposal)),
            votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) bal))
        })
    )
    (ok true))
)

;; Execute a passed proposal (contract owner acts as executor after voting period)
(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> block-height (get open-until proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-INVALID-STATE)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-STATE)
    ;; Apply the parameter change
    (if (is-eq (get param-key proposal) "platform-fee-bps")
        (var-set platform-fee-bps (get new-value proposal))
        true  ;; unknown param-key: no-op (extend as needed)
    )
    (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal { executed: true })
    )
    (ok true))
)

;; ============================================================
;; COMMUNITY CHALLENGE MECHANISM
;; ============================================================

;; Create a community challenge with a STX reward pool
(define-public (create-challenge (description (string-ascii 256)) (reward uint))
    (let (
        (challenge-id (+ (var-get challenge-nonce) u1))
    )
    (asserts! (> reward u0) ERR-INVALID-PARAM)
    (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
    (map-set challenges
        { challenge-id: challenge-id }
        {
            creator:    tx-sender,
            description: description,
            reward:     reward,
            status:     u1,
            winner:     none,
            created-at: block-height
        }
    )
    (var-set challenge-nonce challenge-id)
    (ok challenge-id))
)

;; Award a challenge reward to a winner (creator only)
(define-public (award-challenge (challenge-id uint) (winner principal))
    (let (
        (challenge (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get creator challenge)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status challenge) u1) ERR-INVALID-STATE)
    (try! (as-contract (stx-transfer? (get reward challenge) tx-sender winner)))
    (map-set challenges
        { challenge-id: challenge-id }
        (merge challenge { status: u2, winner: (some winner) })
    )
    (ok true))
)

;; ============================================================
;; PROJECT CANCELLATION
;; ============================================================

;; Cancel a project and refund remaining escrow to the creator
(define-public (cancel-project (project-id uint))
    (let (
        (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (remaining (- (get total-funding project) (get released-amount project)))
    )
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATE)
    (if (> remaining u0)
        (try! (as-contract (stx-transfer? remaining tx-sender (get creator project))))
        true
    )
    (map-set projects
        { project-id: project-id }
        (merge project { status: STATUS-CANCELLED })
    )
    (ok true))
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
    (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-creator-reputation (creator principal))
    (default-to
        { score: u50, projects-completed: u0, projects-created: u0, total-impact-score: u0 }
        (map-get? creator-reputation { creator: creator }))
)

(define-read-only (get-validator-info (validator principal))
    (map-get? validators { validator: validator })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-nft-metadata (token-id uint))
    (map-get? nft-metadata { token-id: token-id })
)

(define-read-only (get-challenge (challenge-id uint))
    (map-get? challenges { challenge-id: challenge-id })
)

(define-read-only (get-apex-balance (holder principal))
    (ft-get-balance apex-token holder)
)

(define-read-only (get-platform-fee-bps)
    (var-get platform-fee-bps)
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (get-funder-amount (project-id uint) (funder principal))
    (default-to { amount: u0 }
        (map-get? project-funders { project-id: project-id, funder: funder }))
)
