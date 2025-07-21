;; ===============================================
;; DECENTRALIZED COMMUNITY FIBER ARTS COLLECTIVE
;; ===============================================
;; A platform for preserving textile traditions with skill sharing,
;; material resource coordination, and cultural knowledge documentation

;; ===================
;; CONTRACT 1: HERITAGE REGISTRY
;; ===================
;; Manages cultural knowledge, pattern preservation, and technique documentation

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PATTERN_NOT_FOUND (err u101))
(define-constant ERR_TECHNIQUE_NOT_FOUND (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))

;; Data Variables
(define-data-var next-pattern-id uint u1)
(define-data-var next-technique-id uint u1)
(define-data-var contract-uri (string-utf8 256) u"")

;; Cultural Pattern Structure
(define-map cultural-patterns
    uint
    {
        name: (string-utf8 128),
        origin-culture: (string-utf8 64),
        historical-period: (string-utf8 64),
        description: (string-utf8 512),
        difficulty-level: uint,
        materials-needed: (list 10 (string-utf8 64)),
        pattern-data: (string-utf8 2048),
        creator: principal,
        verified: bool,
        preservation-score: uint,
        created-at: uint
    }
)

;; Traditional Techniques Structure
(define-map traditional-techniques
    uint
    {
        name: (string-utf8 128),
        cultural-origin: (string-utf8 64),
        technique-type: (string-utf8 64),
        instructions: (string-utf8 1024),
        tools-required: (list 5 (string-utf8 64)),
        skill-level: uint,
        documentation-uri: (string-utf8 256),
        master-artisan: principal,
        transmission-count: uint,
        created-at: uint
    }
)

;; Pattern Custodians (authorized to verify and maintain patterns)
(define-map pattern-custodians principal bool)

;; Technique Masters (skilled artisans authorized to validate techniques)
(define-map technique-masters principal bool)

;; Community Ratings for Patterns
(define-map pattern-ratings
    { pattern-id: uint, rater: principal }
    { rating: uint, comment: (string-utf8 256) }
)

;; Technique Learning Progress
(define-map learning-progress
    { technique-id: uint, learner: principal }
    {
        progress-level: uint,
        mentor: (optional principal),
        started-at: uint,
        completed-at: (optional uint)
    }
)

;; Heritage Collection Statistics
(define-map heritage-stats
    (string-utf8 64)  ;; culture name
    {
        total-patterns: uint,
        total-techniques: uint,
        active-custodians: uint,
        preservation-score: uint
    }
)

;; ===================
;; PATTERN MANAGEMENT
;; ===================

;; Register a new cultural pattern
(define-public (register-pattern
    (name (string-utf8 128))
    (origin-culture (string-utf8 64))
    (historical-period (string-utf8 64))
    (description (string-utf8 512))
    (difficulty-level uint)
    (materials-needed (list 10 (string-utf8 64)))
    (pattern-data (string-utf8 2048)))
    (let ((pattern-id (var-get next-pattern-id)))
        (asserts! (> (len name) u0) ERR_INVALID_INPUT)
        (asserts! (> (len origin-culture) u0) ERR_INVALID_INPUT)
        (asserts! (<= difficulty-level u10) ERR_INVALID_INPUT)

        (map-set cultural-patterns pattern-id
            {
                name: name,
                origin-culture: origin-culture,
                historical-period: historical-period,
                description: description,
                difficulty-level: difficulty-level,
                materials-needed: materials-needed,
                pattern-data: pattern-data,
                creator: tx-sender,
                verified: false,
                preservation-score: u0,
                created-at: stacks-block-height
            })

        (var-set next-pattern-id (+ pattern-id u1))
        (update-heritage-stats origin-culture true false)
        (ok pattern-id)))

;; Verify pattern by authorized custodian
(define-public (verify-pattern (pattern-id uint))
    (let ((pattern (unwrap! (map-get? cultural-patterns pattern-id) ERR_PATTERN_NOT_FOUND)))
        (asserts! (default-to false (map-get? pattern-custodians tx-sender)) ERR_NOT_AUTHORIZED)

        (map-set cultural-patterns pattern-id
            (merge pattern { verified: true, preservation-score: u100 }))
        (ok true)))

;; Rate a pattern
(define-public (rate-pattern (pattern-id uint) (rating uint) (comment (string-utf8 256)))
    (let ((pattern (unwrap! (map-get? cultural-patterns pattern-id) ERR_PATTERN_NOT_FOUND)))
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_INPUT)

        (map-set pattern-ratings
            { pattern-id: pattern-id, rater: tx-sender }
            { rating: rating, comment: comment })
        (ok true)))

;; ===================
;; TECHNIQUE MANAGEMENT
;; ===================

;; Document a traditional technique
(define-public (document-technique
    (name (string-utf8 128))
    (cultural-origin (string-utf8 64))
    (technique-type (string-utf8 64))
    (instructions (string-utf8 1024))
    (tools-required (list 5 (string-utf8 64)))
    (skill-level uint)
    (documentation-uri (string-utf8 256)))
    (let ((technique-id (var-get next-technique-id)))
        (asserts! (> (len name) u0) ERR_INVALID_INPUT)
        (asserts! (<= skill-level u10) ERR_INVALID_INPUT)

        (map-set traditional-techniques technique-id
            {
                name: name,
                cultural-origin: cultural-origin,
                technique-type: technique-type,
                instructions: instructions,
                tools-required: tools-required,
                skill-level: skill-level,
                documentation-uri: documentation-uri,
                master-artisan: tx-sender,
                transmission-count: u0,
                created-at: stacks-block-height
            })

        (var-set next-technique-id (+ technique-id u1))
        (update-heritage-stats cultural-origin false true)
        (ok technique-id)))

;; Start learning a technique
(define-public (start-learning (technique-id uint) (mentor (optional principal)))
    (let ((technique (unwrap! (map-get? traditional-techniques technique-id) ERR_TECHNIQUE_NOT_FOUND)))
        (map-set learning-progress
            { technique-id: technique-id, learner: tx-sender }
            {
                progress-level: u1,
                mentor: mentor,
                started-at: stacks-block-height,
                completed-at: none
            })

        ;; Increment transmission count
        (map-set traditional-techniques technique-id
            (merge technique { transmission-count: (+ (get transmission-count technique) u1) }))
        (ok true)))

;; Complete technique learning
(define-public (complete-learning (technique-id uint))
    (let ((progress (unwrap! (map-get? learning-progress
            { technique-id: technique-id, learner: tx-sender }) ERR_TECHNIQUE_NOT_FOUND)))
        (map-set learning-progress
            { technique-id: technique-id, learner: tx-sender }
            (merge progress {
                progress-level: u10,
                completed-at: (some stacks-block-height)
            }))
        (ok true)))

;; ===================
;; AUTHORIZATION MANAGEMENT
;; ===================

;; Designate pattern custodian
(define-public (add-pattern-custodian (custodian principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (map-set pattern-custodians custodian true)
        (ok true)))

;; Designate technique master
(define-public (add-technique-master (master principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (map-set technique-masters master true)
        (ok true)))

;; ===================
;; HELPER FUNCTIONS
;; ===================

;; Update heritage statistics
(define-private (update-heritage-stats (culture (string-utf8 64)) (is-pattern bool) (is-technique bool))
    (let ((current-stats (default-to
            { total-patterns: u0, total-techniques: u0, active-custodians: u0, preservation-score: u0 }
            (map-get? heritage-stats culture))))
        (map-set heritage-stats culture
            {
                total-patterns: (if is-pattern (+ (get total-patterns current-stats) u1) (get total-patterns current-stats)),
                total-techniques: (if is-technique (+ (get total-techniques current-stats) u1) (get total-techniques current-stats)),
                active-custodians: (get active-custodians current-stats),
                preservation-score: (+ (get preservation-score current-stats) u10)
            })))

;; ===================
;; READ-ONLY FUNCTIONS
;; ===================

;; Get pattern details
(define-read-only (get-pattern (pattern-id uint))
    (map-get? cultural-patterns pattern-id))

;; Get technique details
(define-read-only (get-technique (technique-id uint))
    (map-get? traditional-techniques technique-id))

;; Get learning progress
(define-read-only (get-learning-progress (technique-id uint) (learner principal))
    (map-get? learning-progress { technique-id: technique-id, learner: learner }))

;; Get heritage statistics for a culture
(define-read-only (get-heritage-stats (culture (string-utf8 64)))
    (map-get? heritage-stats culture))

;; Check if user is pattern custodian
(define-read-only (is-pattern-custodian (user principal))
    (default-to false (map-get? pattern-custodians user)))

;; Check if user is technique master
(define-read-only (is-technique-master (user principal))
    (default-to false (map-get? technique-masters user)))

;; Get pattern rating by user
(define-read-only (get-pattern-rating (pattern-id uint) (rater principal))
    (map-get? pattern-ratings { pattern-id: pattern-id, rater: rater }))

;; Get current pattern and technique counts
(define-read-only (get-collection-size)
    {
        total-patterns: (- (var-get next-pattern-id) u1),
        total-techniques: (- (var-get next-technique-id) u1)
    })

;; ===============================================
;; CONTRACT 2: RESOURCE COORDINATION & SKILL SHARING
;; ===============================================
;; Manages material resources, skill sharing, and community coordination

;; Constants for Resource Contract
(define-constant ERR_RESOURCE_NOT_FOUND (err u200))
(define-constant ERR_INSUFFICIENT_QUANTITY (err u201))
(define-constant ERR_SKILL_SHARE_NOT_FOUND (err u202))
(define-constant ERR_INVALID_TRADE (err u203))

;; Resource Management Data
(define-data-var next-resource-id uint u1)
(define-data-var next-skill-share-id uint u1)

;; Material Resources Structure
(define-map material-resources
    uint
    {
        resource-name: (string-utf8 128),
        resource-type: (string-utf8 64), ;; yarn, fabric, dye, tool, etc.
        quantity-available: uint,
        unit-measure: (string-utf8 32), ;; skeins, yards, pieces, etc.
        quality-grade: uint, ;; 1-10 scale
        cultural-significance: (string-utf8 256),
        origin-location: (string-utf8 128),
        owner: principal,
        shared-publicly: bool,
        trade-preference: (string-utf8 256), ;; what owner wants in exchange
        created-at: uint,
        last-updated: uint
    }
)

;; Skill Sharing Sessions
(define-map skill-sharing-sessions
    uint
    {
        session-title: (string-utf8 128),
        technique-focus: (string-utf8 128),
        instructor: principal,
        max-participants: uint,
        current-participants: uint,
        skill-level-required: uint,
        session-duration: uint, ;; in blocks
        materials-provided: bool,
        location-type: (string-utf8 64), ;; virtual, in-person, hybrid
        scheduled-start: uint, ;; block height
        session-fee: uint, ;; in microSTX
        cultural-context: (string-utf8 512),
        created-at: uint
    }
)

;; Resource Sharing History
(define-map resource-shares
    { resource-id: uint, borrower: principal }
    {
        quantity-borrowed: uint,
        borrowed-at: uint,
        return-due: uint,
        returned-at: (optional uint),
        condition-notes: (string-utf8 256)
    }
)

;; Skill Session Participants
(define-map session-participants
    { session-id: uint, participant: principal }
    {
        registered-at: uint,
        attendance-confirmed: bool,
        completion-status: bool,
        feedback-rating: (optional uint),
        feedback-comment: (optional (string-utf8 512))
    }
)

;; Community Member Profiles
(define-map member-profiles
    principal
    {
        display-name: (string-utf8 64),
        cultural-background: (string-utf8 128),
        skill-specialties: (list 5 (string-utf8 64)),
        sharing-preferences: (string-utf8 256),
        reputation-score: uint,
        total-shares: uint,
        total-teaches: uint,
        joined-at: uint
    }
)

;; Resource Trade Requests
(define-map trade-requests
    uint
    {
        requester: principal,
        resource-wanted: (string-utf8 128),
        quantity-needed: uint,
        resource-offered: (string-utf8 128),
        quantity-offered: uint,
        trade-reason: (string-utf8 256),
        status: (string-utf8 32), ;; pending, accepted, completed, cancelled
        created-at: uint,
        expires-at: uint
    }
)

(define-data-var next-trade-id uint u1)

;; ===================
;; RESOURCE MANAGEMENT
;; ===================

;; Add material resource to community pool
(define-public (add-resource
    (resource-name (string-utf8 128))
    (resource-type (string-utf8 64))
    (quantity-available uint)
    (unit-measure (string-utf8 32))
    (quality-grade uint)
    (cultural-significance (string-utf8 256))
    (origin-location (string-utf8 128))
    (shared-publicly bool)
    (trade-preference (string-utf8 256)))
    (let ((resource-id (var-get next-resource-id)))
        (asserts! (> (len resource-name) u0) ERR_INVALID_INPUT)
        (asserts! (and (>= quality-grade u1) (<= quality-grade u10)) ERR_INVALID_INPUT)

        (map-set material-resources resource-id
            {
                resource-name: resource-name,
                resource-type: resource-type,
                quantity-available: quantity-available,
                unit-measure: unit-measure,
                quality-grade: quality-grade,
                cultural-significance: cultural-significance,
                origin-location: origin-location,
                owner: tx-sender,
                shared-publicly: shared-publicly,
                trade-preference: trade-preference,
                created-at: stacks-block-height,
                last-updated: stacks-block-height
            })

        (var-set next-resource-id (+ resource-id u1))
        (update-member-shares tx-sender u1)
        (ok resource-id)))

;; Borrow resource from community
(define-public (borrow-resource (resource-id uint) (quantity uint) (return-blocks uint))
    (let ((resource (unwrap! (map-get? material-resources resource-id) ERR_RESOURCE_NOT_FOUND)))
        (asserts! (<= quantity (get quantity-available resource)) ERR_INSUFFICIENT_QUANTITY)
        (asserts! (get shared-publicly resource) ERR_NOT_AUTHORIZED)

        ;; Update resource quantity
        (map-set material-resources resource-id
            (merge resource {
                quantity-available: (- (get quantity-available resource) quantity),
                last-updated: stacks-block-height
            }))

        ;; Record the borrowing
        (map-set resource-shares
            { resource-id: resource-id, borrower: tx-sender }
            {
                quantity-borrowed: quantity,
                borrowed-at: stacks-block-height,
                return-due: (+ stacks-block-height return-blocks),
                returned-at: none,
                condition-notes: u""
            })
        (ok true)))

;; Return borrowed resource
(define-public (return-resource (resource-id uint) (condition-notes (string-utf8 256)))
    (let ((resource (unwrap! (map-get? material-resources resource-id) ERR_RESOURCE_NOT_FOUND))
          (share-record (unwrap! (map-get? resource-shares
            { resource-id: resource-id, borrower: tx-sender }) ERR_RESOURCE_NOT_FOUND)))

        ;; Update resource quantity
        (map-set material-resources resource-id
            (merge resource {
                quantity-available: (+ (get quantity-available resource)
                                     (get quantity-borrowed share-record)),
                last-updated: stacks-block-height
            }))

        ;; Mark as returned
        (map-set resource-shares
            { resource-id: resource-id, borrower: tx-sender }
            (merge share-record {
                returned-at: (some stacks-block-height),
                condition-notes: condition-notes
            }))
        (ok true)))

;; ===================
;; SKILL SHARING
;; ===================

;; Create skill sharing session
(define-public (create-skill-session
    (session-title (string-utf8 128))
    (technique-focus (string-utf8 128))
    (max-participants uint)
    (skill-level-required uint)
    (session-duration uint)
    (materials-provided bool)
    (location-type (string-utf8 64))
    (blocks-until-start uint)
    (session-fee uint)
    (cultural-context (string-utf8 512)))
    (let ((session-id (var-get next-skill-share-id)))
        (asserts! (> (len session-title) u0) ERR_INVALID_INPUT)
        (asserts! (<= skill-level-required u10) ERR_INVALID_INPUT)

        (map-set skill-sharing-sessions session-id
            {
                session-title: session-title,
                technique-focus: technique-focus,
                instructor: tx-sender,
                max-participants: max-participants,
                current-participants: u0,
                skill-level-required: skill-level-required,
                session-duration: session-duration,
                materials-provided: materials-provided,
                location-type: location-type,
                scheduled-start: (+ stacks-block-height blocks-until-start),
                session-fee: session-fee,
                cultural-context: cultural-context,
                created-at: stacks-block-height
            })

        (var-set next-skill-share-id (+ session-id u1))
        (update-member-teaches tx-sender u1)
        (ok session-id)))

;; Register for skill sharing session
(define-public (register-for-session (session-id uint))
    (let ((session (unwrap! (map-get? skill-sharing-sessions session-id) ERR_SKILL_SHARE_NOT_FOUND)))
        (asserts! (< (get current-participants session) (get max-participants session)) ERR_INVALID_INPUT)

        ;; Update participant count
        (map-set skill-sharing-sessions session-id
            (merge session { current-participants: (+ (get current-participants session) u1) }))

        ;; Register participant
        (map-set session-participants
            { session-id: session-id, participant: tx-sender }
            {
                registered-at: stacks-block-height,
                attendance-confirmed: false,
                completion-status: false,
                feedback-rating: none,
                feedback-comment: none
            })
        (ok true)))

;; ===================
;; COMMUNITY FEATURES
;; ===================

;; Create member profile
(define-public (create-member-profile
    (display-name (string-utf8 64))
    (cultural-background (string-utf8 128))
    (skill-specialties (list 5 (string-utf8 64)))
    (sharing-preferences (string-utf8 256)))
    (begin
        (map-set member-profiles tx-sender
            {
                display-name: display-name,
                cultural-background: cultural-background,
                skill-specialties: skill-specialties,
                sharing-preferences: sharing-preferences,
                reputation-score: u100,
                total-shares: u0,
                total-teaches: u0,
                joined-at: stacks-block-height
            })
        (ok true)))

;; Create trade request
(define-public (create-trade-request
    (resource-wanted (string-utf8 128))
    (quantity-needed uint)
    (resource-offered (string-utf8 128))
    (quantity-offered uint)
    (trade-reason (string-utf8 256))
    (expires-in-blocks uint))
    (let ((trade-id (var-get next-trade-id)))
        (map-set trade-requests trade-id
            {
                requester: tx-sender,
                resource-wanted: resource-wanted,
                quantity-needed: quantity-needed,
                resource-offered: resource-offered,
                quantity-offered: quantity-offered,
                trade-reason: trade-reason,
                status: u"pending",
                created-at: stacks-block-height,
                expires-at: (+ stacks-block-height expires-in-blocks)
            })
        (var-set next-trade-id (+ trade-id u1))
        (ok trade-id)))

;; ===================
;; HELPER FUNCTIONS
;; ===================

;; Update member sharing count
(define-private (update-member-shares (member principal) (increment uint))
    (let ((profile (map-get? member-profiles member)))
        (match profile
            existing-profile (map-set member-profiles member
                (merge existing-profile { total-shares: (+ (get total-shares existing-profile) increment) }))
            true)))

;; Update member teaching count
(define-private (update-member-teaches (member principal) (increment uint))
    (let ((profile (map-get? member-profiles member)))
        (match profile
            existing-profile (map-set member-profiles member
                (merge existing-profile { total-teaches: (+ (get total-teaches existing-profile) increment) }))
            true)))

;; ===================
;; READ-ONLY FUNCTIONS
;; ===================

;; Get resource details
(define-read-only (get-resource (resource-id uint))
    (map-get? material-resources resource-id))

;; Get skill session details
(define-read-only (get-skill-session (session-id uint))
    (map-get? skill-sharing-sessions session-id))

;; Get member profile
(define-read-only (get-member-profile (member principal))
    (map-get? member-profiles member))

;; Get trade request
(define-read-only (get-trade-request (trade-id uint))
    (map-get? trade-requests trade-id))

;; Get session participation
(define-read-only (get-session-participation (session-id uint) (participant principal))
    (map-get? session-participants { session-id: session-id, participant: participant }))

;; Get resource sharing record
(define-read-only (get-resource-share (resource-id uint) (borrower principal))
    (map-get? resource-shares { resource-id: resource-id, borrower: borrower }))
