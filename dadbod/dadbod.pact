(namespace "free")

(module dadbod GOV
  @doc "This code is meant to be copy-pasted into a different smart contract."

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS_INTERNAL ()
    (compose-capability (RESERVE))
    (compose-capability (MINT))
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun rotate-ops-from-gov (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (GOV)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops:string (guard:guard)
    @doc "Requires OPS. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops-internal:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."
    (require-capability (OPS_INTERNAL))

    (update m-guards OPS_GUARD
      { "guard": guard }  
    )

    "Rotated OPS to a new guard"
  )

  (defun get-gov-guard:guard ()
    @doc "Gets the current gov guard and returns it"
    (at "guard" (read m-guards GOV_GUARD))
  )

  (defun get-ops-guard:guard ()
    @doc "Gets the current ops guard and returns it"
    (at "guard" (read m-guards OPS_GUARD))
  )

  (defun init-perms:string (gov:guard ops:guard)
    @doc "Initializes the guards and creates the tables for the module"

    ;; This is only vulnerable if GOV_GUARD doesn't exist
    ;; Which means it's only vulnerable if you don't call 
    ;; init when you deploy the contract.
    ;; So let us be sure that init is called. =)
    (insert m-guards GOV_GUARD
      { "guard": gov }  
    )
    (insert m-guards OPS_GUARD
      { "guard": ops }  
    )
  )

  ;; -------------------------------
  ;; String Values

  (defschema value
    @doc "Stores string values"
    value:string
  )
  (deftable values:{value})

  (defun update-string-value (val-id:string value:string)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write values val-id
        { "value": value }
      )
    )
  )

  (defun get-string-value:string (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read values val-id ["value"]))
  )

  ;; -------------------------------
  ;; Bank

  (defconst BANK_ACCOUNT:string "BANK")

  (defun get-bank:string ()
    (get-string-value BANK_ACCOUNT)
  )

  ;; -------------------------------
  ;; Collections

  (defconst COLLECTION_TYPE_BOD:string "BOD")
  (defconst COLLECTION_TYPE_ITEM:string "ITEM")

  (defconst COLLECTION_STATUS_OPEN:string "OPEN"
    @doc "Open collection means it can be reserved by anyone.")
  (defconst COLLECTION_STATUS_WHITELIST:string "WHITELIST"
    @doc "Whitelist collection means it can only be reserved \
    \ by whitelisted addresses")
  (defconst COLLECTION_STATUS_WHITELIST_FREE:string "WHITELIST_FREE"
    @doc "Whitelist collection means it can only be reserved \
    \ by whitelisted addresses")
  (defconst COLLECTION_STATUS_CLOSED:string "CLOSED"
    @doc "Closed means it can only be reserved by an admin.")

  (defschema bod-collection-info
    is-dadbod:bool
    misc:string
  )
  (defschema item-collection-info
    is-original:bool
    item-id:string
    misc:string
  )
  (defschema collection-tranche
    @doc "A tier in a collection. Used to create tranches for a sale."
    min-supply:decimal
    price:decimal
  )
  (defschema collection
    @doc "The data for a collection. ID is the name."
    name:string
    status:string
    start-date:time
    supply-minted:decimal
    total-supply:decimal
    payment-token:module{fungible-v2}
    type:string
    info:object
    tranches:[object:{collection-tranche}]
  )
  (deftable collections:{collection})

  (defschema in-create-collection
    name:string
    status:string
    start-date:string
    total-supply:decimal
    type:string
    info:object
    tranches:[object:{collection-tranche}]
  )

  (defun create-collection:string 
    (
      c:object{in-create-collection}
      payment-token:module{fungible-v2}
    )
    @doc "Creates a new collection with the given information"
    
    (with-capability (OPS)
      (let
        (
          (name:string (at "name" c))
          (type:string (at "type" c))
          (status:string (at "status" c))
          (tranches:[object:{collection-tranche}] (at "tranches" c))
        )
        ;; Check the types
        (enforce 
          (or (= type COLLECTION_TYPE_BOD) (= type COLLECTION_TYPE_ITEM))
          "Type must be a bod or an item")
        (enforce-valid-status status)
        (enforce (> (length tranches) 0) "Tranches must be provided")

        (insert collections name
          { "name": (at "name" c)
          , "status": (at "status" c)
          , "start-date": (time (at "start-date" c))
          , "supply-minted": 0.0
          , "total-supply": (at "total-supply" c)
          , "payment-token": payment-token
          , "type": (at "type" c)
          , "info": (at "info" c)
          , "tranches": (at "tranches" c)
          }
        )
      )
    )
  )

  (defun get-all-collections:[object{collection}] ()
    (select collections (constantly true))
  )

  (defun get-collection:object{collection} (name:string)
    (read collections name)
  )

  (defun get-incomplete-collections:[object{collection}] ()
    (filter (collection-incomplete) (get-all-collections))
  )

  (defun get-collection-info:object (name:string)
    (at "info" (read collections name ["info"]))
  )

  (defun get-next-id-for-collection:string (name:string)
    (format "{}" 
      [(floor (at "supply-minted" (read collections name ["supply-minted"])))])
  )

  (defun collection-incomplete:bool (c:object{collection})
    (!= (at "supply-minted" c) (at "total-supply" c))
  )

  (defun get-current-tranche 
    (
      supply-minted:decimal 
      tranches:[object:{collection-tranche}]
    )
    @doc "Gets the tranche of the collection with the provided name."
    
    (let* 
      (
        (in-bounds ;; Lambda function to determine if hash rate could in in tier
          (lambda (t) 
            (<= (at "min-supply" t) supply-minted)
          )
        )
        (li ;; Sort and filter based on in bounds
          (sort ["min-supply"] (filter (in-bounds) tranches)))
      )

      ;; Return the last element in the list, based on sort
      (at 
        (- (length li) 1)
        li
      )
    )
  )

  (defun get-current-price-for-collection:decimal (name:string)
    (let
      (
        (c:object{collection} (get-collection name))  
      )
      (at "price" 
        (get-current-tranche 
          (at "supply-minted" c) 
          (at "tranches" c)
        )
      )
    )
  )

  (defun update-status-for-collection:string (name:string new-status:string)
    @doc "Updates the collection status to the provided one."
    (enforce-valid-status new-status)

    (with-capability (OPS)
      (update collections name
        { "status": new-status }  
      )
    )
  )

  (defun enforce-valid-status:bool (status:string)
    (enforce 
      (contains status 
        [
          COLLECTION_STATUS_OPEN 
          COLLECTION_STATUS_CLOSED 
          COLLECTION_STATUS_WHITELIST
          COLLECTION_STATUS_WHITELIST_FREE
        ]
      )
      "Status is not valid")
  )

  ;; -------------------------------
  ;; Reservations

  (defcap RESERVE ()
    true
  )

  (defcap RESERVED (reservation:object{reservation})
    @event
    true
  )

  (defschema reservation
    @doc "A reserved NFT in a collection. OPS will use these to mint NFTs. \
    \ ID is collection|id."
    collection:string
    id:string
    account:string
    minted:bool
  )
  (deftable reservations:{reservation})

  (defun reserve-admin:string (collection-name:string account:string)
    @doc "Reserves an NFT from the collection if possible."

    (with-capability (OPS)
      (reserve-internal collection-name account 1.0)
    )
  )

  (defun reserve-free:string (collection-name:string account:string)
    @doc "Reserves for free, if the account has available free mints"
    (with-capability (RESERVE)
      (let 
        (
          (avail:decimal 
            (free.dadbod-whitelist.get-available-free collection-name account))
        )
        (enforce (> avail 0.0) "No available free mints.")
        (free.dadbod-whitelist.decrement-available-free-with-owner
          collection-name account 1.0)
        (reserve-internal collection-name account 1.0)
      )
    )
  )

  (defun reserve-discount:string (collection-name:string account:string)
    @doc "Reserves for a discount, if the account has available discounted mints"
    (with-capability (RESERVE)
      (let 
        (
          (avail:decimal 
            (free.dadbod-whitelist.get-available-discounts collection-name account))
          (discount:decimal (free.dadbod-whitelist.get-discount collection-name account))
        )
        (enforce (> avail 0.0) "No available discounted mints.")
        (free.dadbod-whitelist.decrement-available-discounts-with-owner
          collection-name account 1.0)
        (reserve-internal collection-name account discount)
      )
    )
  )

  (defun reserve:string (collection-name:string account:string)
    @doc "Reserves an NFT from the collection if possible."

    (with-capability (RESERVE)
      (reserve-internal collection-name account 0.0)
    )
  )

  (defun reserve-internal:string 
    (
      collection-name:string
      account:string
      discount:decimal
    )
    @doc "Private function for reservation"  

    (require-capability (RESERVE))

    (with-read collections collection-name
      { "status":= status
      , "start-date":= start-date
      , "supply-minted":= supply-minted
      , "total-supply":= total-supply
      , "payment-token":= payment-token:module{fungible-v2}
      , "type":= type
      , "info":= info
      , "tranches":= tranches
      }
      (enforce (>= (curr-time) start-date) "The mint hasn't started yet")
      (enforce (< supply-minted total-supply) "Can't mint more than total supply")
      (enforce (!= status COLLECTION_STATUS_CLOSED) "Cannot reserve from a closed collection")

      (if (= status COLLECTION_STATUS_WHITELIST)
        (free.dadbod-whitelist.enforce-whitelisted collection-name account)
        []
      )
      (if (= status COLLECTION_STATUS_WHITELIST_FREE)
        [
          (free.dadbod-whitelist.enforce-whitelisted collection-name account)
          (enforce (= discount 1.0) "Free must be free")
        ]
        []
      )

      (let
        (
          (price:decimal 
            (* 
              (- 1.0 discount)
              (at "price" (get-current-tranche supply-minted tranches))
            )
          )
          (id:string (format "{}" [(floor supply-minted)]))
        )
        
        (if (> price 0.0)
          (payment-token::transfer account (get-bank) price)
          []
        )

        ; Use the current supply minted as an id and insert into reservations
        (let 
          (
            (r:object{reservation} 
                { "collection": collection-name
              , "id": id
              , "account": account 
              , "minted": false
              }
            )
          )
          (insert reservations 
            (get-id collection-name id)
            r
          )

          (emit-event (RESERVED r))
  
          ; Increment the supply minted
          (update collections collection-name
            { "supply-minted": (+ supply-minted 1.0) }  
          )
        )
      )
    )
  )

  (defun get-all-reservations:[object{reservation}] ()
    (select reservations (constantly true))
  )

  (defun get-reservation:object{reservation} 
    (collection-name:string id:string)
    (read reservations (get-id collection-name id))
  )

  (defun get-unminted-reservations:[object{reservation}] ()
    (select reservations (where "minted" (= false)))
  )

  (defun get-reservations-for-account:[object{reservation}] 
    (account:string)
    (select reservations (where "account" (= account)))  
  )

  ;; -------------------------------
  ;; (Dad)Bods and Items

  (defcap MINT ()
    true
  )

  (defschema bod
    @doc "The details for an individual bod (DadBod or Bod). \
    \ Key is collection|id"
    collection:string
    id:string
    ledger-id:string
    account:string
    misc:object
    genetics-skin:object
    genetics-hair:object
    genetics-facial-hair:object
    genetics-eye-left:object
    genetics-eye-right:object
    genetics-scar:object
    item-head:string ;; Refers to an item
    item-eyes:string ;; Refers to an item
    item-ear:string ;; Refers to an item
    item-body:string ;; Refers to an item
    item-back:string ;; Refers to an item
    item-hand-left:string ;; Refers to an item
    item-hand-right:string ;; Refers to an item
  )
  (deftable bods:{bod})

  (defschema item
    @doc "The details for an individual item"
    collection:string
    id:string
    ledger-id:string
    account:string 
    info:object
    misc:object
    equipped-to:string ;; Refers to a bod
  )
  (deftable items:{item})

  (defschema in-mint-bod
    @doc "The details for an individual bod (DadBod or Bod). \
    \ Key is collection|id"
    collection:string
    id:string 
    account:string
    misc:object
    genetics-skin:object
    genetics-hair:object
    genetics-facial-hair:object
    genetics-eye-left:object
    genetics-eye-right:object
    genetics-scar:object
    item-head:object ;; Refers to an item
    item-eyes:object ;; Refers to an item
    item-ear:object ;; Refers to an item
    item-body:object ;; Refers to an item
    item-back:object ;; Refers to an item
    item-hand-left:object ;; Refers to an item
    item-hand-right:object ;; Refers to an item
  )
  (defschema in-mint-item
    collection:string
    id:string 
    account:string
    misc:object ;; Uses the info from the collection to fill this in
  )

  (defun mint-bod:string 
    (
      in:object{in-mint-bod}
      guard:guard
    )
    @doc "Mints a bod: Creates a ledger token, mints one, and does the same for \
    \ each existing item. Returns the collection|id."
    
    (with-capability (OPS)
      (create-bod in guard)
    )
  )

  (defun create-bod:string 
    (
      in:object{in-mint-bod}
      guard:guard
    )
    @doc "Private function to create a bod: Creates a ledger token, mints one, and does the same for \
    \ each existing item. Returns the collection|id."
    
    (require-capability (MINT))

    (let*
      (
        (id:string 
          (if (= "" (at "id" in)) 
            (get-next-id-for-collection (at "collection" in)) 
            (at "id" in)
          )
        )
        (token-id:string (get-id (at "collection" in) id))
        (info:object (get-collection-info (at "collection" in)))
      )

       ;; Create the marmalade NFT
      (create-ledger-nft 
        (at "account" in)
        guard
        (at "collection" in)
        id
        info
      )

      ;; Update the reservation if it has one
      ;; Otherwise, update the supply we minted from
      (if (contains "has-reservation" (at "misc" in))
        (update reservations token-id
          { "minted": true }
        )
        (with-read collections (at "collection" in)
          { "supply-minted":= supply-minted
          , "total-supply":= total-supply 
          }
          (enforce (< supply-minted total-supply) 
            "Can't create bod, not enough supply in collection")
          ; Increment the supply minted
          (update collections (at "collection" in)
            { "supply-minted": (+ supply-minted 1.0) }  
          )
        )
      )

      ;; Create the bod
      (insert bods token-id
        { "collection": (at "collection" in) 
        , "id": id
        , "ledger-id": token-id 
        , "account": (at "account" in)
        , "misc": (at "misc" in)
        , "genetics-skin": (at "genetics-skin" in)
        , "genetics-hair": (at "genetics-hair" in)
        , "genetics-facial-hair": (at "genetics-facial-hair" in)
        , "genetics-eye-left": (at "genetics-eye-left" in) 
        , "genetics-eye-right": (at "genetics-eye-right" in) 
        , "genetics-scar": (at "genetics-scar" in) 
        , "item-head": (if (contains "collection" (at "item-head" in)) 
          (create-item (at "item-head" in) guard token-id) "")
        , "item-eyes": (if (contains "collection" (at "item-eyes" in)) 
          (create-item (at "item-eyes" in) guard token-id) "")
        , "item-ear": (if (contains "collection" (at "item-ear" in)) 
          (create-item (at "item-ear" in) guard token-id) "")
        , "item-body": (if (contains "collection" (at "item-body" in)) 
          (create-item (at "item-body" in) guard token-id) "")
        , "item-back": (if (contains "collection" (at "item-back" in)) 
          (create-item (at "item-back" in) guard token-id) "")
        , "item-hand-left": (if (contains "collection" (at "item-hand-left" in)) 
          (create-item (at "item-hand-left" in) guard token-id) "")
        , "item-hand-right": (if (contains "collection" (at "item-hand-right" in)) 
          (create-item (at "item-hand-right" in) guard token-id) "")
        }
      )

      token-id
    )
  )

  (defun mint-item:string
    (
      in:object{in-mint-item}
      guard:guard
    )
    @doc "Ops function to create an item."
    (with-capability (OPS)
      (create-item in guard "")
    )
  )

  (defun create-item:string 
    (
      in:object{in-mint-item}
      guard:guard
      equipped-to:string
    )
    @doc "Private function to create an item: Creates a ledger token, mints one, \
    \ then creates the item based on information. Returns the collection|id."
    
    (require-capability (MINT))

    (let*
      (
        (id:string 
          (if (= "" (at "id" in)) 
            (get-next-id-for-collection (at "collection" in)) 
            (at "id" in)
          )
        )
        (token-id:string (get-id (at "collection" in) id))
        (info:object (get-collection-info (at "collection" in)))
      )

      ;; Create the marmalade NFT
      (create-ledger-nft 
        (at "account" in)
        guard
        (at "collection" in)
        id
        info
      )

      ;; Update the reservation if it has one
      ;; Otherwise, update the supply we minted from
      (if (contains "has-reservation" (at "misc" in))
        (update reservations token-id
          { "minted": true }
        )
        (with-read collections (at "collection" in)
          { "supply-minted":= supply-minted
          , "total-supply":= total-supply 
          }
          (enforce (< supply-minted total-supply) 
            "Can't create item, not enough supply in collection")
          ; Increment the supply minted
          (update collections (at "collection" in)
            { "supply-minted": (+ supply-minted 1.0) }  
          )
        )
      )

      ;; Create the item
      (insert items token-id
        { "collection": (at "collection" in) 
        , "id": id
        , "ledger-id": token-id 
        , "account": (at "account" in)
        , "info": info
        , "misc": (at "misc" in)
        , "equipped-to": equipped-to
        }
      )

      token-id
    )
  )

  (defun create-ledger-nft 
    (
      account:string
      guard:guard
      collection-name:string
      id:string
      info:object
    )
    (require-capability (MINT))

    (let* 
      (
        (token-id:string (get-id collection-name id))
        (uri (kip.token-manifest.uri "module" "free.dadbod"))
        (datum 
          (kip.token-manifest.create-datum 
            uri { "collection": collection-name, "id": id, "info": info }))
        (manifest (kip.token-manifest.create-manifest uri [datum]))
      )
      
      (free.dadbod-ledger.create-token token-id 0 manifest (get-token-policy))
      (install-capability (free.dadbod-ledger.MINT token-id account 1.0))
      (free.dadbod-ledger.mint token-id account guard 1.0)
    )
  )

  (defun get-all-bods:[object{bod}] ()
    (select bods (constantly true))
  )

  (defun get-all-bods-for-account:[object{bod}] (account:string)
    (select bods (where "account" (= account)))
  )

  (defun get-bod:object{bod} 
    (collection:string id:string)
    (read bods (get-id collection id))  
  )

  (defun get-all-items:[object{item}] ()
    (select items (constantly true))
  )

  (defun get-all-items-for-account:[object{item}] (account:string)
    (select items (where "account" (= account)))
  )

  (defun get-item:object{item} 
    (collection:string id:string)
    (read items (get-id collection id))  
  )

  (defcap TRANSFER (account:string)
    true
  )

  (defun update-account-for-bod 
    (
      collection-name:string 
      id:string 
      account:string
      receiver:string
    )
    (with-capability (TRANSFER account)
      (update bods (get-id collection-name id)
        { "account": receiver }
      )
    )
  )

  ;; -------------------------------
  ;; Policy

  (defconst TOKEN_POLICY:string "POLICY")

  (defschema token-policy
    policy:module{kip.token-policy-v1}  
  )
  (deftable token-policies:{token-policy})

  (defun update-token-policy:string (p:module{kip.token-policy-v1})
    (with-capability (OPS)
      (write token-policies TOKEN_POLICY
        { "policy": p }  
      )
    )
  )

  (defun get-token-policy:module{kip.token-policy-v1} ()
    (at "policy" (read token-policies TOKEN_POLICY ["policy"]))
  )

  ;; -------------------------------
  ;; Utils

  (defun get-id (collection:string id:string)
    @doc "Returns collection|id"
    (concat [collection "|" id])
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"

    (at 'block-time (chain-data))
  )
)

(if (read-msg "init")
  [
    (create-table m-guards)
    (create-table values)
    (create-table collections)
    (create-table reservations)
    (create-table bods)
    (create-table items)
    (create-table token-policies)
    (init-perms (read-keyset "gov") (read-keyset "ops"))
    (update-string-value BANK_ACCOUNT (read-msg "bank-account"))
    (update-token-policy free.dadbod-policy)
  ]
  "Contract upgraded"
)