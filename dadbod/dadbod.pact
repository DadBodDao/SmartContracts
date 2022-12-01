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
  (defconst COLLECTION_STATUS_CLOSED:string "CLOSED"
    @doc "Closed means it can only be reserved by an admin.")

  (defschema bod-collection-info
    is-dadbod:bool
    misc:string
  )
  (defschema item-collection-info
    is-original:bool
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
    start-date:time
    supply-minted:decimal
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
          (type:string (at "type" c))
          (status:string (at "status" c))
        )
      )
      ;; Check the types
      (enforce 
        (or (= type COLLECTION_TYPE_BOD) (= type COLLECTION_TYPE_ITEM))
        "Type must be a bod or an item")
      (enforce 
        (contains status 
          [
            COLLECTION_STATUS_OPEN 
            COLLECTION_STATUS_CLOSED 
            COLLECTION_STATUS_WHITELIST
          ]
        )
        "Status is not valid")
      (enforce (> (length tranches) 0) "Tranches must be provided")

      (insert collections name
        { "name": (at "name" c)
        , "status": (at "status" c)
        , "start-date": (at "start-date" c)
        , "supply-minted": (at "supply-minted" c)
        , "total-supply": (at "total-supply" c)
        , "payment-token": payment-token
        , "type": (at "type" c)
        , "info": (at "info" c)
        , "tranches": (at "tranches" c)
        }
      )
    )
  )

  (defun get-all-collections ()
    (select collections (constantly true))
  )

  (defun get-collection (name:string)
    (read collections name)
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
    )
    (at "price" 
      (get-current-tranche-of-collection 
        (at "supply-minted" c) 
        (at "tranches" c)
      )
    )
  )

  ;; -------------------------------
  ;; Reservations

  (defcap RESERVE ()
    true
  )

  (defcap RESERVED (reservation:object{reservation})
    @event
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

  (defun reserve-free:string (collection-name:string account:string)
    @doc "Reserves for free, if the account has available free mints"
    (with-capability (RESERVE)
      (let 
        (
          (avail:integer 
            (free.dadbod-whitelist.get-available-free collection-name account))
        )
        (enforce (> avail 0) "No available free mints.")
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
          (avail-discount:integer 
            (free.dadbod-whitelist.get-available-discounts collection-name account))
          (discount:decimal (free.dadbod-whitelist.get-discount collection-name account))
        )
        (enforce (> avail 0) "No available discounted mints.")
        (free.dadbod-whitelist.decrement-available-discount-with-owner
          collection-name account 1.0)
        (reserve-internal collection-name account discount)
      )
    )
  )

  (defun reserve-admin:string (collection-name:string account:string)
    @doc "Reserves an NFT from the collection if possible."

    (with-capability (OPS)
      (reserve-internal collection-name account 1.0)
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
      (enforce (> (curr-time) start-date) "The mint hasn't started yet")
      (enforce (< supply-minted total-supply) "Can't mint more than total supply")
      (enforce (!= status COLLECTION_STATUS_CLOSED))

      (if (= status COLLECTION_STATUS_WHITELIST)
        (free.dadbod-whitelist.enforce-whitelisted collection-name account)
        []
      )

      (let
        (
          (price:decimal 
            (* 
              (- 1.0 discount)
              (at "price" (get-current-tranch supply-minted tranches))
            )
          )
          (id:string (format "{}" [supply-minted]))
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
    genetics-skin:string
    genetics-hair:object
    genetics-facial-hair:object
    genetics-eye-left:string
    genetics-eye-right:string
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
    genetics-skin:string
    genetics-hair:object
    genetics-facial-hair:object
    genetics-eye-left:string
    genetics-eye-right:string
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
    misc:object
  )

  (defun mint-bod:string 
    (
      in:object{in-mint-bod}
    )
    @doc "Mints a bod: Creates a ledger token, mints one, and does the same for \
    \ each existing item. Returns the collection|id."
    
    (with-capability (OPS)
      (create-bod in)
    )
  )

  (defun create-bod:string 
    (
      in:object{in-mint-bod}
    )
    @doc "Private function to create a bod: Creates a ledger token, mints one, and does the same for \
    \ each existing item. Returns the collection|id."
    
    (require-capability (MINT))

    ;; Create the marmalade NFT
    (create-ledger-nft 
      (at "account" in)
      (at "collection" in)
      (at "id" in)
    )

    (let
      (
        (token-id:string (get-id (at "collection" in) (at "id" in)))
      )

      ;; Update the reservation if it has one
      (if (contains "has-reservation" (at "misc" in))
        (update reservations (get-id collection-name id)
          { "minted": true }
        )
        []
      )

      ;; Create the bod
      (insert bods token-id
        { "collection": (at "collection" in) 
        , "id": (at "id" in)
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
          (create-item (at "item-head" in)) "")
        , "item-eyes": (if (contains "collection" (at "item-eyes" in)) 
          (create-item (at "item-eyes" in)) "")
        , "item-ear": (if (contains "collection" (at "item-ear" in)) 
          (create-item (at "item-ear" in)) "")
        , "item-body": (if (contains "collection" (at "item-body" in)) 
          (create-item (at "item-body" in)) "")
        , "item-back": (if (contains "collection" (at "item-back" in)) 
          (create-item (at "item-back" in)) "")
        , "item-hand-left": (if (contains "collection" (at "item-hand-left" in)) 
          (create-item (at "item-hand-left" in)) "")
        , "item-hand-right": (if (contains "collection" (at "item-hand-right" in)) 
          (create-item (at "item-hand-right" in)) "")
        }
      )

      token-id
    )
  )

  (defun mint-item:string
    (
      in:object{in-mint-item}
    )
    @doc "Ops function to create an item."
    (with-capability (OPS)
      (create-item in "")
    )
  )

  (defun create-item:string 
    (
      in:object{in-mint-item}
      equipped-to:string
    )
    @doc "Private function to create an item: Creates a ledger token, mints one, \
    \ then creates the item based on information. Returns the collection|id."
    
    (require-capability (MINT))

    ;; Create the marmalade NFT
    (create-ledger-nft 
      (at "account" in)
      (at "collection" in)
      (at "id" in)  
    )

    (let
      (
        (token-id:string (get-id (at "collection" in) (at "id" in)))
      )  

      ;; Update the reservation if it has one
      (if (contains "has-reservation" (at "misc" in))
        (update reservations (get-id collection-name id)
          { "minted": true }
        )
        []
      )

      ;; Create the item
      (insert bods token-id
        { "collection": (at "collection" in) 
        , "id": (at "id" in)
        , "ledger-id": token-id 
        , "account": (at "account" in)
        , "misc": (at "misc" in)
        , "equipped-to": "equipped-to"
        }
      )

      token-id
    )
  )

  (defun create-ledger-nft 
    (
      account:string
      collection-name:string
      id:string
    )
    (require-capability (MINT))

    (let* 
      (
        (token-id:string (get-id collection-name id))
        (uri (kip.token-manifest.uri "module" "free.dadbod"))
        (datum 
          (kip.token-manifest.create-datum 
            uri { "collection": collection-name, "id": id }))
        (manifest (kip.token-manifest.create-manifest uri [datum]))
      )
      
      (free.dadbod-ledger.create-token token-id precision manifest policy)
      (free.dadbod-ledger.mint token-id account 1.0)
    )
  )

  (defun get-all-bods:[object{bod}] ()
    (select bods (constantly true))
  )

  (defun get-all-bods-for-account:[object{bod}] (account:string)
    (select bods (where "account" (= account)))
  )

  (defun get-all-items:[object{item}] ()
    (select items (constantly true))
  )

  (defun get-all-items-for-account:[object{item}] (account:string)
    (select items (where "account" (= account)))
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
    (require-capability (TRANSFER account))

    (update bods (get-id collection-name id)
      { "account": receiver }
    )
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
    (create-table collections)
    (create-table reservations)
    (create-table bods)
    (create-table items)
    (init-perms (read-keyset "gov") (read-keyset "ops"))
    (update-string-value BANK_ACCOUNT (read-msg "bank-account"))
  ]
  "Contract upgraded"
)