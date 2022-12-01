(namespace "free")

(module dadbod-whitelist GOV 

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
    (compose-capability (WL_MANAGE))
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
  ;; Whitelist

  (defcap OWNER (account:string)
    (enforce-guard (at "guard" (coin.details account)))
    (compose-capability (WL_MANAGE))
  )
  (defcap WL_MANAGE () true)
  
  (defschema whitelist
    @doc "Stores all the whitelisters. ID is collection-account"
    collection:string
    account:string
    is-whitelisted:bool
    available-free:decimal
    available-discounts:decimal
    discount:decimal
  )
  (deftable whitelisted:{whitelist})

  (defun update-whitelisted:[string] 
    (
      whitelisted-info:[object{whitelist}]
    )
    @doc "Updates the whitelist information with the provided. \
    \ If an account already exists, the available is summed, \
    \ and the discount is overwritten"
    (with-capability (OPS)
      (map (update-whitelist) whitelisted-info)
    )
  )

  (defun update-whitelist:string 
    (info:object{whitelist})
    @doc "Updates an individual whitelist information with the provided. \
    \ If an account already exists, the available is summed, \
    \ and the discount is overwritten"
    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id 
          (get-whitelist-id (at "collection" info) (at "account" info)))
      )  

      (with-default-read whitelisted whitelist-id
        { "collection": (at "collection" info)
        , "account": (at "account" info)
        , "is-whitelisted": false
        , "available-free": 0.0
        , "available-discounts": 0.0
        , "discount": 0.0
        }
        { "collection":= collection
        , "account":= account
        , "is-whitelisted":= is-whitelisted
        , "available-free":= curr-available-free
        , "available-discounts":= curr-available-discounts
        , "discount":= curr-discount
        }

        (let
          (
            (discount:decimal (at "discount" info))
            (avail-free:decimal (+ curr-available-free (at "available-free" info)))
            (avail-discount:decimal (+ curr-available-discounts (at "available-discounts" info)))
          )
          (enforce (!= "" collection) "Collection name cannot be empty")
          (enforce (>= avail-free 0.0) "Error: Available free must be >= 0")
          (enforce (>= avail-discount 0.0) "Error: Available discount must be >= 0")
          (enforce (>= discount 0.0) "Error: Discount must be >= 0.0")

          (write whitelisted whitelist-id
            { "collection": collection
            , "account": account
            , "is-whitelisted": (at "is-whitelisted" info)
            , "available-free": avail-free
            , "available-discounts": avail-discount
            , "discount": discount
            }
          )
        )
      )
    )
  )

  (defun decrement-available-free-with-ops:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OPS)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free-with-owner:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OWNER account)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id (get-whitelist-id collection account))
      )  
      (with-read whitelisted whitelist-id
        { "available-free":= curr-available-free
        }
        (let
          (
            (avail (- curr-available-free amount))
          )
          (enforce (>= avail 0.0) "Error: Available free must be >= 0")

          (update whitelisted whitelist-id
            { "available-free": avail
            }
          )
        )
      )
    )
  )

  (defun decrement-available-discounts-with-ops:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OPS)
      (decrement-available-discounts collection account amount)
    )
  )

  (defun decrement-available-discounts-with-owner:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OWNER account)
      (decrement-available-discounts collection account amount)
    )
  )

  (defun decrement-available-discounts:string 
    (collection:string account:string amount:decimal)
    @doc "Decreases the available discounted mints by the amount provided. \
    \ Must enforce that resulting available is not negative."
    
    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id (get-whitelist-id collection account))
      )  
      (with-read whitelisted whitelist-id
        { "available-discounts":= curr-available-discounts
        }
        (let
          (
            (avail (- curr-available-discounts amount))
          )
          (enforce (>= avail 0.0) "Error: Available discounts must be >= 0")

          (update whitelisted whitelist-id
            { "available-discounts": avail
            }
          )
        )
      )
    )
  )

  (defun enforce-whitelisted:bool (collection:string account:string)
    @doc "Ensures that the account is whitelisted"
    (with-default-read whitelisted (get-whitelist-id collection account)
      { "collection": collection
      , "account": account
      , "is-whitelisted": false
      , "available-free": 0.0
      , "available-discounts": 0.0
      , "discount": 0.0
      }
      { "is-whitelisted":= wl }
      (enforce wl "Must be whitelisted")
    )
  )

  (defun get-all-whitelisted:[object{whitelist}] ()
    @doc "Gets all the whitelisted"
    (select whitelisted (constantly true))
  )

  (defun get-whitelist:object{whitelist} 
    (collection:string account:string)
    @doc "Get the whitelist info for the account"
    (read whitelisted (get-whitelist-id collection account))
  )

  (defun get-available-free:decimal
    (collection:string account:string)
    @doc "Get the available free mints for the account"
    (at "available-free" 
      (read whitelisted 
        (get-whitelist-id collection account) ["available-free"]))
  )

  (defun get-available-discounts:decimal
    (collection:string account:string)
    @doc "Get the available discounted mints for the account"
    (at "available-discounts" 
      (read whitelisted 
        (get-whitelist-id collection account) ["available-discounts"]))
  )

  (defun get-discount:decimal 
    (collection:string account:string)
    @doc "Get the discount for the given account"
    (at "discount" 
      (read whitelisted 
        (get-whitelist-id collection account) ["discount"]))
  )

  (defun get-whitelist-id:string (collection:string account:string)
    @doc "Creates the ID for the whitelist based on the collection and account"
    (concat [collection "|" account])
  )
)

(if (read-msg "init")
  [
    (create-table m-guards)
    (create-table whitelisted)
    (init-perms (read-keyset "gov") (read-keyset "ops"))
  ]
  "Contract upgraded"
)