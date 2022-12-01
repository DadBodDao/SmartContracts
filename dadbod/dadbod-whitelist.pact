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
    whitelisted:bool
    available-free:integer
    available-discounts:integer
    discount:decimal
  )
  (deftable whitelisted:{whitelist})

  (defun update-whitelisted:[string] 
    (
      whitelisted:[object{whitelist}]
    )
    @doc "Updates the whitelist information with the provided. \
    \ If an account already exists, the available is summed, \
    \ and the discount is overwritten"
    (with-capability (OPS)
      (map (update-whitelist) whitelisted)
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
        (whitelist-id (get-whitelist-id (at "collection" info) (at "account" info)))
      )  
      (with-default-read whitelist-id
        { "collection": (at "collection" info)
        , "account": (at "accont" info)
        , "whitelisted": false
        , "available-free": 0
        , "available-discounts": 0
        , "discount": 0.0
        }
        { "collection":= collection
        , "account":= account
        , "whitelisted":= whitelisted
        , "available-free":= curr-available-free
        , "available-discounts":= curr-available-discounts
        , "discount":= curr-discount
        }
        (let
          (
            (avail-free (+ curr-available-free (at "available-free" info)))
            (avail-discount (+ curr-available-free (at "available-free" info)))
          )  
          (enforce (!= "" collection))
          (enforce (>= avail-free 0) "Error: Available free must be >= 0")
          (enforce (>= avail-discount 0) "Error: Available discount must be >= 0")
          (enforce (>= discount 0.0) "Error: Discount must be >= 0.0")

          (write whitelisters whitelist-id
            { "collection" collection
            , "account": account
            , "whitelisted": (at "whitelisted" info)
            , "available-free": avail-free
            , "available-discounts": avail-discount
            , "discount": (at "discount" info)
            }
          )
        )
      )
    )
  )

  (defun decrement-available-free-with-ops 
    (collection:string account:string amount:integer)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OPS)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free-with-owner 
    (collection:string account:string amount:integer)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OWNER account)
      (decrement-available-free collection account amount)
    )
  )

  (defun decrement-available-free
    (collection:string account:string amount:integer)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id (get-whitelist-id (at "collection" info) (at "account" info)))
      )  
      (with-read whitelist-id
        { "available-free":= curr-available-free
        }
        (let
          (
            (avail (- curr-available-free amount))
          )
          (enforce (>= avail 0) "Error: Available free must be >= 0")

          (update whitelisters whitelist-id
            { "available-free": avail
            }
          )
        )
      )
    )
  )

  (defun decrement-available-discount-with-ops 
    (collection:string account:string amount:integer)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OPS)
      (decrement-available-discount collection account amount)
    )
  )

  (defun decrement-available-discount-with-owner 
    (collection:string account:string amount:integer)
    @doc "Decreases the available free mints by the amount provided. \
    \ Must enforce that resulting available is not negative."

    (with-capability (OWNER account)
      (decrement-available-discount collection account amount)
    )
  )

  (defun decrement-available-discount
    (collection:string account:string amount:integer)
    @doc "Decreases the available discounted mints by the amount provided. \
    \ Must enforce that resulting available is not negative."
    
    (require-capability (WL_MANAGE))

    (let
      (
        (whitelist-id (get-whitelist-id (at "collection" info) (at "account" info)))
      )  
      (with-read whitelist-id
        { "available-discount":= curr-available-discount
        }
        (let
          (
            (avail (- curr-available-discount amount))
          )
          (enforce (>= avail 0) "Error: Available discount must be >= 0")

          (update whitelisters whitelist-id
            { "available-discount": avail
            }
          )
        )
      )
    )
  )

  (defun enforce-whitelisted:bool (collection:string account:string)
    @doc "Ensures that the account is whitelisted"
    (with-default-read whitelisters (get-whitelist-id collection account)
      { "collection": collection
      , "account": account
      , "whitelisted": false
      , "available-free": 0
      , "available-discounts": 0
      , "discount": 0.0
      }
      { "whitelisted":= wl }
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

  (defun get-available-free:integer
    (collection:string account:string)
    @doc "Get the available free mints for the account"
    (read whitelisted ["available-free"] (get-whitelist-id collection account))
  )

  (defun get-available-discounts:integer
    (collection:string account:string)
    @doc "Get the available discounted mints for the account"
    (read whitelisted ["available-discounts"] (get-whitelist-id collection account))
  )

  (defun get-discount:decimal 
    (collection:string account:string)
    @doc "Get the discount for the given account"
    (read whitelisted ["discount"] (get-whitelist-id collection account))
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