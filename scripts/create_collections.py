
from kadena_sdk.kadena_sdk import KadenaSdk
from kadena_sdk.key_pair import KeyPair

import argparse
parser = argparse.ArgumentParser()
parser.add_argument('-m') # Network, if this exists, it will use mainnet
parser.add_argument('-s') # Send, if this exists, this will send
args = parser.parse_args()

start_date = "2022-11-30T00:00:00Z"

def create_item(name, supply, info):
  return {
    "name": name,
    "status": "CLOSED",
    "start-date": start_date,
    "total-supply": supply,
    "type": "ITEM",
    "info": info,
    "tranches": [
      {
        "min-supply": 0.0,
        "price": 1.0
      }
    ]
  }

collections = [
  {
    "name": "dadbod",
    "status": "OPEN",
    "start-date": start_date,
    "total-supply": 9001.0,
    "type": "BOD",
    "info": {
      "is-dadbod": True
    },
    "tranches": [
      {
        "min-supply": 1000.0,
        "price": 10.0
      },
      {
        "min-supply": 2000.0,
        "price": 12.5
      },
      {
        "min-supply": 3000.0,
        "price": 15.0
      },
      {
        "min-supply": 4000.0,
        "price": 17.5
      },
      {
        "min-supply": 5000.0,
        "price": 20.0
      },
      {
        "min-supply": 6000.0,
        "price": 22.5
      },
      {
        "min-supply": 7000.0,
        "price": 25.0
      },
      {
        "min-supply": 8000.0,
        "price": 27.5
      },
      {
        "min-supply": 8500.0,
        "price": 30.0
      },
      {
        "min-supply": 9000.0,
        "price": 0.0
      }
    ]
  },
  {
    "name": "dadbod-wl",
    "status": "WHITELIST_FREE",
    "start-date": start_date,
    "total-supply": 199.0,
    "type": "BOD",
    "info": {
      "is-dadbod": True
    },
    "tranches": [
      {
        "min-supply": 0.0,
        "price": 10000.0
      }
    ]
  },
  # Head
  create_item('hat', 10000.0, 
    { "is-original": True, "type": "head", "item-id": "1"}),
  # Body
  create_item('clothing1', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "1"}),
  create_item('clothing2', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "2"}),
  create_item('clothing3', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "3"}),
  create_item('clothing4', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "4"}),
  create_item('clothing5', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "5"}),
  create_item('clothing6', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "6"}),
  create_item('clothing7', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "7"}),
  create_item('clothing8', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "8"}),
  create_item('clothing9', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "9"}),
  create_item('clothing10', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "10"}),
  create_item('clothing11', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "11"}),
  create_item('clothing12', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "12"}),
  create_item('clothing13', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "13"}),
  create_item('clothing14', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "14"}),
  create_item('clothing15', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "15"}),
  create_item('clothing16', 10000.0, 
    { "is-original": True, "type": "body", "item-id": "16"}),
]

MAINNET = {
  'base_url': 'https://api.chainweb.com',
  'network_id': 'mainnet01',
  'chain_id': '1',
}
TESTNET = {
  'base_url': 'https://api.testnet.chainweb.com',
  'network_id': 'testnet04',
  'chain_id': '1',
}
NETWORK = TESTNET if args.m == None else MAINNET
print(NETWORK)

key_pair = KeyPair('keys.json')
sdk = KadenaSdk(key_pair, 
  NETWORK['base_url'], 
  NETWORK['network_id'], 
  NETWORK['chain_id'])

for c in collections:
  payload = {
    "exec": {
      "data": {
        "c": c,
      },
      "code": '(free.dadbod.create-collection (read-msg "c") coin)',
    }
  }
  signers = [
    {
      "pubKey": key_pair.get_pub_key(),
      # Unrestricted signing key!
    }
  ]

  print()
  cmd = sdk.build_command(f'k:{key_pair.get_pub_key()}', payload, signers)
  result = sdk.local(cmd)
  print(result.text)
  if args.s != None:
    result = sdk.send(cmd)
    print(result.text)

  print()