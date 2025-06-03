#!/usr/bin/env bash

set -e

script_dir=$(dirname "$0")

rt_path="$script_dir/../../../../target/release/wbuild/people-polkadot-runtime/people_polkadot_runtime.compact.compressed.wasm"
para_id=1004

# build the chain spec we'll manipulate
chain-spec-builder --chain-spec-path "$script_dir/chain-spec-plain.json" create --runtime-wasm-path $rt_path default > /dev/null 2>&1

# convert runtime to hex
cat $rt_path | od -A n -v -t x1 | tr -d ' \n' > "$script_dir/rt-hex.txt" 2>/dev/null

# replace the runtime in the spec with the given runtime and set some values to production
# Boot nodes, invulnerables, and session keys from https://github.com/paritytech/devops/issues/2847
#
# Note: This is a testnet runtime. Each invulnerable's Aura key is also used as its AccountId. This
# is not recommended in value-bearing networks.
cat "$script_dir/chain-spec-plain.json" | jq --rawfile code "$script_dir/rt-hex.txt" '.genesis.runtimeGenesis.code = ("0x" + $code)' \
    | jq '.name = "Polkadot People"' \
    | jq '.id = "people-polkadot"' \
    | jq '.chainType = "Local"' \
    | jq '.bootNodes = [
		"/dns/polkadot-people-connect-0.polkadot.io/tcp/30334/p2p/12D3KooWP7BoJ7nAF9QnsreN8Eft1yHNUhvhxFiQyKFEUePi9mu3",
     "/dns/polkadot-people-connect-1.polkadot.io/tcp/30334/p2p/12D3KooWSSfWY3fTGJvGkuNUNBSNVCdLLNJnwkZSNQt7GCRYXu4o",
     "/dns/polkadot-people-connect-2.polkadot.io/tcp/30334/p2p/12D3KooWGywSaoumTpcTwojvLBjZbrrwLgSRL9jFsCPJpQXjsXzp",
     "/dns/polkadot-people-connect-3.polkadot.io/tcp/30334/p2p/12D3KooWLegYvfSKxVdLXgcSYt24bcHtn1VyKGMiDrpfQ8M96w11",
     "/dns/polkadot-people-connect-0.polkadot.io/tcp/443/wss/p2p/12D3KooWP7BoJ7nAF9QnsreN8Eft1yHNUhvhxFiQyKFEUePi9mu3",
     "/dns/polkadot-people-connect-1.polkadot.io/tcp/443/wss/p2p/12D3KooWSSfWY3fTGJvGkuNUNBSNVCdLLNJnwkZSNQt7GCRYXu4o",
     "/dns/polkadot-people-connect-2.polkadot.io/tcp/443/wss/p2p/12D3KooWGywSaoumTpcTwojvLBjZbrrwLgSRL9jFsCPJpQXjsXzp",
     "/dns/polkadot-people-connect-3.polkadot.io/tcp/443/wss/p2p/12D3KooWLegYvfSKxVdLXgcSYt24bcHtn1VyKGMiDrpfQ8M96w11"
]' \
    | jq '.relay_chain = "polkadot"' \
    | jq --argjson para_id $para_id '.para_id = $para_id' \
    | jq --argjson para_id $para_id '.genesis.runtimeGenesis.config.parachainInfo.parachainId |= $para_id' \
    | jq --argjson para_id $para_id '.genesis.runtimeGenesis.config.parachainInfo.parachainId |= $para_id' \
    | jq '.genesis.runtimeGenesis.config.balances.balances = [
            [
              "5GrwvaEF5zXb26Fz9rcQpDWS57CtERHpNehXCPcNoHGKutQY",
              1152921504606846976
            ],
            [
              "5FHneW46xGXgs5mUiveU4sbTyGBzmstUspZC92UhjJM694ty",
              1152921504606846976
            ],
            [
              "5FLSigC9HGRKVhB9FiEo4Y3koPsNmBmLJbpXg2mp1hXcS59Y",
              1152921504606846976
            ],
            [
              "5DAAnrj7VHTznn2AWBemMuyBwZWs6FNFjdyVXUeYum3PTXFy",
              1152921504606846976
            ],
            [
              "5HGjWAeFDfFCWPsjFQdVV2Msvz2XtMktvgocEZcCj68kUMaw",
              1152921504606846976
            ],
            [
              "5CiPPseXPECbkjWCa6MnjNokrgYjMqmKndv2rSnekmSK2DjL",
              1152921504606846976
            ],
            [
              "5GNJqTPyNqANBkUVMN1LPPrxXnFouWXoe2wNSmmEoLctxiZY",
              1152921504606846976
            ],
            [
              "5HpG9w8EBLe5XCrbczpwq5TSXvedjrBGCwqxK1iQ7qUsSWFc",
              1152921504606846976
            ],
            [
              "5Ck5SLSHYac6WFt5UZRSsdJjwmpSZq85fd5TRNAdZQVzEAPT",
              1152921504606846976
            ],
            [
              "5HKPmK9GYtE1PSLsS1qiYU9xQ9Si1NcEhdeCq9sw5bqu4ns8",
              1152921504606846976
            ],
            [
              "5FCfAonRZgTFrTd9HREEyeJjDpT397KMzizE6T3DvebLFE7n",
              1152921504606846976
            ],
            [
              "5CRmqmsiNFExV6VbdmPJViVxrWmkaXXvBrSX8oqBT8R9vmWk",
              1152921504606846976
            ]
          ]' \
    | jq '.genesis.runtimeGenesis.config.collatorSelection.invulnerables = [
		"5Gnjmw1iuF2kV4PecFgetJed7B8quBKfLiRM99ELcXvFH9Vn",
		"5FLZRxyeRPhG69zo4ZPqCJSYboSKaRBUjBvQc1nkuWoBpZ5P",
		"5DNnmPH2MT6SXpfqbJZbTz4eERmuZegssfxc4ysL8PWrHaNN",
		"5DkKcSP5MboNMpXScW1CyRqaktKMXH8QLP4Mn49TwS5vhL6k"
	]' \
    | jq '.genesis.runtimeGenesis.config.session.keys = [
            [
                "5Gnjmw1iuF2kV4PecFgetJed7B8quBKfLiRM99ELcXvFH9Vn",
                "5Gnjmw1iuF2kV4PecFgetJed7B8quBKfLiRM99ELcXvFH9Vn",
                    {
                        "aura": "5Gnjmw1iuF2kV4PecFgetJed7B8quBKfLiRM99ELcXvFH9Vn"
                    }
            ],
            [
                "5FLZRxyeRPhG69zo4ZPqCJSYboSKaRBUjBvQc1nkuWoBpZ5P",
                "5FLZRxyeRPhG69zo4ZPqCJSYboSKaRBUjBvQc1nkuWoBpZ5P",
                    {
                        "aura": "5FLZRxyeRPhG69zo4ZPqCJSYboSKaRBUjBvQc1nkuWoBpZ5P"
                    }
            ],
            [
                "5DNnmPH2MT6SXpfqbJZbTz4eERmuZegssfxc4ysL8PWrHaNN",
                "5DNnmPH2MT6SXpfqbJZbTz4eERmuZegssfxc4ysL8PWrHaNN",
                    {
                        "aura": "5DNnmPH2MT6SXpfqbJZbTz4eERmuZegssfxc4ysL8PWrHaNN"
                    }
            ],
            [
                "5DkKcSP5MboNMpXScW1CyRqaktKMXH8QLP4Mn49TwS5vhL6k",
                "5DkKcSP5MboNMpXScW1CyRqaktKMXH8QLP4Mn49TwS5vhL6k",
                    {
                        "aura": "5DkKcSP5MboNMpXScW1CyRqaktKMXH8QLP4Mn49TwS5vhL6k"
                    }
            ]
        ]' \
    > "$script_dir/edited-chain-spec-plain.json"

# build a raw spec
$POLKADOT_PARACHAIN_BINARY build-spec --chain "$script_dir/edited-chain-spec-plain.json" --raw > "$script_dir/chain-spec-raw.json" 2>/dev/null
mv "$script_dir/edited-chain-spec-plain.json" "$script_dir/people-polkadot-spec.json"
mv "$script_dir/chain-spec-raw.json" "$script_dir/people-polkadot-spec-raw.json"

# build genesis data
$POLKADOT_PARACHAIN_BINARY export-genesis-state --chain "$script_dir/people-polkadot-spec-raw.json" > "$script_dir/people-polkadot-genesis-head-data" > /dev/null 2>&1

# build genesis wasm
$POLKADOT_PARACHAIN_BINARY export-genesis-wasm --chain "$script_dir/people-polkadot-spec-raw.json" > "$script_dir/people-polkadot-wasm"

# clean up useless files
rm "$script_dir/rt-hex.txt"
rm "$script_dir/chain-spec-plain.json"

cat "$script_dir/people-polkadot-spec.json"