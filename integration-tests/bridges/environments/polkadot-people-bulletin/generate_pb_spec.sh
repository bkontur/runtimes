#!/bin/bash
set -e

bridged_chain='PolkadotBulletin'
shift

# Add Alice as bridge owner
# We do this only if there is a `.genesis.runtimeGenesis.patch` object.
# Otherwise we're working with the raw chain spec.
$POLKADOT_BULLETIN_BINARY_PATH -- build-spec --chain=local > sc_init.json
$POLKADOT_BULLETIN_BINARY_PATH -- build-spec --chain=sc_init.json | jq 'if .genesis.runtimeGenesis.patch
    then .genesis.runtimeGenesis.patch.bridge'$bridged_chain'Grandpa.owner = "5GrwvaEF5zXb26Fz9rcQpDWS57CtERHpNehXCPcNoHGKutQY"
    else .
    end'
