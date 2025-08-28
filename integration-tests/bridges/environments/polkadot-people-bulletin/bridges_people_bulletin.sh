#!/bin/bash

# import common functions
source "$FRAMEWORK_PATH/utils/bridges.sh"

function init_bulletin_polkadot() {
    local RELAYER_BINARY_PATH=$(ensure_relayer)

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH init-bridge polkadot-bulletin-to-people-polkadot \
        --source-uri ws://localhost:10000 \
        --source-version-mode Auto \
        --target-uri ws://localhost:9910 \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4
}

function init_polkadot_bulletin() {
    local RELAYER_BINARY_PATH=$(ensure_relayer)

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH init-bridge polkadot-to-polkadot-bulletin \
        --source-uri ws://localhost:9942 \
        --source-version-mode Auto \
        --target-uri ws://localhost:10000 \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4
}

function run_finality_relay() {
  local RELAYER_BINARY_PATH=$(ensure_relayer)

  RUST_LOG=rpc=trace,bridge=trace \
      $RELAYER_BINARY_PATH relay-headers polkadot-bulletin-to-people-polkadot \
      --only-free-headers \
      --source-uri ws://localhost:10000 \
      --source-version-mode Auto \
      --target-uri ws://localhost:9910 \
      --target-version-mode Auto \
      --target-signer //Alice \
      --target-transactions-mortality 4 &

  RUST_LOG=rpc=trace,bridge=trace \
      $RELAYER_BINARY_PATH relay-headers polkadot-to-polkadot-bulletin \
      --only-free-headers \
      --source-uri ws://localhost:9942 \
      --source-version-mode Auto \
      --target-uri ws://localhost:10000 \
      --target-version-mode Auto \
      --target-signer //Alice \
      --target-transactions-mortality 4
}

function run_parachains_relay() {
    local RELAYER_BINARY_PATH=$(ensure_relayer)

    RUST_LOG=rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-parachains polkadot-to-polkadot-bulletin \
        --only-free-headers \
        --source-uri ws://localhost:9942 \
        --source-version-mode Auto \
        --target-uri ws://localhost:10000 \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4
}

function run_messages_relay() {
    local RELAYER_BINARY_PATH=$(ensure_relayer)

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-messages polkadot-bulletin-to-people-polkadot \
        --source-uri ws://localhost:10000 \
        --source-version-mode Auto \
        --source-signer //Alice \
        --source-transactions-mortality 4 \
        --target-uri ws://localhost:9910 \
        --target-version-mode Auto \
        --target-signer //Eve \
        --target-transactions-mortality 4 \
        --lane 00000000 &

    RUST_LOG=rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-messages people-polkadot-to-polkadot-bulletin \
        --source-uri ws://localhost:9910 \
        --source-version-mode Auto \
        --source-signer //Ferdie \
        --source-transactions-mortality 4 \
        --target-uri ws://localhost:10000 \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4 \
        --lane 00000000
}

function send_data() {
    local url=$1
    local seed=$2
    local data=$3

    echo "  calling send_data:"
    echo "      url: ${url}"
    echo "      seed: ${seed}"
    echo "      data: ${data}"
    echo ""
    echo "--------------------------------------------------"

    # Destination: Location::new(2, [GlobalConsensus(Bulletin)])
    local dest
    dest=$(jq --null-input '{ "V4": { "parents": 2, "interior": { "X1": [ { "GlobalConsensus": "PolkadotBulletin" } ] } } }')

    # XCM program: send a simple Trap to verify routing over the bridge.
    local xcm_msg
    xcm_msg=$(jq --null-input '{ "V5": [ { "Trap": 0 } ] }')

    call_polkadot_js_api \
        --ws "${url?}" \
        --seed "${seed?}" \
        tx.polkadotXcm.send \
            "$dest" \
            "$xcm_msg"
}

function store_data_with_bulletin() {
    local url=$1
    local seed=$2
    local data=$3
    echo "  calling store_data_with_bulletin:"
    echo "      url: ${url}"
    echo "      seed: ${seed}"
    echo "      data: ${data}"
    echo ""
    echo "--------------------------------------------------"

    call_polkadot_js_api \
        --ws "${url?}" \
        --seed "${seed?}" \
        tx.transactionStorage.store \
            "${data}"
}

case "$1" in
  run-finality-relay)
    init_bulletin_polkadot
    init_polkadot_bulletin
    run_finality_relay
    ;;
  run-parachains-relay)
    run_parachains_relay
    ;;
  run-messages-relay)
    run_messages_relay
    ;;
  init-people-polkadot-local)
      ensure_polkadot_js_api
      ;;
  init-bulletin-local)
      ensure_polkadot_js_api
      ;;
  stop)
    pkill -f polkadot
    pkill -f polkadot-parachain
    pkill -f substrate-relay
    ;;
  send-data)
     url=$2
     seed=$3
     data=$4
     send_data "$url" "$seed" "$data"
     ;;
  store-data)
    # TODO: replace with something useful
    # store data on bulletin
    url=$2
    seed=$3
    data=$4
    store_data_with_bulletin "$url" "$seed" "$data"
    ;;
  *)
    echo "A command is require. Supported commands for:
    Local (zombienet) run:
          - run-finality-relay
          - run-parachains-relay
          - run-messages-relay
          - init-people-polkadot-local
          - init-bulletin-local
          - stop";
    exit 1
    ;;
esac
