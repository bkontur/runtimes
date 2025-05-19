#!/bin/bash

# import common functions
source "$(dirname "$0")"/bridges_common.sh

function init_bulletin_polkadot() {
    ensure_relayer

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH init-bridge polkadot-bulletin-to-people-hub-polkadot \
        --source-uri ws://localhost:10000 \
        --source-version-mode Auto \
        --target-uri ws://localhost:8943 \
        --target-version-mode Auto \
        --target-signer //Bob \
        --target-transactions-mortality 4
}

function init_polkadot_bulletin() {
    ensure_relayer

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH init-bridge polkadot-to-polkadot-bulletin \
        --source-uri ws://localhost:9942 \
        --source-version-mode Auto \
        --target-uri localhost \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4
}

function run_finality_relay() {
  ensure_relayer

    RUST_LOG=rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-headers polkadot-bulletin-to-people-hub-polkadot \
        --only-free-headers \
        --source-uri ws://localhost:10000 \
        --source-version-mode Auto \
        --target-uri ws://localhost:8943 \
        --target-version-mode Auto \
        --target-signer //Bob \
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
    ensure_relayer

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
    ensure_relayer

    RUST_LOG=runtime=trace,rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-messages polkadot-bulletin-to-people-hub-polkadot \
        --source-uri ws://localhost:10000 \
        --source-version-mode Auto \
        --source-signer //Alice \
        --source-transactions-mortality 4 \
        --target-uri ws://localhost:8943 \
        --target-version-mode Auto \
        --target-signer //Eve \
        --target-transactions-mortality 4 \
        --lane 00000000 &

    RUST_LOG=rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-messages people-hub-polkadot-to-polkadot-bulletin \
        --source-uri ws://localhost:8943 \
        --source-version-mode Auto \
        --source-signer //Ferdie \
        --source-transactions-mortality 4 \
        --target-uri ws://localhost:10000 \
        --target-version-mode Auto \
        --target-signer //Alice \
        --target-transactions-mortality 4 \
        --lane 00000000
}

function run_relay() {
    echo OK
    ensure_relayer

    RUST_LOG=rpc=trace,bridge=trace \
        $RELAYER_BINARY_PATH relay-headers-and-messages polkadot-bulletin-people-hub-polkadot \
        --polkadot-bulletin-host localhost \
        --polkadot-bulletin-port 10000 \
        --polkadot-bulletin-version-mode Auto \
        --polkadot-bulletin-signer //Alice \
        --polkadot-bulletin-transactions-mortality 4 \
        --polkadot-host localhost \
        --polkadot-port 9942 \
        --polkadot-version-mode Auto \
        --people-hub-polkadot-host localhost \
        --people-hub-polkadot-port 8943 \
        --people-hub-polkadot-version-mode Auto \
        --people-hub-polkadot-signer //Charlie \
        --people-hub-polkadot-transactions-mortality 4 \
        --lane 00000000
}

case "$1" in
  init-bridge)
    init_bulletin_polkadot
    init_polkadot_bulletin
    ;;
  run-finality-relay)
    run_finality_relay
    ;;
  run-parachains-relay)
    run_parachains_relay
    ;;
  run-messages-relay)
    run_messages_relay
    ;;
  run-relay)
    init_bulletin_polkadot
    init_polkadot_bulletin
    run_relay
    ;;
  init-people-polkadot-local)
      ensure_polkadot_js_api
      ;;
  stop)
    pkill -f polkadot
    pkill -f polkadot-parachain
    pkill -f substrate-relay
    ;;
  *)
    echo "A command is require. Supported commands for:
    Local (zombienet) run:
          - run-relay";
    exit 1
    ;;
esac
