#!/bin/bash

# import common functions
source "$FRAMEWORK_PATH/utils/bridges.sh"

function ensure_js_api() {
    if ! which polkadot-js-api &> /dev/null; then
        echo ''
        echo 'Required command `polkadot-js-api` not in PATH, please, install, e.g.:'
        echo "npm install -g @polkadot/api-cli@beta"
        echo "      or"
        echo "yarn global add @polkadot/api-cli"
        echo ''
        exit 1
    fi
    if ! which jq &> /dev/null; then
        echo ''
        echo 'Required command `jq` not in PATH, please, install, e.g.:'
        echo "apt install -y jq"
        echo ''
        exit 1
    fi
    generate_hex_encoded_call_data "check" "--"
    local retVal=$?
    if [ $retVal -ne 0 ]; then
        echo ""
        echo ""
        echo "-------------------"
        echo "Installing (nodejs) sub module: ${BASH_SOURCE%/*}/generate_hex_encoded_call"
        pushd ${BASH_SOURCE%/*}/generate_hex_encoded_call
        npm install
        popd
    fi
}

function generate_hex_encoded_call_data() {
    local type=$1
    local endpoint=$2
    local output=$3
    shift
    shift
    shift
    echo "Input params: $@"

    node ${BASH_SOURCE%/*}/generate_hex_encoded_call "$type" "$endpoint" "$output" "$@" 2>&1
    local retVal=$?

    if [ $type != "check" ]; then
        local hex_encoded_data=$(cat $output)
        echo "Generated hex-encoded bytes to file '$output': $hex_encoded_data"
    fi

    return $retVal
}

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
    dest=$(jq --null-input '{ "V5": { "parents": 2, "interior": { "X1": [ { "GlobalConsensus": "PolkadotBulletin" } ] } } }')

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

function add_validator_to_bulletin() {
    local relay_url=$1
    local relay_chain_seed=$2
    local people_para_id=$3
    local people_chain_endpoint=$4
    local bulletin_chain_endpoint=$5
    local validator_id=$6
    
    echo "  calling add_validator_to_bulletin:"
    echo "      relay_url: ${relay_url}"
    echo "      relay_chain_seed: ${relay_chain_seed}"
    echo "      people_para_id: ${people_para_id}"
    echo "      people_chain_endpoint: ${people_chain_endpoint}"
    echo "      bulletin_chain_endpoint: ${bulletin_chain_endpoint}"
    echo "      validator_id: ${validator_id}"
    echo ""
    echo "--------------------------------------------------"

    # Create temporary files for hex encoded data
    local tmp_bulletin_call_file=$(mktemp)
    local tmp_people_call_file=$(mktemp)

    # Step 1: Generate hex encoded call for ValidatorSet::add on Bulletin chain
    generate_hex_encoded_call_data "add-bulletin-validator" "${bulletin_chain_endpoint}" "${tmp_bulletin_call_file}" "$validator_id"
    local bulletin_call_hex=$(cat $tmp_bulletin_call_file)
    echo "Generated Bulletin ValidatorSet::add call: $bulletin_call_hex"

    # Step 2: Generate hex encoded call for People chain to send XCM to Bulletin
    generate_hex_encoded_call_data "people-xcm-send-to-bulletin" "${people_chain_endpoint}" "${tmp_people_call_file}" "$bulletin_call_hex"
    local people_call_hex=$(cat $tmp_people_call_file)
    echo "Generated People XCM send call: $people_call_hex"

    # Step 3: Send governance transact from relay chain to People chain
    # This will trigger People chain to send the XCM message to Bulletin chain
    send_governance_transact "${relay_url}" "${relay_chain_seed}" "${people_para_id}" "${people_call_hex}" 200000000 12000

    # Clean up temporary files
    rm -f "$tmp_bulletin_call_file" "$tmp_people_call_file"
}

function authorize_account_on_bulletin() {
    local relay_url=$1
    local relay_chain_seed=$2
    local people_para_id=$3
    local people_chain_endpoint=$4
    local bulletin_chain_endpoint=$5
    local account_to_authorize=$6
    local transactions=$7
    local bytes=$8

    echo "  calling authorize_account_on_bulletin:"
    echo "      relay_url: ${relay_url}"
    echo "      relay_chain_seed: ${relay_chain_seed}"
    echo "      people_para_id: ${people_para_id}"
    echo "      people_chain_endpoint: ${people_chain_endpoint}"
    echo "      bulletin_chain_endpoint: ${bulletin_chain_endpoint}"
    echo "      account_to_authorize: ${account_to_authorize}"
    echo "      transactions: ${transactions}"
    echo "      bytes: ${bytes}"
    echo "--------------------------------------------------"

    local tmp_bulletin_call_file=$(mktemp)
    local tmp_people_call_file=$(mktemp)

    generate_hex_encoded_call_data "bulletin-transaction-storage-authorize-account" "${bulletin_chain_endpoint}" "${tmp_bulletin_call_file}" "$account_to_authorize" "$transactions" "$bytes"
    local bulletin_call_hex=$(cat $tmp_bulletin_call_file)
    echo "Generated Bulletin transactionStorage.authorizeAccount call: $bulletin_call_hex"

    generate_hex_encoded_call_data "people-xcm-send-to-bulletin" "${people_chain_endpoint}" "${tmp_people_call_file}" "$bulletin_call_hex"
    local people_call_hex=$(cat $tmp_people_call_file)
    echo "Generated People XCM send call: $people_call_hex"

    send_governance_transact "${relay_url}" "${relay_chain_seed}" "${people_para_id}" "${people_call_hex}" 200000000 12000

    # Clean up temporary files
    rm -f "$tmp_bulletin_call_file" "$tmp_people_call_file"
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
  add-validator-to-bulletin)
    ensure_js_api
    relay_url=$2
    relay_chain_seed=$3
    people_para_id=$4
    people_chain_endpoint=$5
    bulletin_chain_endpoint=$6
    validator_id=$7
    add_validator_to_bulletin "$relay_url" "$relay_chain_seed" "$people_para_id" "$people_chain_endpoint" "$bulletin_chain_endpoint" "$validator_id"
    ;;
  authorize-account-on-bulletin)
    ensure_js_api
    relay_url=$2
    relay_chain_seed=$3
    people_para_id=$4
    people_chain_endpoint=$5
    bulletin_chain_endpoint=$6
    account_to_authorize=$7
    transactions=$8
    bytes=$9
    authorize_account_on_bulletin "$relay_url" "$relay_chain_seed" "$people_para_id" "$people_chain_endpoint" "$bulletin_chain_endpoint" "$account_to_authorize" "$transactions" "$bytes"
    ;;
  *)
    echo "A command is require. Supported commands for:
    Local (zombienet) run:
          - run-finality-relay
          - run-parachains-relay
          - run-messages-relay
          - init-people-polkadot-local
          - init-bulletin-local
          - add-validator-to-bulletin
          - authorize-account-on-bulletin
          - stop";
    exit 1
    ;;
esac
