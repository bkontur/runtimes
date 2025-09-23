#!/bin/bash

# Test suite for Polkadot-People-Bulletin bridge governance XCM calls.
# This test is intentionally not added to the CI. It is meant to be ran manually.

set -e

source "$FRAMEWORK_PATH/utils/common.sh"
source "$FRAMEWORK_PATH/utils/zombienet.sh"

export ENV_PATH=`realpath ${BASH_SOURCE%/*}/../../environments/polkadot-people-bulletin`

$ENV_PATH/spawn.sh --init --start-relayer &
env_pid=$!

ensure_process_file $env_pid $TEST_DIR/polkadot.env 600
polkadot_dir=`cat $TEST_DIR/polkadot.env`
echo

ensure_process_file $env_pid $TEST_DIR/bulletin.env 300
bulletin_dir=`cat $TEST_DIR/bulletin.env`
echo

echo "--- Test 1: Add validator to bulletin via governance XCM ---"
run_zndsl ${BASH_SOURCE%/*}/add-validator-to-bulletin.zndsl $bulletin_dir

echo "--- Test 2: Authorize account on bulletin via governance XCM ---"
run_zndsl ${BASH_SOURCE%/*}/authorize-account-on-bulletin.zndsl $bulletin_dir

echo "--- Test 3: Store data to bulletin with authorized account ---"
run_zndsl ${BASH_SOURCE%/*}/store-data-to-bulletin.zndsl $bulletin_dir

echo "All tests completed successfully!"
