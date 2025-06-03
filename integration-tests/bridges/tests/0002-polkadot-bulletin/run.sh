#!/bin/bash

# Test that checks if asset transfer works on P<>K bridge.
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

run_zndsl ${BASH_SOURCE%/*}/store-data-from-people.zndsl $polkadot_dir
