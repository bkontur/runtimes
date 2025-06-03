#!/bin/bash

set -e

trap "trap - SIGTERM && kill -9 -$$" SIGINT SIGTERM EXIT

source "$FRAMEWORK_PATH/utils/zombienet.sh"

# whether to init the chains (open HRMP channels, set XCM version, create reserve assets, etc)
init=0
start_relayer=0
while [ $# -ne 0 ]
do
    arg="$1"
    case "$arg" in
        --init)
            init=1
            ;;
        --start-relayer)
            start_relayer=1
            ;;
    esac
    shift
done

logs_dir=$TEST_DIR/logs
helper_script="${BASH_SOURCE%/*}/helper.sh"

polkadot_def=${BASH_SOURCE%/*}/people_hub_polkadot_local_network.toml
start_zombienet $TEST_DIR $polkadot_def polkadot_dir polkadot_pid
echo

bulletin_def=${BASH_SOURCE%/*}/polkadot_bulletin_local_network.toml
start_zombienet $TEST_DIR $bulletin_def bulletin_dir bulletin_pid
echo

if [[ $init -eq 1 ]]; then
  polkadot_init_log=$logs_dir/polkadot-init.log
  echo -e "Setting up the polkadot side of the bridge. Logs available at: $polkadot_init_log\n"
  bulletin_init_log=$logs_dir/bulletin-init.log
  echo -e "Setting up the bulletin side of the bridge. Logs available at: $bulletin_init_log\n"
  $helper_script init-people-polkadot-local >> $polkadot_init_log 2>&1 &
  polkadot_init_pid=$!
  $helper_script init-bulletin-local >> $kusama_init_log 2>&1 &
  bulletin_init_pid=$!
  wait -n $polkadot_init_pid $kusama_init_pid

  run_zndsl ${BASH_SOURCE%/*}/polkadot-init.zndsl $polkadot_dir
  run_zndsl ${BASH_SOURCE%/*}/kusama-init.zndsl $kusama_dir
fi

if [[ $start_relayer -eq 1 ]]; then
  ${BASH_SOURCE%/*}/start_relayer.sh $polkadot_dir $kusama_dir finality_relayer_pid parachains_relayer_pid messages_relayer_pid
fi

echo $polkadot_dir > $TEST_DIR/polkadot.env
echo $bulletin_dir > $TEST_DIR/bulletin.env
echo

wait -n $polkadot_pid $bulletin_pid $finality_relayer_pid $parachains_relayer_pid $messages_relayer_pid
kill -9 -$$
