#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

USER_NAME="switcher_01"
IP=10.10.60.98

readonly TARGET_HOST=$USER_NAME@$IP
readonly TARGET_PATH=/home/$USER_NAME/switcher
readonly TARGET_ARCH=aarch64-unknown-linux-gnu
readonly SOURCE_PATH=./target/${TARGET_ARCH}/release/switcher

cross build --release --target=${TARGET_ARCH}
rsync -Pauvht --stats ${SOURCE_PATH} ${TARGET_HOST}:${TARGET_PATH}
ssh -t ${TARGET_HOST} sudo systemctl restart antenna_switcher_api.service
