#!/bin/sh
# Download and install the gamepad mapping database.

SCRIPT_PATH="$(cd "$(dirname "$0")" ; pwd -P)"
REMOTE_FILE="https://raw.githubusercontent.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt"
LOCAL_FILE="${SCRIPT_PATH}/../data/gamepad-db.txt"

wget -q ${REMOTE_FILE} -O ${LOCAL_FILE}
