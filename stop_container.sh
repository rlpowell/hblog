#!/bin/bash

exec 2>&1
set -x

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

$CONTAINER_BIN stop --time=30 hblog
$CONTAINER_BIN kill hblog
$CONTAINER_BIN rm hblog
