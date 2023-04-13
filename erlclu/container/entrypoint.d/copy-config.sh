#!/bin/sh

set -eu

mkdir -p "$CONFIG_TARGET"
cp -r "$CONFIG_SOURCE" "$CONFIG_TARGET"
