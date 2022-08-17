#!/usr/bin/env bash

set -xeuo pipefail

stylus=$(xsetwacom list devices | grep stylus | sed 's/.*id: \([0-9]*\).*/\1/g')
xsetwacom set $stylus Rotate ccw
xsetwacom set $stylus MapToOutput 850x1080+20+0
xsetwacom set $stylus Area 0 0 25920 20400
