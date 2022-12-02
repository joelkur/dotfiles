#!/usr/bin/env bash

killall -q polybar

echo "---" | tee -a /tmp/topbar.log
polybar topbar 2>&1 | tee -a /tmp/topbar.log & disown

echo "Bar launched..."
