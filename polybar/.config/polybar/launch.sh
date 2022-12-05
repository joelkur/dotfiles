#!/usr/bin/env bash

killall -q polybar


for monitor in $(xrandr --query | grep " connected" | cut -d " " -f1); do
  echo "---" | tee -a /tmp/polybar-$monitor.log
  MONITOR="$monitor" polybar topbar 2>&1 | tee -a /tmp/polybar-$monitor.log & disown
done

