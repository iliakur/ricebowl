#!/bin/sh
set -e

# - Ask acpi for the battery status
# - parse with awk, treating commas as separators
# - if the status contains "Discharging" look at the second field, which is the percentage left
# - if that's lower than the threshold (10 percent), send a notification
acpi -b | awk -F , '/Discharging/ {if (int($2) < 10) system("notify-send -u critical 'Plug me in please!'")}'
