#!/bin/bash

total=`cat /proc/acpi/battery/BAT0/info | grep "design capacity:"`
total=`echo $total | cut -d' ' -f3`
remain=`cat /proc/acpi/battery/BAT0/state | grep "remaining capacity:"`
remain=`echo $remain | cut -d' ' -f3`
percent=`echo "scale=3; $remain / $total * 100" | bc | cut -d'.' -f1`
echo "$percent%"
