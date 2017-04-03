#!/bin/sh
# Since i3wm cannot exec aliases/functions defined for zsh, we have to expose
# the workrave control functionality in a good old shell script.
# It currently only accepts one argument with 3 possible values corresponding
# to the if branches below.
# This file is symlinked to ~/bin/workravectl to make it visible to i3.

if [[ "$1" = "toggle-op-mode" ]]; then

    current_mode=`dbus-send --dest=org.workrave.Workrave \
                --print-reply=literal --type=method_call \
                /org/workrave/Workrave/Core \
                org.workrave.CoreInterface.GetOperationMode | tr -d ' '`
    if [[ $current_mode == "suspended" ]]; then
        dbus-send --dest=org.workrave.Workrave \
                    --type=method_call \
                    /org/workrave/Workrave/Core \
                    org.workrave.CoreInterface.SetOperationMode \
                    string:"normal"
    # This is currently very primitive: if mode is anything other than
    # "suspended", it will suspend workrave.
    else
        dbus-send --dest=org.workrave.Workrave \
                    --type=method_call \
                    /org/workrave/Workrave/Core \
                    org.workrave.CoreInterface.SetOperationMode \
                    string:"suspended"

    fi
elif [[ "$1" = "enable-reading-mode" ]]; then
    dbus-send --dest=org.workrave.Workrave \
                --type=method_call \
                /org/workrave/Workrave/UI \
                org.workrave.ControlInterface.ReadingMode \
                boolean:true
elif [[ "$1" = "disable-reading-mode" ]]; then
    dbus-send --dest=org.workrave.Workrave \
                --type=method_call \
                /org/workrave/Workrave/UI \
                org.workrave.ControlInterface.ReadingMode \
                boolean:false
fi