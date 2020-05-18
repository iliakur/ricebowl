#!/usr/bin/env python3
# -> ~/bin/extmonitor
import re
from functools import partial
from pathlib import Path
from subprocess import run
from pprint import pprint

import click
import pyudev

mentions_keyboard = partial(re.search, "keyboard", flags=re.IGNORECASE)
homebin = Path().home() / "bin"


@click.command()
@click.option("--debug", default=False, is_flag=True)
def main(debug):

    monitor = pyudev.Monitor.from_netlink(pyudev.Context())
    monitor.filter_by("usb")
    # This subscribes to monitor events.
    monitor.filter_by("drm")

    # Monitoring synchronously is good enough for this script.
    for device in iter(monitor.poll, None):
        if debug:
            pprint(dict(device))
        # The fields we're checking are likely to contain the word "keyboard".
        # This is an attempt to future-proof the setup so that it works with *any*
        # USB keyboard that gets plugged in, not just the model that I have currently.
        elif mentions_keyboard(device.get("ID_MODEL", "")) or mentions_keyboard(
            device.get("ID_SERIAL", "")
        ):
            run(homebin / "ikkbremaps")

        # There is not a lot to work with when it comes to monitor events.
        # The link below suggests checking the HOTPLUG attribute.
        # https://stackoverflow.com/a/5711868/4501212
        elif device.get("HOTPLUG") == "1":
            currently_connected = len(
                re.findall(
                    "\sconnected", run("xrandr", capture_output=True).stdout.decode()
                )
            )
            # For now only react to 2 plugged in monitors.
            if currently_connected == 2:
                run(homebin / "selectdisplay")


if __name__ == "__main__":
    main()
