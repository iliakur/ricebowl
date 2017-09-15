#!/bin/sh
# This file contains various steps that I found I had to do when setting up a fedora box
# from scratch. Notably it doesn't include Dropbox setup because that's separate.

dnf distro-sync --refresh

dnf install -y \
	util-linux-user \
	network-manager-applet \
    gnome-pomodoro \
	py3status \
	okular \
    dnf-automatic


# Generating the right ssh keys:

ssh-keygen -t rsa -N "" -f ~/.ssh/id_poseidon
ssh-keygen -t rsa -N "" -f ~/.ssh/id_medusa
# Github suggests these settings. I'm sure the comment is irrelevant, but the key size might be important
ssh-keygen -t rsa -b 4096 -C "ilia.kurenkov@gmail.com" -N "" -f ~/.ssh/id_git

# dnf automatic config
rm /etc/dnf/automatic.conf && ln -s $PWD/automatic.conf /etc/dnf/