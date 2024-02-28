#!/usr/bin/bash

if [[ "$(pwd)" =~ /home/share|/mnt/c ]]; then
    /mnt/c/software/ripgrep/rg.exe "$@"
else
    /usr/bin/rg "$@"
fi
