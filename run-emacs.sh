#!/bin/bash


cmd=emacs
opts="--user $(id -u):$(id -g) --rm" # --group-add sudo
if [ "$1" = "debug" ]; then
    cmd=bash
    opts="-it ${opts}"
fi


if [ -z "$WSL_DISTRO_NAME" ]; then
    docker run $opts --privileged --network=host \
           -v ~:/home/"$USER" \
           -v /etc/ssl:/etc/ssl:ro \
           -v /etc/pki:/etc/pki:ro \
           -e JAVA_HOME \
           emacs \
           "$cmd"
    exit 0
fi


export WSL_PATH=/wsl
export WIN_PATH=/mnt/c
export JAVA_HOME="$(wslpath "$(cmd.exe /C echo %JAVA_HOME% | tr -d '\r')")"
export LS_IP=$(ipconfig.exe | grep 'vEthernet.*WSL' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')
homepath=/home/share
network_mnts="-v ${WIN_PATH}:${WIN_PATH}"
instances=($(cmd.exe /C "wsl --list -q" | iconv -f UTF-16LE -t UTF-8 | tr -d '\r'))
for inst in "${instances[@]}"; do
    sudo mkdir -p /mnt/"$inst"
    mountpoint -q /mnt/"$inst" || sudo mount -t drvfs '\\wsl.localhost\'$inst /mnt/"$inst"
    network_mnts="${network_mnts} -v /mnt/${inst}:/mnt/${inst}"
done
if [ $(stat --format '%U' "$XDG_RUNTIME_DIR") = root ]; then
    sudo chown -R "$(id -u):$(id -g)" "$XDG_RUNTIME_DIR"
fi

# Set the user/group to silence dconf-CRITICAL warnings
docker run $opts --privileged --network=host $network_mnts \
       -w "$homepath" \
       -v "/:${WSL_PATH}" \
       -v "${homepath}:${homepath}" \
       -v /mnt/wslg:/mnt/wslg \
       -v /mnt/wslg/.X11-unix:/tmp/.X11-unix:ro \
       -v /run/WSL:/run/WSL:ro \
       -v /init:/init:ro \
       -v /etc/ssl:/etc/ssl:ro \
       -v /etc/pki:/etc/pki:ro \
       -e DISPLAY \
       -e WAYLAND_DISPLAY \
       -e XDG_RUNTIME_DIR \
       -e PULSE_SERVER \
       -e WSL_DISTRO_NAME \
       -e WSL_INTEROP \
       -e WSLENV \
       -e WSL_PATH \
       -e WIN_PATH \
       -e JAVA_HOME \
       -e LS_IP \
       emacs \
       "$cmd"
