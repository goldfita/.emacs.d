#!/bin/bash


export HOST_PATH=/host
cmd=emacs
opts="--user $(id -u):$(id -g) --rm" # --group-add sudo
if [ "$1" = "debug" ]; then
    cmd=bash
    opts="-it ${opts}"
fi

#*** Start docker service.
# When WSL has not yet started, the docker daemon will often fail to load because of a problem
# setting up iptables. Immediately checking service status or iptables fails because the error
# message takes some time to appear, and basically service initially lies and says dockerd is
# running. Instead, skip iptables since we're running in the host network namespace anyway.
if [ ! -f /var/run/docker.pid ]; then
    sudo dockerd --iptables=false > /dev/null 2>&1 &
    sleep 1
fi


#*** Run plain old linux
if [ -z "$WSL_DISTRO_NAME" ]; then
    export LS_IP="$(ip route get 8.8.8.8 | head -1 | grep -oP "src\s\K.*?\s")"
    docker run $opts --privileged --network=host \
           -v ~:"/home/${USER}" \
           -v "/:${HOST_PATH}" \
           -v /etc/ssl:/etc/ssl:ro \
           -v /etc/pki:/etc/pki:ro \
           -e USER \
           -e HOST_PATH \
           -e JAVA_HOME \
           -e LS_IP \
           emacs "$cmd"
    exit 0
fi


#*** WSL linux
export WIN_PATH=/mnt/c
export WIN_DEPS_PATH="$WIN_PATH"/software
export XDG_CACHE_HOME="/home/${USER}/.cache"  # so tramp doesn't create a socket file on windows
export RUSTUP_HOME="$WIN_DEPS_PATH"/rustup
export CARGO_HOME="$WIN_DEPS_PATH"/cargo
win_paths="$(echo "$PATH" | tr ':' '\n' | grep "$WIN_PATH" | tr '\n' ':')"
guest_home_path=/home/share
host_dev_path="${WIN_PATH}/Development"

# mount other WSL instances in /mnt/c; this will start the instance if not already started
network_mnts="-v ${WIN_PATH}:${WIN_PATH}"
instances=($(cmd.exe /C "wsl --list -q" | iconv -f UTF-16LE -t UTF-8 | tr -d '\r'))
for inst in "${instances[@]}"; do
    sudo mkdir -p /mnt/"$inst"
    mountpoint -q /mnt/"$inst" || sudo mount -t drvfs '\\wsl.localhost\'"$inst" /mnt/"$inst"
    network_mnts="${network_mnts} -v /mnt/${inst}:/mnt/${inst}"
done

# set permissions so we don't get errors
if [ $(stat --format '%U' "$XDG_RUNTIME_DIR") = root ]; then
    sudo chown -R "$(id -u):$(id -g)" "$XDG_RUNTIME_DIR"
fi

# when running nohup from shortcut, win/wsl commands don't always work immediately
#until [ -n "$LS_IP" ]; do
#    sleep .1
    export LS_IP=$(ipconfig.exe | grep 'vEthernet.*WSL' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')
    export JAVA_HOME="$(wslpath "$(cmd.exe /C echo %JAVA_HOME% | tr -d '\r')")"
#done

#*** Run (set the user/group to silence dconf-CRITICAL warnings)
cid_file=/tmp/emacs-cid-$$
docker run $opts --privileged --network=host $network_mnts --cidfile $cid_file \
       -w "$guest_home_path" \
       -v "/:${HOST_PATH}" \
       -v "${host_dev_path}:${guest_home_path}" \
       -v "${cid_file}:/cid" \
       -v /mnt/wslg:/mnt/wslg \
       -v /mnt/wslg/.X11-unix:/tmp/.X11-unix:ro \
       -v /run/WSL:/run/WSL:ro \
       -v /init:/init:ro \
       -v /etc/ssl:/etc/ssl:ro \
       -v /etc/pki:/etc/pki:ro \
       -e DISPLAY \
       -e WAYLAND_DISPLAY \
       -e XDG_RUNTIME_DIR \
       -e XDG_CACHE_HOME \
       -e PULSE_SERVER \
       -e WSL_DISTRO_NAME \
       -e WSL_INTEROP \
       -e WSLENV \
       -e USER \
       -e HOST_PATH \
       -e WIN_PATH \
       -e WIN_DEPS_PATH \
       -e JAVA_HOME \
       -e RUSTUP_HOME \
       -e CARGO_HOME \
       -e LS_IP \
       emacs bash -c "ln -s \"${guest_home_path}\"/.emacs.d/wsl/* \"/home/${USER}\";
                      sudo ln -s /init /usr/bin/wslpath;
                      PATH=\"\${PATH}:${win_paths}\" \"$cmd\""
rm $cid_file
