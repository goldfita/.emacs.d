Set the HOME environment variable to the path where you want the .emacs.d directory to go. Do this from a
script so you don't change HOME for other applications. This file cannot be compiled until Emacs has run at
least once and installed use-package. Get the icon:

https://github.com/emacs-mirror/emacs/blob/master/nt/icons/emacs.ico

Run M-x package-refresh-contents to update package list. You need to do this if you get a melpa error about
the package not being found. To see installed packages, M-x list-packages or look under .emacs.d/elpa. Type
'd' over each installed package and then shift-u x to delete or just shift-u x to update. You can also
delete packages by wiping out everything in the .emacs.d/elpa directory. Delete use-package to before
starting to re-install deleted packages. Use '(setq package-check-signature nil)' if you get errors about
signatures.

Packages are prioritized by melpa. Older, "stable" versions of packages seem to be more buggy and missing
critical features. When updating packages, usually you have to wipe out the entire elpa directory and
delete any state files like workspace or session files. If you attempt to update just one package that
integrates with anything else, the results are usually ugly.

***Packages***

Projectile:
On windows, create a shortcut from .projectile to .gitignore to make projectile use .gitignore.

Lsp-java:
Install Eclipse JDT LS and set the path. Check lsp-java-jdt-download-url for version. Run lsp-describe-session
for capabilities. Install the language server version of Java and JDT LS and create workspace directory:
https://download.eclipse.org/jdtls/milestones/
C:\software\ls-jdk
C:\software\eclipse.jdt.ls
C:\software\ls-workspace

For DAP, get the jar from maven central, and put it in eclipse.jdt.ls/bundles:
https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin
C:\software\eclipse.jdt.ls\bundles\com.microsoft.java.debug.plugin-x.y.z.jar

Tramp:
To use plink on Windows, first generate public/private keys on Linux. Add the public key to authorized keys
on remote.
   ssh-keygen -t rsa -f file-name
   cat file-name.pub >> .ssh/authorized_keys
Copy private key (pem format) to Windows. Load in PuttyGen, and save as ppk format. In Pageant, add the
private key. Now you should be able to use plink to ssh.
   plink -no-antispoof user@server
Set tg/remote-user and tg/remote-host in early-init.el. Then enter '/-::' in find file to find a remote file
in user's home directory, or use '/-::/' to go to the root directory. To open a file as root, you only need to
type '/root::'. If emacs hangs, delete tramp customizations and the tramp file before retrying.

***External Dependencies***

Windows:

For all the icons to work properly, go into .fonts directory, right click on each file, and select install.
This may require a reboot. Add Google Noto Sans Symbols 2 font for Centaur Tabs:
    https://fonts.google.com/noto/specimen/Noto+Sans+Symbols+2

Install Hack Fonts on Windows
    https://github.com/source-foundry/Hack

WSL/Windows:
    Git:     Git-xxx.exe /DIR=C:\software\git
    Ripgrep: https://github.com/BurntSushi/ripgrep
    Python:  python-xxx.exe (install to C:\software\python)
    Java:    OpenJDK-xxx.msi (install to C:\software\jdk)
    Node:    Install Windows NVM to c:\nvm4w and make sure NVM_SYMLINK=c:\nvm4w\nodejs

***WSL***

To run from WSL, create a shortcut with the folliwng target.

C:\Windows\System32\wsl.exe -d ubuntu -u <username> -e bash -ic "nohup ~/run-emacs.sh > /dev/null 2>&1 &"

Set up the default WSL image and install docker:

usermod -d /mnt/c/Development/.emacs.d <username>  # as root
echo "$USER ALL=(ALL) NOPASSWD:ALL" | sudo tee /etc/sudoers.d/$USER
# Install docker (https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository)
sudo apt-get install docker.io
sudo usermod -aG docker $USER  # log out and back in

Update the hosts file with development machine host/IP(s). DAP uses the 'hostName' parameter set in
.dir-locals.el. Alternatively, use '--add-host' parameter in run-emacs.sh.
