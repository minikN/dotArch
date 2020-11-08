# Installing Dependencies

``` shell
sudo apt update && sudo apt install -y wget git zsh zsh-syntax-highlighting unzip vim pass libgpg-error-dev libassuan-dev lbzip2 autoconf automake autotools-dev bsd-mailx build-essential diffstat gnutls-dev imagemagick libasound2-dev libc6-dev libdatrie-dev libdbus-1-dev libgconf2-dev libgif-dev libgnutls28-dev libgpm-dev libgtk2.0-dev libgtk-3-dev libice-dev libjpeg-dev liblockfile-dev liblqr-1-0 libm17n-dev libmagickwand-dev libncurses5-dev libncurses-dev libotf-dev libpng-dev librsvg2-dev libsm-dev libthai-dev libtiff5-dev libtiff-dev libtinfo-dev libtool  libx11-dev libxext-dev libxi-dev libxml2-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev quilt sharutils texinfo xaw3dg xaw3dg-dev xorg-dev xutils-dev zlib1g-dev libjansson-dev libxaw7-dev libselinux1-dev libmagick++-dev libacl1-dev
```
# Installing WSL utilities

``` shell
sudo apt install gnupg2 apt-transport-https
wget -O - https://access.patrickwu.space/wslu/public.asc | sudo apt-key add -
echo "deb https://access.patrickwu.space/wslu/debian buster main" | sudo tee -a /etc/apt/sources.list
sudo apt update
sudo apt install wslu
```

# Preparing pass

``` shell
# clone private pass repo to $PASSWORD_STORE_DIR
```

# Compiling Emacs 27

```shell
cd ~
git clone -b emacs-27 --depth 1 git://git.sv.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure
make
sudo make install
cd ~
rm-rf ~/emacs
```

# Compiling pinentry-emacs

``` shell
cd ~
wget https://gnupg.org/ftp/gcrypt/pinentry/pinentry-1.1.0.tar.bz2
tar -xf pinentry-1.1.0.tar.bz2
cd pinentry-1.1.0
./configure --enable-pinentry-emacs --enable-inside-emacs
make
sudo make install
cd ~
rm -rf pinentry-1.1.0 pinentry-1.1.0.tar.bz2
```

# Configuring zsh

``` shell
sudo mkdir -p /usr/share/zsh/plugins
sudo ln -sf /usr/share/zsh-syntax-highlighting /usr/share/zsh/plugins/zsh-syntax-highlighting
```

