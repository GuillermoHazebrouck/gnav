#!/bin/bash
#set -x

# prepare libs
apt install libjpeg62-turbo-dev libconfig9 rpi-update dnsmasq git cmake libusb-1.0-0-dev build-essential \
  autoconf libtool i2c-tools libfftw3-dev libncurses-dev python3-serial jq ifplugd iptables -y

# cleanup
apt autoremove -y
apt clean

# install default or latest golang
#----------------------------------------------------------
rm -rf /root/go
rm -rf /root/go_path
if [ -f "go1.17.3.linux-arm64.tar.gz" ]; then
    cp go1.17.3.linux-arm64.tar.gz /root/
    cd /root
else
    cd /root
    wget https://golang.org/dl/go1.17.3.linux-arm64.tar.gz
fi
tar xzf *.gz
rm *.gz

# install default or latest librtlsdr
#----------------------------------------------------------
rm -rf /root/rtl-sdr
if [ -d "rtl-sdr" ]; then
    cp -r rtl-sdr /root/
else
    cd /root
    git clone https://github.com/osmocom/rtl-sdr.git
fi
cd /root/rtl-sdr
mkdir build
cd build
cmake ../ -DDETACH_KERNEL_DRIVER=ON -DINSTALL_UDEV_RULES=ON
make
sudo make install
sudo ldconfig
rm -rf /root/rtl-sdr

# install default or latest kalibrate-rtl
#----------------------------------------------------------
rm -rf /root/kalibrate-rtl
if [ -d "kalibrate-rtl" ]; then
    cp -r kalibrate-rtl /root/
else
    cd /root
    git clone https://github.com/steve-m/kalibrate-rtl
fi    
cd /root/kalibrate-rtl
./bootstrap && CXXFLAGS='-W -Wall -O3'
./configure
make -j8 && make install
rm -rf /root/kalibrate-rtl

# install default or latest stratux
#----------------------------------------------------------
rm -r /root/stratux
if [ -d "stratux" ]; then
    cp -r stratux /root/
else
    cd /root
    git clone --recursive https://github.com/b3nn0/stratux.git /root/stratux
fi   

# copy various files from /root/stratux/image
#----------------------------------------------------------
cd /root/stratux/image
cp -f config.txt /boot/config.txt
cp -f bashrc.txt /root/.bashrc
cp -f rc.local /etc/rc.local
cp -f modules.txt /etc/modules
cp -f motd /etc/motd
cp -f logrotate.conf /etc/logrotate.conf
cp -f logrotate_d_stratux /etc/logrotate.d/stratux
cp -f rtl-sdr-blacklist.conf /etc/modprobe.d/
cp -f stxAliases.txt /root/.stxAliases
cp -f stratux-dnsmasq.conf /etc/dnsmasq.d/stratux-dnsmasq.conf
cp -f wpa_supplicant_ap.conf /etc/wpa_supplicant/wpa_supplicant_ap.conf
cp -f interfaces /etc/network/interfaces
cp -f sshd_config /etc/ssh/sshd_config

#disable serial console, disable rfkill state restore, enable wifi on boot
#-------------------------------------------------------------------------
sed -i /boot/cmdline.txt -e "s/console=serial0,[0-9]\+ /systemd.restore_state=0 rfkill.default_state=1 /"

# prepare services
#----------------------------------------------------------
systemctl enable ssh
systemctl disable dnsmasq # we start it manually on respective interfaces
systemctl disable dhcpcd
systemctl disable hciuart
systemctl disable triggerhappy
systemctl disable wpa_supplicant
systemctl disable apt-daily.timer
systemctl disable apt-daily-upgrade.timer
systemctl disable man-db.timer

# Run DHCP on eth0 when cable is plugged in
#----------------------------------------------------------
sed -i -e 's/INTERFACES=""/INTERFACES="eth0"/g' /etc/default/ifplugd
