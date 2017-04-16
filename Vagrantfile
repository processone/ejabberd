# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update -y
    curl https://packagecloud.io/install/repositories/basho/riak/script.deb.sh | sudo bash
    sudo apt-get install -y build-essential autoconf erlang libexpat1-dev libyaml-dev libpam0g-dev libsqlite3-dev riak mysql-server postgresql postgresql-contrib git

    sudo mysql -u root -e "CREATE USER 'ejabberd_test'@'localhost' IDENTIFIED BY 'ejabberd_test';"
    sudo mysql -u root -e "CREATE DATABASE ejabberd_test;"
    sudo mysql -u root -e "GRANT ALL ON ejabberd_test.* TO 'ejabberd_test'@'localhost';"
    sudo -u postgres psql -U postgres -c "CREATE USER ejabberd_test WITH PASSWORD 'ejabberd_test';"
    sudo -u postgres psql -U postgres -c "CREATE DATABASE ejabberd_test;"
    sudo -u postgres psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE ejabberd_test TO ejabberd_test;"

    cd /vagrant
    ./autogen.sh
    ./configure --enable-all --disable-odbc --disable-elixir
    make xref
  SHELL
end
