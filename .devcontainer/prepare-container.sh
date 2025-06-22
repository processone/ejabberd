echo "export PATH=/workspaces/ejabberd/_build/relive:$PATH" >>$HOME/.bashrc
echo "COOKIE" >$HOME/.erlang.cookie
#chmod 400 $HOME/.erlang.cookie
echo "-= complie source =-"
./autogen.sh
./configure
make
make dev
