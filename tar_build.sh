INSTDIR=$(pwd)/installdir

#clean compile and make the package
./autogen.sh
./configure --prefix=$INSTDIR
make clean
PREFIX=$INSTDIR make 
PREFIX=$INSTDIR make install

#build the package
cd $INSTDIR
tar cvzf ejabberd.tar.gz .
