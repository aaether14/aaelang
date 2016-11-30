cd /home/aaether/Desktop/Test/gcc-build
make -j$(getconf _NPROCESSORS_ONLN)
if [ $? -eq 0 ]; then
make install
fi