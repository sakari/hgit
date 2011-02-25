set -e
ghc --make setup.hs -o setup
./setup configure --user --enable-test
./setup build -v
./setup test