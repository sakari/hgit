set -e
ghc --make setup.hs -o setup
./setup clean
./setup configure --user --enable-test
./setup build -v
./setup test