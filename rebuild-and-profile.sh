cabal configure --enable-executable-profiling
cabal build > build.out
echo "Running profiled program..."
time ./profile.sh
