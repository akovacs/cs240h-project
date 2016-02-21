README
======

# Install haskell stack

    pacman -S ghc stack

# Install necessary libraries for Tidal http://tidal.lurk.org/getting_started.html

    pacman -S alsa-plugins jack2 qjackctl libsndfile libsamplerate liblo 

    git clone --recursive https://github.com/tidalcycles/Dirt.git
    cd Dirt
    make clean; make


# Run to create new project from scratch

    # stack setup
    # cabal install cabal-install

# Update dependencies in stack.yaml (after adding new libraries) and build

    stack solver --update-config
    stack build

# Start audio

    # Only need to do this once
    sudo gpasswd -a akovacs audio
    # Start daemon in non-realtime mode using -r flag, send to audio card 1 `aplay -l`
    jackd -r -d alsa -d hw:1
    # Start software synth
    ./dirt

# Run standalone commandline example

    stack exec parseini-exe

# Open web ui in browser

    stack exec parseini-web
    firefox "localhost:8888" &
