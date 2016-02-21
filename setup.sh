README
======

# Install haskell stack

    pacman -S ghc stack

# Install necessary libraries for Tidal http://tidal.lurk.org/getting_started.html

    pacman -S alsa-plugins jack2 qjackctl libsndfile libsamplerate liblo 

    git clone --recursive https://github.com/tidalcycles/Dirt.git
    cd Dirt
    make clean; make

# Setup audio

    sudo gpasswd -a akovacs audio
    # non-realtime using -r flag, send to audio card 1 `aplay -l`
    jackd -r -d alsa -d hw:1
    
    ./dirt

# Run to create new project from scratch

    # stack setup
    # cabal install cabal-install

# Update dependencies in stack.yaml (after adding new libraries)

    stack solver --update-config

# Compile and run

    stack build
    stack exec parseini-web

# Open web ui in browser

    firefox "localhost:8888" &
