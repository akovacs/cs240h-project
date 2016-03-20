% Stanford CS240H Final Project: MapReduce in Haskell
Arpad Kovacs <akovacs@stanford.edu>

This project is an implementation of the MapReduce programming model in Haskell, with an example application that performs word count on a corpus of 42 Shakespeare documents. A more detailed description of the architecture and implementation details are available in the final report.

The project can be run in both single-node standalone mode, as well as a distributed system with a master coordinator and one or more slave nodes.

Setup instructions are provided for Arch Linux and Ubuntu Linux below.

# Setup Instructions

    # Install Haskell stack on Arch Linux
    sudo pacman -S ghc stack

    # Install Haskell stack on Ubuntu Linux
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    echo 'deb http://download.fpcomplete.com/ubuntu wily main'|sudo tee /etc/apt/sources.list.d/fpco.list
    sudo apt-get update && sudo apt-get install stack -y

    # Setup compiler
    stack setup

    # Compile the project
    stack build

# Running standalone single-node wordcount example
The following command will read the 42 .txt documents from the ./corpus directory and execute a wordcount MapReduce job locally:

    stack exec mapreduce standalone

# Running distributed system
Identify the IPv4 address of each computer which will participate in the system

    ip link show

Ensure that all computers participating in the system are visible to each other on the local network, and can transmit/receive UDP multicast datagrams (which are required for node service discovery).

Start up one or more computers as slave nodes which will listen for instructions from the master. Be sure to use the IP address of each slave which you determined in the previous step, as well as a unique port.

    stack exec mapreduce slave "192.168.1.11" 10001

Once all the slaves are running, start the master to execute the MapReduce computation: 

    stack exec mapreduce master "192.168.1.10" 10000

The master will load .txt documents from the ./corpus directory, add them to a workQueue, and the slaves will begin pulling tasks from the workQueue, executing the mapper on the inputs, and streaming the intermediate results to the master.
