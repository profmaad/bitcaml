bitcaml
=======

bitcaml is striving to be a Bitcoin node implementation in OCaml.
Currently it is mostly a collection of OCaml code that does something akin to a part of Bitcoin..

Prerequisites
=============
The code uses the following OCaml libraries, available via OPAM:

* Bitstring (https://code.google.com/p/bitstring/)
* Sha (https://github.com/vincenthz/ocaml-sha)
* sqlexpr (https://github.com/mfp/ocaml-sqlexpr)
* Cryptokit

Additionally, it uses my own ocaml-microecc bindings for the micro-ecc library.
The library itself is available here: https://github.com/kmackay/micro-ecc
The bindings are here: https://github.com/profmaad/ocaml-microecc

Building & Installation
=======================

After cloning the repository, perform the following to build bitcaml:
```sh
oasis setup
./configure
make
```

The build process is currently configured to build a bytecode executable which can be run as
```sh
./main.byte
```

Some configuration variables are available in ```src/config.ml```.
To run, at least ```bitcaml_folder``` will have to be changed to a valid location. It will hold the blockchain used by bitcaml.

The binary currently connects to one peer, as specified by ```peer_ip_address``` and ```peer_port``` in ```src/config.ml```. It currently defaults to testnet and I would seriously advise against using this on mainnet.

State
=====
The following is more or less implemented:

* Network protocol (nearly complete, lacking support for alert messages)
* Scripting engine (complete, but not fully tested)
* Block verification
* Blockchain download
* Blockchain storage
* Blockchain reorganisation (switching sidechain to mainchain, untested)

The following is missing:
* Transaction verification
* Transaction memory pool
* Transaction and block relaying
* Connecting to multiple peers
* Managing bitcoin network time
* Any kind of wallet or enduser functionality

Disclaimer
==========
DO NOT USE THIS ON MAINNET. Seriously, the code is incomplete, untested and not fit to participate in mainnet. Use on testnet at your own risk.

Since it doesn't have wallet functionality, you can't use it on your funds anyway, and that's good.

Copyright
=========
Copyright 2014 by Maximilian Wolter (Prof. MAAD)
