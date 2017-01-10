# osilo

[![License](https://img.shields.io/badge/license-ICS-blue.svg)](https://travis-ci.org/m-harrison/osilo/LICENSE) [![Build Status](https://travis-ci.org/m-harrison/osilo.svg?branch=master)](https://travis-ci.org/m-harrison/osilo)

**Note:** Travis CI will fail until a fixed version of Macaroons (PR 3) is published on OPAM.

Using  social  applications  presupposes  handing  over  personal  data  to  these  applications to do what they please with and users have no control over this data once it is handed over.  This project attempts to prototype a system where users host their own data in private repositories and provide controlled sharing with third-party social applications. The system will work over HTTP so that a simple API can be exposed, leaving an entrypoint to build social applications on top of it.

## Building

The project is currently set up with OASIS as the build system, with the source code downloaded and dependencies installed, the code is built by running the following commands.

```
$ oasis setup -setup-update dynamic
$ make
```

This will result in the two project executables being built. `Osilo_server.native` is an implementation of a peer (server) in the system and `Osilo_client.native` is a simple command line client implementation for manual testing. Running either executable with the `-h` flag will display the command line documentation to use the file.

## Tests

There is a small unit test suite and a small performance benchmarking suite in the `tests` directory. To run these use the following commands.

```
$ configure --enable-tests
$ make test
```

Note that this will run both the unit tests and the performance benchmarks, which in combination can take quite a long time. The test executables are both built and can be run separately which may be more desirable.

## Running a peer

A peer using the `Osilo_server.native` implementation always runs on port 6620. When it is started, its hostname (as accessed by other peers and clients) needs to be passed in. For peer(s), pointing to a given silo, this hostname cannot change between running peers. This is because cached and owned data is stored symmetrically under the hostnames in the data silo.

## Datakit

To run a peer using `Osilo_server.native`, it is necessary to start a [Datakit](https://github.com/docker/datakit) server pointing towards a git repository (structured as a data silo). This should use the default Datakit port (5640) and the hostname the port is exposed at should be passed into `Osilo_server.native` using the `-ds` flag.

## Acknowledgements

- `.travis.yml` file was [provided](https://github.com/avsm/ocaml-dockerfile/blob/master/.travis.yml) by Anil Madhavapeddy 
