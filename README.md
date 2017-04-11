# osilo

[![License](https://img.shields.io/badge/license-ICS-blue.svg)](https://travis-ci.org/m-harrison/osilo/LICENSE) [![Build Status](https://travis-ci.org/m-harrison/osilo.svg?branch=master)](https://travis-ci.org/m-harrison/osilo)

Using  social  applications  presupposes  handing  over  personal  data  to  these  applications to do what they please with and users have no control over this data once it is handed over.  This project attempts to prototype a system where users host their own data in private repositories and provide controlled sharing with third-party social applications. The system will work over HTTP so that a simple API can be exposed, leaving an entrypoint to build social applications on top of it.

## Datakit

To run a peer using `Osilo_server.native`, it is necessary to start a [Datakit](https://github.com/docker/datakit) server pointing towards a git repository (structured as a data silo). This should use the default Datakit port (5640) and the hostname the port is exposed at should be passed into `Osilo_server.native` using the `-ds` flag.

## Working with the code

Documentation generated with ocamldoc for the code can be found [here](https://cdn.rawgit.com/m-harrison/osilo.github.io/b10d9497/www/index.html).

## Acknowledgements

- `.travis.yml` file was [provided](https://github.com/avsm/ocaml-dockerfile/blob/master/.travis.yml) by Anil Madhavapeddy
