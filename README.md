# osilo

[![License](https://img.shields.io/badge/license-ICS-blue.svg)](https://travis-ci.org/m-harrison/osilo/LICENSE) [![Build Status](https://travis-ci.org/m-harrison/osilo.svg?branch=master)](https://travis-ci.org/m-harrison/osilo)

**Note:** Travis CI will fail until a fixed version of Macaroons (PR 3) is published on OPAM.

Using  social  applications  presupposes  handing  over  personal  data  to  these  applications to do what they please with and users have no control over this data once it is handed over.  This project attempts to prototype a system where users host their own data in private repositories and provide controlled sharing with third-party social applications. The system will work over HTTP so that a simple API can be exposed, leaving an entrypoint to build social applications on top of it.

## Acknowledgements

- `.travis.yml` file was [provided](https://github.com/avsm/ocaml-dockerfile/blob/master/.travis.yml) by Anil Madhavapeddy 
