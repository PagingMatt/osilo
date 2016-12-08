#!/usr/bin/python

import sys

if __name__ == "__main__":
  if len(sys.argv) == 3:
    exec_server = sys.argv[0]
    exec_client = sys.argv[1]
    path_repo = sys.argv[2]
  else:
  	print "Usage: python profile-local-server.py <server executable> <client executable> <path to profile repo>"