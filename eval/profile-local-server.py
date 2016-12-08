#!/usr/bin/python

import logging
import subprocess
import sys

logger = logging.getLogger('local server profiler')

class Server(threading.Thread):
  def __init__(self,exec_server):
  	Thread.__init__(self)
    self.executable = exec_server

  def run(self):
  	cmd = [self.executable,"start","-h","127.0.0.1","-k","testtesttesttesttesttesttesttest","-ds","172.17.0.2"]
    logger.debug(" ".join(cmd))
    exit = subprocess.call(cmd)
    if exit != 0:
    	logger.error("Server quit with exit code " + str(exit))

if __name__ == "__main__":
  if len(sys.argv) == 3:
    exec_server = sys.argv[0]
    exec_client = sys.argv[1]
    path_repo = sys.argv[2]

    logger.info("Starting up server at " + exec_server)
    server = new Server(exec_server)
    server.start()

  else:
  	logger.error("Usage: python profile-local-server.py <server executable> <client executable> <path to profile repo>")