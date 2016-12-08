#!/usr/bin/python

import logging
import subprocess
import sys
import threading

logging.basicConfig(format='%(message)s')
logger = logging.getLogger('local server profiler')
logger.setLevel(10)

class Server(threading.Thread):
  def __init__(self,exec_server):
    threading.Thread.__init__(self)
    self.executable = exec_server

  def run(self):
    cmd = [self.executable,"start","-h","127.0.0.1","-k","testtesttesttesttesttesttesttest","-ds","172.17.0.2"]
    logger.debug(" ".join(cmd))
    exit = subprocess.call(cmd)
    if exit != 0:
      logger.error("Server quit with exit code " + str(exit))

class Datakit(threading.Thread):
  def __init__(self,path_repo):
    threading.Thread.__init__(self)
    self.repository = path_repo

  def run(self):
    cmd = ["docker","run","-ti","-v",self.repository + ":/data","docker/datakit"]
    logger.debug(" ".join(cmd))
    exit = subprocess.call(cmd)
    if exit != 0:
      logger.error("Datakit server quit with exit code " + str(exit))

if __name__ == "__main__":
  if len(sys.argv) == 4:
    exec_server = sys.argv[1]
    exec_client = sys.argv[2]
    path_repo = sys.argv[3]

    logger.info("Starting datakit instance for repository " + path_repo)
    datakit = Datakit(path_repo)
    datakit.start()

    logger.info("Starting up server at " + exec_server)
    server = Server(exec_server)
    server.start()

  else:
    logger.error("Usage: python profile-local-server.py <server executable> <client executable> <path to profile repo>")