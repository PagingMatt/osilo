#!/usr/bin/python

import logging
import matplotlib.pyplot as pyplot
import numpy
import requests
import statistics
import subprocess
import sys
import threading
import time

logging.basicConfig(format='%(message)s')
logger = logging.getLogger('local server profiler')
logger.setLevel(10)

class Pinger(threading.Thread):
  latency = 0
  size = 0
  def run(self):
    start = time.time()
    response = requests.get('http://127.0.0.1:6620/ping/')
    stop = time.time()
    self.latency = stop - start
    if response.status_code == 200:
      self.size = sys.getsizeof(response.content)

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

def profile_local_pings(max_concurrent):
  results = []

  for n in range(2,max_concurrent,1):
    clients = [Pinger() for i in range(n)]
    start = time.time()
    map(lambda c: c.start(), clients)
    map(lambda c: c.join(), clients)
    stop = time.time()

    latencies = map(lambda c: c.latency, clients)
    latency_mean = statistics.mean(latencies)
    latency_var = statistics.variance(latencies,latency_mean)

    throughput = sum(map(lambda c: c.size, clients)) / (stop - start)

    results.append(((latency_mean,latency_var),throughput))

  return results

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

    time.sleep(2)
    print ""

    results = zip(*profile_local_pings(50))
    l = zip(*results[0])
    mean_latency = l[0]
    var_latency = l[1]
    throughput = results[1]
    pyplot.figure()
    pyplot.ylabel('Mean latency /s')
    pyplot.xlabel('Throughput /bytes s^-1')
    pyplot.title("Mean latency against throughput for pinging a local Osilo server")
    pyplot.scatter(throughput, mean_latency, c='b')
    pyplot.show()
  else:
    logger.error("Usage: python profile-local-server.py <server executable> <client executable> <path to profile repo>")