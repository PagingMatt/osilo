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

def run_local_pings(max_concurrent):
  results = []

  for n in range(2,max_concurrent,1):
    clients = [Pinger() for i in range(n)]
    start = time.time()
    map(lambda c: c.start(), clients)
    map(lambda c: c.join(), clients)
    stop = time.time()

    latencies = map(lambda c: c.latency, clients)
    latency_mean = statistics.mean(latencies)

    throughput = sum(map(lambda c: c.size, clients)) / (stop - start)

    results.append((latency_mean,throughput))

  return results # [(mean latency, throughput),...]

def profile_local_pings(max_concurrent, trials):
  lt = []
  for n in range(trials):
    lt.append(run_local_pings(max_concurrent))      # [[(l1,t1),...], [trial 2], ...]

  latencies = map(lambda i: list(zip(*i)[0]), lt)   # [[l1,l2,l3,...], [trial 2], ...]
  throughputs = map(lambda i: list(zip(*i)[1]), lt) # [[t1,t2,t3,...], trial 2], ...]

  average_latency = []
  average_throughput = []

  for n in range(2,max_concurrent,1):
    average_latency.append(statistics.mean(map(lambda l: l[n-2], latencies)))      # [l1,l2,l3,...]
    average_throughput.append(statistics.mean(map(lambda t: t[n-2], throughputs))) # [t1,t2,t3,...]
    
  pyplot.figure()
  pyplot.ylabel('Mean latency /s')
  pyplot.xlabel('Throughput /bytes s^-1')
  pyplot.title("Mean latency against throughput for pinging a local Osilo server")
  pyplot.scatter(average_throughput, average_latency, c='b')
  pyplot.show()

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

    profile_local_pings(20,10)
  else:
    logger.error("Usage: python profile-local-server.py <server executable> <client executable> <path to profile repo>")