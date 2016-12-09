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

server = ""

class Pinger(threading.Thread):
  latency = 0
  size = 0
  def run(self):
    start = time.time()
    response = requests.get("http://" + server + ":6620/ping/")
    stop = time.time()
    self.latency = stop - start
    if response.status_code == 200:
      self.size = sys.getsizeof(response.content)

def run_ping_trials(num_clients,num_trials):
  latency_mean = []
  throughput = []

  for n in range(num_trials):
    clients = [Pinger() for i in range(num_clients)]
    start = time.time()
    map(lambda c: c.start(), clients)
    map(lambda c: c.join(), clients)
    stop = time.time()

    latencies = map(lambda c: c.latency, clients)
    latency_mean += (latencies)
    throughput.append(sum(map(lambda c: c.size, clients)) / (1000 * (stop - start)))

  l = statistics.mean(latency_mean)
  t = statistics.mean(throughput)
  if num_clients > 1:
    lv = statistics.stdev(latencies,l)
    tv = statistics.stdev(throughput,t)
  else:
    lv = 0
    tv = 0
  return (l,lv,t,tv)

def profile_local_ping(max_concurrent, trials):
  ls = []
  lvs = []
  ts = []
  tvs = []
  con = range(1,max_concurrent)
  for n in con:
    l,lv,t,tv = run_ping_trials(n,trials)
    ls.append(l)
    lvs.append(lv)
    ts.append(t)
    tvs.append(tv)

  pyplot.figure()
  pyplot.ylabel('Mean latency /s')
  pyplot.xlabel('Concurrent clients')
  pyplot.title("Mean latency against concurrent clients for pinging a local Osilo server")
  pyplot.errorbar(con, ls, yerr=lvs, linestyle="None")
  pyplot.scatter(con, ls, c='b')
  pyplot.show()

  pyplot.figure()
  pyplot.ylabel('Mean latency /s')
  pyplot.xlabel('Throughput /Kbytes s^-1')
  pyplot.title("Mean latency against throughput for pinging a local Osilo server")
  pyplot.errorbar(ts, ls, yerr=lvs, linestyle="None")
  pyplot.errorbar(ts, ls, xerr=tvs, linestyle="None")
  pyplot.scatter(ts, ls, c='b')
  pyplot.show()

if __name__ == "__main__":
  if len(sys.argv) == 3:
    server = sys.argv[1]
    exec_client = sys.argv[2]

    profile_local_ping(100,5)
  else:
    logger.error("Usage: python profile-local-server.py <server> <client executable>")
