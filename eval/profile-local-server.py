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
  def __init__(self,server):
    threading.Thread.__init__(self)
    self.server = server
  def run(self):
    start = time.time()
    response = requests.get("http://" + self.server + ":6620/ping/", headers={'Connection':'close'})
    stop = time.time()
    self.latency = stop - start
    self.size = sys.getsizeof(response.content)

def run_ping_trials(server,num_clients,num_trials):
  latency_mean = []
  throughput = []

  for n in range(num_trials):
    clients = [Pinger(server) for i in range(num_clients)]
    start = time.time()
    map(lambda c: c.start(), clients)
    map(lambda c: c.join(), clients)
    stop = time.time()

    latencies = map(lambda c: c.latency, clients)
    latency_mean += (latencies)
    throughput.append(sum(map(lambda c: c.size, clients)) / (1000 * (stop - start)))

  l = statistics.mean(latency_mean)
  t = statistics.mean(throughput)
  lv = statistics.stdev(latency_mean,l)
  tv = statistics.stdev(throughput,t)

  return (l,lv,t,tv)

def profile_local_ping(max_concurrent, trials):
  ls1 = []
  lvs1 = []
  ts1 = []
  tvs1 = []
  ls2 = []
  lvs2 = []
  ts2 = []
  tvs2 = []
  con = range(1,max_concurrent)
  for n in con:
    l,lv,t,tv = run_ping_trials("127.0.0.1",n,trials)
    ls1.append(l)
    lvs1.append(lv)
    ts1.append(t)
    tvs1.append(tv)

  for n in con:
    l,lv,t,tv = run_ping_trials("172.16.54.52",n,trials)
    ls2.append(l)
    lvs2.append(lv)
    ts2.append(t)
    tvs2.append(tv)

  pyplot.figure()
  pyplot.ylabel('Mean latency /s')
  pyplot.xlabel('Concurrent clients')
  pyplot.title("Mean latency against concurrent clients for pinging a local and a remote Osilo server")
  pyplot.errorbar(con, ls1, yerr=lvs1, linestyle="None", color="blue")
  pyplot.scatter(con, ls1, c='b')
  pyplot.errorbar(con, ls2, yerr=lvs2, linestyle="None", color="green")
  pyplot.scatter(con, ls2, c='g')
  pyplot.show()

  pyplot.figure()
  pyplot.ylabel('Mean latency /s')
  pyplot.xlabel('Throughput /Kbytes s^-1')
  pyplot.title("Mean latency against throughput for pinging a local and a remote Osilo server")
  pyplot.errorbar(ts1, ls1, yerr=lvs1, linestyle="None", color="blue")
  pyplot.errorbar(ts1, ls1, xerr=tvs1, linestyle="None", color="blue")
  pyplot.scatter(ts1, ls1, c='b')
  pyplot.errorbar(ts2, ls2, yerr=lvs2, linestyle="None", color="green")
  pyplot.errorbar(ts2, ls2, xerr=tvs2, linestyle="None", color="green")
  pyplot.scatter(ts2, ls2, c='g')
  pyplot.show()

if __name__ == "__main__":
  if len(sys.argv) == 2:
    exec_client = sys.argv[1]

    profile_local_ping(75,25)
  else:
    logger.error("Usage: python profile-local-server.py <client executable>")
