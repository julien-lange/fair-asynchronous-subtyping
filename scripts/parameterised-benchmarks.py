#!/usr/bin/python3
import sys
import subprocess
import os
import os.path
import string
import time
# import numpy as np
import csv


# Number of iterations (> 1)
maxiterations = 2 # 6

logfile = "log-file-benchmarks.txt"
tmpfile = "tmp-cfsm.txt"


# OUTPUT FILE
prefname = "parametrised-benchmarks"
MIN = "--minimise"
# MIN = ""


# TIMEOUT (in seconds)
# cmdtimeout = 360
# cmdtimeout = 1500 # 25 min
cmdtimeout = 300 #



def cleanup(): 
    subprocess.call(["killall","Checker"]
                    , stdout=subprocess.PIPE
                    , stderr=subprocess.PIPE)




def runOverRange(sid,minx, maxx, gencmd, step):
    print("SID: "+sid)
    name = prefname+"_"+sid+"_"+str(minx)+"-"+str(maxx)+".csv"
    with open(name,"w") as out:    
        write = csv.writer(out) 
        with open(name+logfile, "wb") as log_file:
            for x in range(minx,maxx+step,step):
                        print("Test: ",str(x))
                        gcmd = gencmd(x)
                        gcmd.wait(timeout=cmdtimeout)                                                        
                        nstates = ""
                        ntrans = ""
                        if len(MIN)> 0: 
                            infocmd = subprocess.Popen(["../Checker","t1.txt","t2.txt", "--info", MIN], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                        else:
                            infocmd = subprocess.Popen(["../Checker","t1.txt","t2.txt", "--info"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                        infocmd.wait(timeout=cmdtimeout)                                                        
                        for line in infocmd.stdout:
                            sp = line.decode("utf-8")
                            if 'Size' in sp:
                                sizes = sp.split(",")
                                nstates = sizes[3]
                                ntrans = sizes[4].strip()
                        timings = []
                        memsize = []
                        for it in range(1,maxiterations):
                            print("Running Checker: ",str(x))
                            startt = time.time() # time in seconds
                            if len(MIN)> 0:                                
                                kmccmd = subprocess.Popen(
                                    ["/usr/bin/time","-l","../Checker","t1.txt","t2.txt", MIN]
                                    , stdout=subprocess.PIPE
                                    , stderr=subprocess.PIPE)
                            else:
                                kmccmd = subprocess.Popen(
                                    ["/usr/bin/time","-l","../Checker","t1.txt","t2.txt"]
                                    , stdout=subprocess.PIPE
                                    , stderr=subprocess.PIPE)
                            try:
                                kmccmd.wait(timeout=cmdtimeout)
                                endt = time.time()
                                txt = "Measured execution time: "+str(endt-startt)
                                print(txt)
                                for line in kmccmd.stderr:
                                    sp = line.decode("utf-8")
                                    if 'maximum resident set size' in sp:
                                        spl = sp.split()
                                        memsize.append(int(spl[0]))
                                    log_file.write(line)
                                log_file.write((txt+"\n").encode())
                                timings.append(endt-startt)
                            except subprocess.TimeoutExpired:
                                kmccmd.kill()
                                kmccmd.wait()
                                cleanup()
                                print("/!\ Checker timedout, terminating these benchmarks")
                                return
                        avg = sum(timings)/float(len(timings))
                        avgmem = sum(memsize)/float(len(memsize))
                        write.writerow([x,avgmem,nstates,ntrans,avg]+timings)
                            





def moreRcvWidths(x):
    cmd = subprocess.Popen(["./GenAsyncTypes",str(x),"1","1"], stdout=subprocess.PIPE)    
    return cmd

def moreRcvDepths(x):
    cmd = subprocess.Popen(["./GenAsyncTypes","1", str(x),"1"], stdout=subprocess.PIPE)
    return cmd 

def moreSndWidths(x):
    cmd = subprocess.Popen(["./GenAsyncTypes","1", "1",str(x)], stdout=subprocess.PIPE)
    return cmd 



#runOverRange("B", 1, 60, moreRcvDepths, 5)
#runOverRange("C", 1, 60, moreSndWidths, 5)
runOverRange("A", 1, 20, moreRcvWidths, 1)
