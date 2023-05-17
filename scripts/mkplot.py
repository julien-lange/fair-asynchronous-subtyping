#!/usr/bin/python

import sys
import matplotlib.pyplot as plt
from pylab import * 
import numpy as np
import csv
import string
import os
from matplotlib.ticker import ScalarFormatter 
from scipy.optimize import curve_fit

filetype = ".eps"
ticksfontsize = 12
axisfontsize = 15
legentfontsize = 15
mymarkersize = 5


def fitfunc(x, a, b, c):
    return a * np.power(b, x) + c


def mkFitLegend(ca, cb, cc):
    return (r'$F(x)\approx%5.3f + %5.3f * %5.3f^x$' % (cc, ca, cb))


def mkPlot(bfile, outpath):
    tab = np.loadtxt(
        bfile,
        usecols=(3, 1, 4, 0),
        unpack=True,
        delimiter=',',
        dtype=float
    )

    stab = tab[0].argsort()
    final = tab[:, stab]
    tr, tm, tk, tp = final

    plt.figure(figsize=(9, 9))
    fig, ax1 = plt.subplots()

    linx = np.array(tr)
    liny = np.array(tk)
    linspace = np.linspace(0, max(tr))
    logspace = np.logspace(0, max(tr))
    popt, pcov = curve_fit(fitfunc, linx, liny, maxfev=8000)

    print("Fitted curve: " + str(popt))

    preflegend = "Execution time"
    memlegend = "Memory usage"

    plt.yticks(fontsize=ticksfontsize)
    plt.xticks(rotation=40, ha='right', fontsize=ticksfontsize)

    plt.grid(linestyle='dotted', color='gray')

    ax1.set_ylabel('Time (seconds)', fontsize=axisfontsize)
    ax2 = ax1.twinx()
    ax2.set_ylabel('Memory (megabytes)', fontsize=axisfontsize)
    ax3 = ax1.twiny()

    ax1.plot(linspace, fitfunc(linspace, *popt), color='orange', zorder=10)
    ax1.plot(tr, tk, marker='o', markersize=mymarkersize, linestyle='None', color='blue', zorder=20)
    ax2.plot(tr, list(map(lambda x : x/1000000, tm)), marker='.', markersize=mymarkersize, linestyle='None', color='red', zorder=15)

    ax1.legend([mkFitLegend(*popt), preflegend, memlegend], loc=(0.05, 0.8), fontsize=legentfontsize, numpoints=1)
    ax2.legend([memlegend], loc=(0.05, 0.7), fontsize=legentfontsize, numpoints=1)

    ax1.set_xlabel(r'Number of transitions in candidate supertype', fontsize=axisfontsize)

    ax3.set_xlim(ax1.get_xlim())
    ax3.set_xticks(tr)
    ax3.set_xticklabels(tp.astype(int))
    ax3.set_xlabel(r"Parameter")

    posfifx = '-lin' + filetype
    # plt.ticklabel_format(style='sci', axis='x', useOffset=True)

    plt.savefig('./plots/' + outpath, dpi=300, bbox_inches="tight")


if not os.path.exists('./plots'):
    os.makedirs('./plots')

datadir = "./"
if len(sys.argv) > 1:
    datadir = sys.argv[1]

print("Source data:", datadir)

i = 0
for f in os.listdir(datadir):
    if f.startswith("parametrised-benchmarks_") and f.endswith(".csv"):
        ostr = f + "plot-" + string.ascii_lowercase[i] + filetype
        print("Converting " + f + " to " + ostr)
        mkPlot(os.path.abspath(datadir + f), ostr)
        i += 1
