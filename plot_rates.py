

import matplotlib.pyplot as plt 
import numpy as np
import scipy
import json
import seaborn as sns
import argparse
import sys 
import os 
from scipy.stats import bootstrap 
import time 
import multiprocessing as mp
from multiprocessing import Pool 



parser = argparse.ArgumentParser(description='Plotter for maxbandit results')
parser.add_argument('-f', '--file', default="", type=str, nargs="+", help='file to extract from')
parser.add_argument('-p', '--prefix', default="nguyen-results-noexp/", type=str, help='prefix to file path')
parser.add_argument('-s', '--suffix', default="", type=str, help='suffix to file path')
parser.add_argument('-n', '--name', default=None, type=str, nargs="+", help='suffix to file path')
parser.add_argument('-i', '--index', default=50, type=int, help='max index')
args = parser.parse_args()




plt.rcParams["figure.figsize"] = (12,16)
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42
plt.rcParams.update({'font.size': 18})
plt.rc('ytick', labelsize=10) 
plt.rc('xtick', labelsize=10) 

samr_files = []
bandit_files = []

names = args.file if args.name is None else args.name

samr_files = ["nguyen1-range-samr2-217193", "nguyen2-range-samr2-217194", "nguyen3-range-samr2-217196", "nguyen4-range-samr2-217198",
"nguyen5-range-samr2-217199", "nguyen6-range-samr2-217202", "nguyen7-range-samr2-217203", "nguyen8-range-samr2-217205"]
bandit_files = ["nguyen1-range-bandit-217180", "nguyen2-range-bandit-217178", "nguyen3-range-bandit-217175", "nguyen4-range-bandit-217173", "nguyen5-range-bandit-217171", 
"nguyen6-range-bandit-217167", "nguyen7-range-bandit-217166","nguyen8-range-bandit-217160"]
gesmr_files = ["nguyen1-range-gesmr-218332", "nguyen2-range-gesmr-218331", "nguyen3-range-gesmr-218330", "nguyen4-range-gesmr-218329",
"nguyen5-range-gesmr-218328", "nguyen6-range-gesmr-218327", "nguyen7-range-gesmr-218281", "nguyen8-range-gesmr-218231"]

total_files = [samr_files, bandit_files, gesmr_files]

names = ["Nguyen1", "Nguyen2", "Nguyen3", "Nguyen4", "Nguyen5", "Nguyen6", "Nguyen7", "Nguyen8"]
# names =  ["Nguyen1", "Nguyen2", "Nguyen3"]

colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2',
 '#7f7f7f', '#bcbd22', '#17becf']

def get_rates(directory):
    rates = []
    for i in range(1,args.index+1):
        with open(directory + f"/{i}.rates","r") as f:
            r = json.load(f)[:300]
            r = np.log10(r)
            r = np.mean(r,axis=-1)
            r = np.pad(r, (0,300-len(r)), constant_values=np.nan)
            rates.append(r)
    rates = np.nanmean(rates, axis=0)
    return rates[~np.isnan(rates)]

def get_arr_at_idx(arr,idx):
    if len(arr)<=idx:
        return None 
    return arr[idx]

def bt(r):
    t = time.time()
    ci_ = bootstrap((r,),np.mean).confidence_interval
    mean = np.mean(r)
    print(f"TIME {time.time()-t}")
    return mean,(ci.low,ci.high)

def rate_ci_by_generation(directory):
    rates = []
    for i in range(1,args.index+1):
        with open(directory + f"/{i}.rates","r") as f:
            r = json.load(f)[:300]
            r = np.log10(r)
            rates.append(np.mean(r,axis=-1))
    rates_transpose = [[r[i] for r in rates if len(r)>i] for i in range(300)]
    rates_transpose = [np.array(r) for r in rates_transpose if len(r)>0]
    rates_bootstrap = [bootstrap((r,),np.mean).confidence_interval for r in rates_transpose if len(r)>1]
    rates_bootstrap = [(c.low,c.high) for c in rates_bootstrap]
    rates_mean = [np.mean(r) for r in rates_transpose]
    return rates_mean,rates_bootstrap


def rate_ci_parallel(directory):
    rates = []
    for i in range(1,args.index+1):
        with open(directory + f"/{i}.rates","r") as f:
            r = json.load(f)[:300]
            r = np.log10(r)
            rates.append(np.array(r))
    rates_transpose = [[r[i] for r in rates if len(r)>i] for i in range(300)]
    rates_transpose = [np.concatenate(r,axis=0) for r in rates_transpose if len(r)>0]
    rates_bootstrap = []
    processes = []
    with Pool(processes=10) as pool:
        mci = pool.map(bt,rates_transpose)
        
    
    rates_bootstrap = [m for m,ci in mci]
    rates_mean = [ci for m,ci in mci]
    return rates_mean,rates_bootstrap

def rate_ci(directory):
    rates = []
    for i in range(1,args.index+1):
        with open(directory + f"/{i}.rates","r") as f:
            r = json.load(f)[:300]
            r = np.log10(r)
            rates.append(np.array(r))
    rates_transpose = [[r[i] for r in rates if len(r)>i] for i in range(300)]
    rates_transpose = [np.concatenate(r,axis=0) for r in rates_transpose if len(r)>0]
    rates_bootstrap = []
    for i,r in enumerate(rates_transpose):
        t = time.time()
        ci_ = bootstrap((r,),np.mean).confidence_interval
        rates_bootstrap.append(ci_)
        print(f"DIR {directory} I {i} CI {[ci_.low,ci_.high]} TIME {time.time()-t}")
    rates_bootstrap = [(c.low,c.high) for c in rates_bootstrap]
    rates_mean = [np.mean(r) for r in rates_transpose]
    return rates_mean,rates_bootstrap

def error_ci(directory, method="r1"):
    print(f"DIR {directory}")
    errors = []
    for i in range(1,args.index+1):
        with open(directory + f"/{i}.errs","r") as f:
            r = json.load(f)[:300]
            r1 = np.mean(np.log(np.array(r)+1), axis=-1)
            r2 = np.log(np.mean(r,axis=-1)+1)
            r = r1 if method=="r1" else r2
            # print(f"INDEX {i} SHAPE {r1.shape} LEN {np.array(r).shape} FILE {directory}/{i}.errs")
            errors.append(np.reshape(r,[-1]))
    # print(f"ERROR SHAPE {[(i,r.shape) for i,r in enumerate(errors)]}")
    # print(f"TYPES {[(i,type(r),r.shape) for i,r in enumerate(errors)]}")
    errors_transpose = [[r[i] for r in errors if len(r)>i] for i in range(300)]
    errors_transpose = [np.array(r) for r in errors_transpose if len(r)>0]
    # print(bootstrap((errors_transpose[0],), np.mean).confidence_interval)
    errors_bootstrap = [bootstrap((r,),np.mean).confidence_interval for r in errors_transpose if len(r)>2]
    errors_bootstrap = [(c.low,c.high) for c in errors_bootstrap]
    print(errors_bootstrap)
    print()
    print()
    errors_mean = [np.mean(r) for r in errors_transpose]
    return errors_mean,errors_bootstrap


fig, axes = plt.subplots(nrows=len(samr_files),ncols=2, squeeze=False)

def plot_data(label, file, c):
    mean, rbt = rate_ci_by_generation(args.prefix+file+args.suffix)
    if label is not None:
        ax[0].plot(mean, label=label)
    else:
        ax[0].plot(mean)

    if c==colors[1]:
        print(mean)
    ax[0].fill_between(np.arange(len(rbt)), [c[0] for c in rbt], [c[1] for c in rbt],color=c, alpha=.1)
    mean,bt = error_ci(args.prefix+file+args.suffix)
    ax[1].plot(mean)
    
    ax[1].fill_between(np.arange(len(bt)), [c[0] for c in bt], [c[1] for c in bt],color=c, alpha=.1)
    

for i,(ax,s,b,g,n,c) in enumerate(zip(axes,samr_files, bandit_files, gesmr_files,names, colors)):
    print(n)
    if i==0:
        plot_data("SAMR", s,colors[0])
        plot_data("Bandit", b,colors[1])
        plot_data("GESMR", g,colors[2])
        ax[0].set_title("Log UMAD Rate")
        ax[1].set_title("Log Best Error")
    else:
        plot_data(None, s,colors[0])
        plot_data(None, b,colors[1])
        plot_data(None, g,colors[2])
    ax[0].set_ylabel(n)
    

fig.supylabel("Problem")
fig.supxlabel("Generation")
fig.align_ylabels()
fig.legend(loc = "upper center", ncols=5,bbox_to_anchor=(0.5, 1.05))
# fig.legend()
plt.tight_layout()
plt.savefig("sr_rates.pdf", bbox_inches="tight")
plt.show()





