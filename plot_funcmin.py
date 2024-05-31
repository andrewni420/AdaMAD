

import matplotlib.pyplot as plt 
import numpy as np
import scipy
import json
import seaborn as sns
import argparse
import sys 
import re 
import os 
from itertools import product
from scipy.stats import bootstrap, ttest_ind
from scipy.special import softmax 

parser = argparse.ArgumentParser(description='Plotter for maxbandit results')
parser.add_argument('-f', '--file', default="", type=str, nargs="+", help='file to extract from')
parser.add_argument('-p', '--prefix', default="funcmin-results4/", type=str, help='prefix to file path')
parser.add_argument('-s', '--suffix', default="", type=str, help='suffix to file path')
parser.add_argument('--std', default=None, type=int, nargs="+", help='stdev of file')
parser.add_argument('--dim', default=None, type=int, nargs="+", help='dimension of file')
parser.add_argument('--problem', default=None, type=str, nargs="+", help='problem of file')
parser.add_argument('--method', default=None, type=str,nargs="+", help='problem of file')
parser.add_argument('--xex', action="store_true", help='whether to display xe^x')
args = parser.parse_args()



# arr = np.arange(20)
# # for t1,t2,eps in zip(temp1,temp2,epsilon):
# #     res = eps*softmax(t1*arr)+(1-eps)*softmax(t2*arr)
# #     plt.plot(res, label=f"t1 {t1} t2 {t2} eps {eps}")
# #     plt.legend()
# arr =  (arr-0)/100
# res = np.exp(-arr**2)
# plt.plot(res/res.sum())
# plt.ylim(bottom=0)
# plt.show()
# sys.exit(0)


plt.rcParams["figure.figsize"] = (12,12)
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42
plt.rcParams.update({'font.size': 18})
plt.rc('ytick', labelsize=10) 
plt.rc('xtick', labelsize=10) 


def process_file(filename, method):
    errors = []
    print(filename)
    for i in range(1,51):
        # print(filename + f"/{i}.errs")
        with open(filename + f"/{i}.errs","r") as f:
            # print(f.read())
            arr = json.load(f)[:1000]
            # print(np.array(arr).shape)
            errors.append(arr)
    errors = np.array(errors)
    # if method=="lamr":
    #     print(errors[:,-1])
    # print(errors.shape)

    analysis = []
    for i in range(1,51):
        # print(filename + f"/{i}.analysis")
        with open(filename + f"/{i}.analysis","r") as f:
            # print(f.read())
            arr = json.load(f)[:1000]
            if method in ["bandit3-1", "bandit", "bandit3"]:
                analysis.append([a[-100:] for a in arr])
                # print(np.array(analysis[-1]).shape)
            else: 
                analysis.append(arr)
            # print([np.array(a).shape for a in arr])

    # print([np.array(a).shape for a in analysis])

    analysis = np.array(analysis)
    # print(analysis.shape)
    return errors,analysis


def process_analysis(method, analysis):
    tiles = np.arange(6667)/6667*200-100+0.015
    tiles /= np.log(10)
    if method=="bandit":
        return np.log10(analysis).mean(axis=-1).mean(axis=0)
        # analysis = analysis[0]
        # analysis_roll = np.roll(analysis,1, axis=0)
        # analysis_roll[0]*=0
        # return (tiles*(analysis- analysis_roll)).mean(axis=-1) #((analysis - analysis_roll)).mean(axis=-1).mean(axis=0)#*tiles.reshape([1,1,-1])).mean(axis=-1).mean(axis=0)
    elif method == "samr":
        return np.log10(analysis).mean(axis=-1).mean(axis=0)
    elif method=="lamr":
        return np.log10(analysis).mean(axis=0)
    else:
        return np.log10(analysis).mean(axis=-1).mean(axis=0)

# ax1 = plt.subplot2grid((1, 2), (0, 0))
# ax2 = plt.subplot2grid((1, 2), (0, 1))
# for i,f in enumerate(args.file):
#     errors, analysis = process_file(args.prefix+f+args.suffix)
#     errors = np.log10(np.mean(errors,axis=0))
#     ax1.plot(errors, label=f)
#     analysis = process_analysis(["bandit", "gsemr"][i], analysis)
#     ax2.plot(analysis, label=f)
#     print(f)
#     print(errors)
#     # print(f"LOG RANGE {np.ceil(np.min(errors))} {np.floor(np.max(errors))+1}")
#     # log_range = np.arange(np.ceil(np.min(errors)),np.floor(np.max(errors))+1,1)
#     # log_range = log_range[::max(1,int(len(log_range)/4))]
#     # ax1.set_yticks(log_range, ["$10^{"+str(int(i))+"}$" for i in log_range])
# ax1.set_title(args.file)
# ax1.set_xlabel("Generations")
# ax1.set_ylabel("Function Value")
# ax1.locator_params(nbins=4, axis="y")
# plt.legend()
# plt.show()
# sys.exit(0)

problems = ["ackley", "griewank", "rastrigin", "rosenbrock", "sphere", "linear"]
# problems = ["ackley"]
methods = ["bandit", "gsemr", "lamr", "samr"]

if args.problem is not None:
    problems = args.problem

if args.method is not None:
    methods = args.method


directory = "/home/ani24/propeller-master/funcmin-results-ensemble"

def construct_file(problem, method):
    pattern = f"{problem}-{method}-.*"
    for file in os.listdir(directory):
        if re.match(pattern, file):
            return file

def find_dimensions(n):
    for i in range(int(np.sqrt(n_plots)),-1,-1):
        if n_plots%i==0:
            return i,n_plots/i

labels = {"gsemr": "GESMR", "bandit3-1": "Bandit","bandit3": "Bandit","bandit": "Bandit", "lamr": "LAMR-100", "samr": "SAMR"}

n_plots = len(problems)
n_cols = 1

n_cols, n_rows = 2, n_plots//2 

# fig = plt.figure(layout="constrained")

# subfigs = fig.subfigures(1, 2, wspace=0.07, hspace=0.07)
# axsLeft = subfigs[0].subplots(n_plots, 1)
# axsRight = subfigs[1].subplots(n_plots, 1)
# subfigs[1].supylabel("Average Log Mutation Strength")
# subfigs[0].supylabel("Average Log Best Function Value")
colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2',
 '#7f7f7f', '#bcbd22', '#17becf']

def get_ci(data):
    b = bootstrap((data,), np.mean, method="percentile")
    return [b.confidence_interval.low, b.confidence_interval.high]



for i,p in enumerate(problems):
    print(p)
    a1 = plt.subplot2grid((n_plots, 2), (i, 0))
    a2 = plt.subplot2grid((n_plots, 2), (i, 1))
    # a1 = axsLeft[i]
    # a2 = axsRight[i]
    a1.set_title(p.capitalize(), fontsize=15)
    a2.set_title(p.capitalize(), fontsize=15)
    t_test_data = []
    for j,m in enumerate(methods):
        print(m)
        err,analysis = process_file(f"{directory}/"+construct_file(p,m),m)
        
        
        if p=="linear":
            err = -np.log(-err)
        else:
            err = np.log(err)
        t_test_data.append(err[:,-1])
        print([np.mean(err[:,-1]), bootstrap((err[:,-1],), np.mean).confidence_interval])
        c_err = [get_ci(err[:,i]) for i in range(err.shape[-1])]
        err = err.mean(axis=0)

        if m=="lamr":
            analysis = np.log(analysis)
        else:
            analysis = np.log(analysis).mean(axis=-1)
        c_an = [get_ci(analysis[:,i]) for i in range(analysis.shape[-1])]
        analysis = analysis.mean(axis=0)

        # if p=="linear":
        #     err = -np.log10(-err)
        # else:
        #     err = np.log10(err)
        # analysis = np.log(analysis)
        if p=="ackley":
            c = a1.plot(err, label=labels[m])
            legend_h, legend_l = a1.get_legend_handles_labels()
        else:
            a1.plot(err)
        a1.fill_between(np.arange(len(c_err)), [c[0] for c in c_err], [c[1] for c in c_err],color=colors[j], alpha=.1)
        a2.plot(analysis)
        a2.fill_between(np.arange(len(c_err)), [c[0] for c in c_an], [c[1] for c in c_an], color=colors[j], alpha=.1)
    
    for i1, i2 in [(0,1), (0,2), (0,3), (1,2), (1,3), (2,3)]:
        print((i1,i2))
        print(ttest_ind(t_test_data[i1],t_test_data[i2], equal_var = False))
fig = plt.gcf()

leg = fig.legend(legend_h, legend_l, bbox_to_anchor=(0.5, 1.05), loc='upper center', ncols=4)
# fig.supylabel("Average Log Best Function Value", x=0)
# fig.supylabel("Average Log Mutation Rate", x=1)
fig.text(0, 0.5, "Average Log Best Function Value", rotation=90, va="center", ha="center")
fig.text(1.01, 0.5, "Average Log Mutation Rate", rotation=90, va="center", ha="center")
fig.supxlabel("Generation")

plt.tight_layout()
plt.savefig("func_min.pdf", bbox_inches="tight")

plt.show()
sys.exit(0)

    




errors, analysis = process_file(args.prefix+args.file+args.suffix)

errors = -np.log10(-np.mean(errors,axis=0))
ax1 = plt.subplot2grid((1, 1), (0, 0))
ax1.plot(errors)
ax1.set_title(args.file)
ax1.set_xlabel("Generations")
ax1.set_ylabel("Function Value")
ax1.locator_params(nbins=4, axis="y")
print(f"LOG RANGE {np.ceil(np.min(errors))} {np.floor(np.max(errors))+1}")
log_range = np.arange(np.ceil(np.min(errors)),np.floor(np.max(errors))+1,1)
log_range = log_range[::max(1,int(len(log_range)/4))]
ax1.set_yticks(log_range, ["$10^{"+str(int(i))+"}$" for i in log_range])
plt.show()


sys.exit(0)

if args.index is None:
   
   from matplotlib.ticker import StrMethodFormatter
   
   ax1 = plt.subplot2grid((4, 2), (0, 0))
   ax2 = plt.subplot2grid((4, 2), (0, 1))
   ax3 = plt.subplot2grid((4, 2), (1, 0), sharex=ax1)
   ax4 = plt.subplot2grid((4, 2), (1, 1), sharex=ax2)
   ax5 = plt.subplot2grid((4, 2), (2, 0), colspan=2)
   ax6 = plt.subplot2grid((4, 2), (3, 0), colspan=2, sharex=ax5)
   
   ax1 = sns.heatmap(arr_w, ax=ax1, cbar=False)
   ax1.set_xticks(get_heatmap(arr_w),get_ticks[1], rotation=0)
   ax1.set_title("Bandit Controller Weights")
   ax1.tick_params("both", reset=True, top=False, right=False)
   ax1.set_ylabel("Generation")

   ax2 = sns.heatmap(arr_n, ax=ax2, cbar=False)
   ax2.set_xticks(get_heatmap(arr_n),get_ticks[1], rotation=0)
   ax2.set_title("Frequencies of Sampled UMAD Rates")
   ax2.tick_params('both', reset=True, top=False, right=False)
   intervals = np.arange(5,len(arr_w)-1,int((len(arr_w)-10)/4))
   # intervals[-1]=177
   # intervals = [5, 75, 150, 225, 295]
   for i in intervals:
      ax3.plot(arr_w[i], label=i)
      ax4.plot(arr_n[i], label=i)

   ax3.set_xticks(get_heatmap(arr_w),get_ticks[1])
   ax3.legend(title="Generation")
   ax3.set_xlabel("Log10 UMAD Rate")
   ax3.set_ylabel("Log-Diff Reward")
   # ax3.set_ylim(bottom=0)

   ax4.set_xticks(get_heatmap(arr_n),get_ticks[1])
   ax4.legend(title="Generation")
   ax4.set_ylim(bottom=0)
   ax4.set_xlabel("Log10 UMAD Rate")

   ax5.plot(errs, label="Average Log-error")
   # ax5.set_ylim((0,500))
   ax5.legend()

   ax6.plot(rew, label="Rewards")
   ax6.set_xlabel("Generation")
   ax6.legend()
   plt.suptitle("Syllables")
   plt.savefig(args.prefix+args.file+args.suffix+"-heatmap.png", bbox_inches='tight')
   plt.show()
   

else:
   arr = arr[args.index]
   plt.plot(np.arange(len(arr))/len(arr)*10-10,arr)
   plt.xticks(np.log(10.**np.arange(-2,1)),10.**np.arange(-2,1))
   plt.show()
   dir_name = args.prefix+args.file+args.suffix+"-slices"
   os.makedirs(dir_name, exist_ok=True)
   plt.savefig(dir_name+f"/{args.index}.png")

# normal = -4,1
# other = -2,1
# Y = np.arange(0, len(arr))
# X = np.arange(len(arr[0]))/len(arr[0])*10-10

# print(X.shape)
# print(Y.shape)
# X, Y = np.meshgrid(X, Y)

# print(X.shape)
# print(Y.shape)

# fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
# ax.plot_surface(X, Y, arr)
# ax.set_xticks(np.log(10.**np.arange(-4,1)),10.**np.arange(-4,1))
# plt.show()

# if isinstance(arr[0],list) or (isinstance(arr, np.ndarray) and arr.ndim>1):
#     f, (ax1, ax2) = plt.subplots(nrows=2)
#     for a,l in zip(arr,labels):
#         # ax1.plot(np.exp(np.arange(len(a))/len(a)*10-10),a)
#         ax1.plot(np.arange(len(a))/len(a)*10-10,np.cumsum(a/sum(a)), label=l)
#         ax2.plot(np.arange(len(a))/len(a)*10-10,a, label=l)
#     # ax1.set_xticks(np.arange(11)/10)
#     ax1.set_xticks(np.log(10.**np.arange(-4,1)),10.**np.arange(-4,1))
#     ax2.set_xticks(np.log(10.**np.arange(-4,1)),10.**np.arange(-4,1))
#     plt.legend()
#     plt.show()
# else:
#     # plt.plot(np.exp(np.arange(len(arr))/len(arr)*10-10),arr)
#     plt.plot(np.arange(len(arr))/len(arr)*10-10,arr/max(arr))
#     plt.plot(np.arange(len(arr))/len(arr)*10-10,np.cumsum(arr/sum(arr)))
#     plt.xticks(np.log(10.**np.arange(-4,1)),10.**np.arange(-4,1))
#     # plt.ylim([0,max(arr)*1.1])
#     plt.show()
