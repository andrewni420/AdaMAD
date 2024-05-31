

import matplotlib.pyplot as plt 
import numpy as np
import scipy
import json
import seaborn as sns
import argparse
import sys 
import os 

parser = argparse.ArgumentParser(description='Plotter for maxbandit results')
parser.add_argument('-f', '--file', default="", type=str, help='file to extract from')
parser.add_argument('-p', '--prefix', default="maxbandit-results-fixlexicase/", type=str, help='prefix to file path')
parser.add_argument('-s', '--suffix', default="", type=str, help='suffix to file path')
parser.add_argument('-t', '--temp', default=-1, type=float, help='softmax temperature')
parser.add_argument("-i", '--index', default=None, type=int, help='index to visualize')
parser.add_argument('--small', action="store_true", help='whether we\'re searching over the small set of umad rates')
parser.add_argument('--xex', action="store_true", help='whether to display xe^x')
args = parser.parse_args()

plt.rcParams["figure.figsize"] = (12,6)

plt.rcParams['figure.constrained_layout.use'] = True
# normal_ticks = (np.log(10.**np.arange(-4,1)),10.**np.arange(-4,1))
small_ticks = (np.log(10.**np.arange(-2,0)),10.**np.arange(-2,0))
normal_ticks = (np.log(10.**(np.arange(-8,1)/2)),(np.arange(-8,1)/2))
# normal_heatmap_x = lambda a: (np.log(10.**np.arange(-4,1))+10)*(len(a[0])/10)
small_heatmap_x = lambda a: (np.log(10.**np.arange(-2,1))+5)*(len(a[0])/4)
normal_heatmap_x = lambda a: (np.log(10.**(np.arange(-8,1)/2))+10)*(len(a[0])/10)
normal_x = lambda a: np.arange(len(a[0]))/len(a[0])*10-10
small_x = lambda a: np.arange(len(a[0]))/len(a[0])*4-5

get_ticks = small_ticks if args.small else normal_ticks
get_heatmap = small_heatmap_x if args.small else normal_heatmap_x
get_x = small_x if args.small else normal_x

with open(args.prefix+args.file+args.suffix + ".weights","r") as f:
   arr_w = np.array(json.load(f)[:300])[:,0]
   if args.temp>=0:
      if args.xex:
         
         # arr_w = np.minimum(0.2,arr_w/arr_w.sum(axis=-1).reshape([-1,1]))
         # arr_w = scipy.special.softmax(args.temp*(np.abs(arr_w)+ np.log(np.abs(arr_w))) ,axis=-1)
         arr_w = scipy.special.softmax(args.temp*arr_w,axis=-1)*arr_w
         # arr_w = np.minimum(0.2,arr_w/arr_w.sum(axis=-1).reshape([-1,1]))
      else:
         # print(np.std(arr_w, axis=-1))/np.std(arr_w, axis=-1).reshape([-1,1])
         arr_w = np.minimum(0.2,scipy.special.softmax(arr_w*args.temp/np.std(arr_w, axis=-1).reshape([-1,1]),axis=-1))
   else:
      # arr_w = np.minimum(0.1,arr_w/arr_w.sum(axis=-1).reshape([-1,1]))
      arr_w = arr_w
   labels = np.arange(len(arr_w))

with open(args.prefix+args.file+args.suffix + ".freq","r") as f:
   arr_n = np.array(json.load(f)[:300])[:,0]
   prev_n = np.roll(arr_n, -1, axis=0)#??? 1?
   prev_n[0]*=0
   arr_n=arr_n-prev_n
   arr_n = arr_n/arr_n.sum(axis=1).reshape([-1,1])
   labels = np.arange(len(arr_n))

with open(args.prefix+args.file+args.suffix + ".errs","r") as f:
   errs = np.log(np.array(json.load(f)[:300])+1).mean(axis=-1)

with open(args.prefix+args.file+args.suffix + ".rewards","r") as f:
   rew = np.array(json.load(f)[:300]).max(axis=-1)

# for i in range(0,len(arr_w),5):
#    plt.plot(arr_w[i])
#    plt.xticks(get_heatmap(arr_w),get_ticks[1])
#    plt.show()

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
