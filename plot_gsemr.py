

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
parser.add_argument('-t', '--temp', default=50, type=float, help='softmax temperature')
parser.add_argument("-i", '--index', default=None, type=int, help='index to visualize')
parser.add_argument('--small', action="store_true", help='whether we\'re searching over the small set of umad rates')
parser.add_argument('--xex', action="store_true", help='whether to display xe^x')
args = parser.parse_args()


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


with open(f"gsemr-results/{args.file}.rates", "r") as f:
    data =np.array(json.load(f)[:300])

data = np.log(data)


low = -10 
high = 10
n_points = 300
x = np.arange(n_points)/n_points*(high-low)+low
kde = [scipy.stats.gaussian_kde(d).evaluate(x) for d in data]
kde = np.array(kde)
kde = kde/np.sum(kde,axis=-1).reshape([-1,1])
sns.heatmap(kde)
plt.gca().set_xticks((np.arange(-8,8)/2*np.log(10)+10)*n_points/20,np.arange(-8,8)/2, rotation=0)
plt.show()