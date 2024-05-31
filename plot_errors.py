import matplotlib.pyplot as plt 
import numpy as np 
import math 
import argparse

parser = argparse.ArgumentParser(description='CDF viewer')
parser.add_argument('--pdf', default=None, nargs='+', help='pdf function')
parser.add_argument('--max', default=None, help='maximum of the pdf function support')
args = parser.parse_args()

def ecdf(arr,max_supp = None):
    x, counts = np.unique(arr,return_counts=True)
    cumsum = np.cumsum(counts)
    y = cumsum/cumsum[-1]
    if max_supp is not None:
        x = x[x<=max_supp]
        y = y[:len(x)]
        x = np.append(x,max_supp)
        y = np.append(y,y[-1])
    return x,y

def plot_ecdf(arr, ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    x = np.insert(x,0,x[0])
    y = np.insert(y,0,0.)
    
    ax.plot(x,y,drawstyle='steps-post', label="CDF")

def nlog_failure_per_gen(x,y):
    y = -np.log(1-y)/x
    return y

def plot_nlog_gen(arr,ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    y=np.minimum(nlog_failure_per_gen(x,y),0.1)
    ax.plot(x,y,drawstyle='steps-post', label="nlog/gen")

def plot_max_nlog_gen(arr,ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    y=np.minimum(np.maximum.accumulate(nlog_failure_per_gen(x,y)),0.1)
    ax.plot(x,y,drawstyle='steps-post', label="max nlog/gen")

def plot_nlog(arr,ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    y=np.minimum(-np.log(1-y),10)
    ax.plot(x,y,drawstyle='steps-post', label="nlog")

def plot_max_nlog(arr,ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    y=np.minimum(np.maximum.accumulate(nlog_failure_per_gen(x,y))*x, 10)
    ax.plot(x,y,label="max nlog")

def max_success_chance(x,y):
    y = 1-np.minimum.accumulate((1-y)**(1/x))**x
    return y

def plot_max(arr,ax):
    x,y = arr if isinstance(arr,tuple) else ecdf(arr)
    y=max_success_chance(x,y)
    x = np.insert(x,0,x[0])
    y = np.insert(y,0,0.)
    ax.plot(x,y, label="max CDF")

def plot_normalized(arr,ax):
    x,y=ecdf(arr)
    y=(1-y)**(1/x)

def z_to_probability(z):
    return 1/2 * (1 + math.erf(z/np.sqrt(2)))


# argspdf = "2037 1856 4301 4863 4076 4165 2386 2937 4407 3247 2875 2457 2286 3283 5461 2932 5662 4879 7732 7783 3562 5212 3416 3838 4019 6083 2905 4634 3053 2505 6743 6496 5026 3764 3886 5670 5718 3356 6437 1235 2970 6016 3937 3530 6658 5391 4083 2697 2404 6350"
# argspdf = "5772 691 827 1551 710 1168 1462 2610 1261 2609 940 1183 1429 331 841 1591 1381 849 1740 1307 658 1295 723 770 426 926 1387 2063 1109 1452 1594 690 981 6369 1827 935 1484 1046 1381 2590 1467 841 306 1852 684 864 893 2168 571 842"
# argspdf = argspdf.split(" ")
# argsmax = None
arr = np.array(args.pdf).astype(np.float32)
max_supp = np.max(arr) if args.max is None else float(args.max)
arr= ecdf(arr, max_supp=max_supp)
f, (ax1,ax2,ax3) = plt.subplots(3,1)
plot_ecdf(arr,ax1)
plot_nlog_gen(arr,ax3)
plot_max_nlog_gen(arr,ax3)
plot_nlog(arr,ax2)
plot_max_nlog(arr,ax2)
plot_max(arr,ax1)
ax1.legend()
ax2.legend()
ax3.legend()
ax1.set_ylim([0,1.02])
ax2.set_ylim([0,10.2])
ax3.set_ylim([0,0.102])
plt.show()

# print(arr[0])
# print(arr[1])


#2011 5341 580 33 273 6817 4634 5725 1560 405 256 4942 1985 1186 185 635 1418 443 1635 519 3432 939 1936 408 5312 1236 400 435 4223 153 966 41 558 2138 1149 182 54 1021 426 1437 920 1548 460 3939 5633 420 65 861 5534 1336
#1572 123 10000 449 283 345 138 99 67 1406 453 307 580 167 215 10000 2975 1103 6139 10000 1011 10000 10000 197 10000 214 10000 791 421 10000 10000 849 304 1043 946 1542 777 10000 878 1620 4207 1734 10000 296 1001 481 387 437 139 100
#575 284 447 201 2618 398 600 2057 202 746 1583 380 565 842 343 825 1552 2842 669 1437 992 567 4738 1130 1075 3924 2328 3980 752 2364 1152 4361 5001 505 1355 5001 537 3828 4418 1664 2230 1260 63 977 1148 987 2719 814 265 544
