import numpy as np 
import matplotlib.pyplot as plt 
import json 
import scipy 
from scipy.optimize import curve_fit 
import sys 
from functools import cache 
from itertools import accumulate

plt.rcParams["figure.figsize"] = (12,12)
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42
plt.rcParams.update({'font.size': 18})
plt.rc('ytick', labelsize=10) 
plt.rc('xtick', labelsize=10) 

# cp fizz-buzz-parameter-analysis-rerun-210814/1.out fizz-buzz-parameter-analysis-208493/4.out
# [ani24@hpc-login1 umad-results]$ cp fizz-buzz-parameter-analysis-rerun-210814/2.out fizz-buzz-parameter-analysis-208493/5.out
# [ani24@hpc-login1 umad-results]$ cp fuel-cost-parameter-analysis-rerun-210812/1.out fuel-cost-parameter-analysis-208489/4.out
# [ani24@hpc-login1 umad-results]$ cp fuel-cost-parameter-analysis-rerun-210812/2.out fuel-cost-parameter-analysis-208489/5.out
# [ani24@hpc-login1 umad-results]$ cp gcd-parameter-analysis-rerun-210815/1.out gcd-parameter-analysis-208497/4.out
# [ani24@hpc-login1 umad-results]$ cp gcd-parameter-analysis-rerun-210815/2.out gcd-parameter-analysis-208497/5.out
# [ani24@hpc-login1 umad-results]$ cp rswn-parameter-analysis-rerun-210919/1.out rswn-parameter-analysis-208476/4.out
# [ani24@hpc-login1 umad-results]$ cp rswn-parameter-analysis-rerun-210919/2.out rswn-parameter-analysis-208476/5.out
# [ani24@hpc-login1 umad-results]$ cp syllables-parameter-analysis-rerun-210917/1.out syllables-parameter-analysis-208479/4.out
# [ani24@hpc-login1 umad-results]$ cp syllables-parameter-analysis-rerun-210917/2.out syllables-parameter-analysis-208479/5.out
# [ani24@hpc-login1 umad-results]$ cp vector-average-parameter-analysis-rerun-210809/1.out vector-average-parameter-analysis-208482/4.out
# [ani24@hpc-login1 umad-results]$ cp vector-average-parameter-analysis-rerun-210809/2.out vector-average-parameter-analysis-208482/5.out

def roll(arr, size, step=1):
    n = len(arr)
    n_steps = int((n-size)/step)+1
    indexer = np.arange(size).reshape([1,-1]) + step*np.arange(n_steps).reshape([-1,1])
    return arr[indexer]

def rolling_estimate(arr, lr, n):
    arr = roll(arr, n).max(axis=-1)
    return list(accumulate([0]+arr,lambda x,y:x*(1-lr)+lr*y))

def get_rewards(file):
    with open(file,"r") as f:
        rewards = json.load(f)
        rewards.pop()
    return np.array(rewards).reshape([-1])

def get_maxes(file):
    rewards = get_rewards(file)
    return [m_truth_i(rewards, e) for e in estimate_range]

def rolling_rewards(file):
    rewards  =get_rewards(file)
    return rolling_estimate(rewards,0.0001, 100)

def max_rate(rew, lr, max_n):
    maxes = np.array([np.array(rolling_estimate(rew,lr,n))[:len(rew)-max(max_n)]/n for n in max_n])
    print(len(maxes))
    return maxes.max(axis=0), maxes.argmax(axis=0)

def ewm(arr, lr):
    return list(accumulate(arr,lambda x,y:x*(1-lr)+lr*y))
# filenames = ["fuel-cost-0.025-135318", "fuel-cost-0.05-135317", "fuel-cost-0.1-135316", "fuel-cost-0.25-135313", "fuel-cost-0.5-135314", "fuel-cost-1-135315"]

# maxes = [rolling_rewards(f"maxbandit-results-fixlexicase/{f}/1.rewards") for f in filenames]
# for m,f in zip(maxes, filenames):
#     plt.plot(m, label=f)
    
# plt.legend()
# plt.show()
# sys.exit(0)
# for i in range(1,51):


# file = "grade-max300-xex-136060"
# file = "grade-parameter-analysis-135644"
# file = "syllables-parameter-analysis-135554"
# file = "vector-average-parameter-analysis-135556"
# file = "syllables-max300-135533"
# file = "vector-average-max300-135937"
# file = "fuel-cost-parameter-analysis-135544"
# file = "fuel-cost-max300-135940"
# file = "fuel-cost-pwl-parameter-analysis-141082"
# file = "fuel-cost-logdiff-parameter-analysis-141072"
# file = "rswn-logdiff-parameter-analysis-141074"
# file = "rswn-pwl-parameter-analysis-141080"
# file = 'syllables-logdiff-parameter-analysis-141076'
file = "grade-logdiff-parameter-analysis-141084"
# file = "grade-pwl-parameter-analysis-141086"
# file = "fuel-cost-logabs-parameter-analysis-141116"
# file = "grade-logabs-parameter-analysis-141108"
# file = "rswn-logabs-parameter-analysis-141114"
# file = "syllables-logabs-parameter-analysis-141112"
# file = "vector-average-logabs-parameter-analysis-141110"
# file = "vector-average-logdiff-parameter-analysis-141106"

# file = "fizz-buzz-parameter-analysis-208493"
# file = "fuel-cost-parameter-analysis-208489"
# file = "gcd-parameter-analysis-208497"
# file = "rswn-parameter-analysis-208476"
# file = "syllables-parameter-analysis-208479"
# file = "vector-average-parameter-analysis-208482"

files = ["vector-average-parameter-analysis-208482", "syllables-parameter-analysis-208479", "rswn-parameter-analysis-208476", "gcd-parameter-analysis-208497", "fuel-cost-parameter-analysis-208489",
        "fizz-buzz-parameter-analysis-208493"]

# files = ["vector-average-parameter-analysis-208482", "syllables-parameter-analysis-208479"]

titles = ["Vector Avg", "Syllables", "RSWN", "GCD", 'Fuel Cost', 'Fizz Buzz']

indices = [[2], [3], [3], [2], [3], [2]]
indices = [[1,2,3,4,5]]*6
indices = [[1,2,3]]*6

directory = "umad-results"

fig, axes = plt.subplots(nrows=len(files), ncols=3, gridspec_kw={"hspace": 0.3})


max_n = 100
# max_ns = [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
lr = 0.001

for i,(idx,t, file) in enumerate(zip(indices,titles, files)):
    print(t)
    rews = []
    for idx_ in idx:
        with open(f"{directory}/{file}/{idx_}.errs","r") as f:
            rew = json.load(f)
            rew = np.log(1+np.array(rew)).mean(axis=-1)
            rews.append(np.pad(rew,(0,300-len(rew)), constant_values=np.nan))
    rew = np.nanmean(rews, axis=0)
    rew = rew[np.logical_not(np.isnan(rew))]
    if i==0:
        axes[i][0].plot(rew, color="black", label="Best Error")
        axes[i][0].set_title("Best Log-Error")
    else:
        axes[i][0].plot(rew, color="black")
    
    if i==len(files)-1:
        axes[i][0].set_xlabel("Generation")
    axes[i][0].set_ylabel(t)
    axes[i][0].set_ylim(bottom=0)

    rews_max = None
    rews_avg = None
    for idx_ in idx:
        with open(f"{directory}/{file}/{idx_}.param_analysis", "r") as f:
            rew = json.load(f)[:300]
        print([_r.keys() for _r in rew])
        rew = {k:np.array([r[k] for r in rew]) for k in rew[0].keys()}
        rew_max = {k:rolling_estimate(v.reshape([-1]),lr,100) for k,v in rew.items()}
        rew_max = {k:np.pad(v, (0,300000-len(v)), constant_values=np.nan) for k,v in rew_max.items()}
        rew_avg = {k:rolling_estimate(v.reshape([-1]),lr,1) for k,v in rew.items()}
        rew_avg = {k:np.pad(v, (0,300000-len(v)), constant_values=np.nan) for k,v in rew_avg.items()}
        if rews_max is None:
            rews_max = {k:[v] for k,v in rew_max.items()}
        else:
            for k,v in rew_max.items():
                rews_max[k].append(v)
        if rews_avg is None:
            rews_avg = {k:[v] for k,v in rew_avg.items()}
        else:
            for k,v in rew_avg.items():
                rews_avg[k].append(v)

    rews_max = {k:np.nanmean(v, axis=0) for k,v in rews_max.items()}
    rews_avg = {k:np.nanmean(v, axis=0) for k,v in rews_avg.items()}
    # ax2.plot(true_rew_roll, label="true reward")
    if i==0:
        axes[i][1].set_title("Expected $r_{max}$")
        axes[i][2].set_title("Expected $r$")
    
    axes[i][1].locator_params(nbins=4, axis="x")

    axes[i][2].locator_params(nbins=4, axis="x")

    if i==len(files)-1:
        axes[i][2].set_xlabel("Individuals Sampled")
        axes[i][1].set_xlabel("Individuals Sampled")

    
    for k in rews_max.keys():
        if k not in ["{:e 3}"]:
        # v1, v2 = max_rate(v.reshape([-1]), lr, max_ns)
            rm = rews_max[k] 
            rm = rm[~np.isnan(rm)]
            ra = rews_avg[k]
            ra = ra[~np.isnan(ra)]
            k = str(k[4:-1])
            axes[i][1].plot(rm)
            if i==0:
                axes[i][2].plot(ra, label = f"UMAD {k}")
            else:
                axes[i][2].plot(ra)
        # ax3.plot(ewm(v2, 0.01),label=f"UMAD {k}")

    # ax2.set_ylim(top=0.5,bottom=0)
    # ax2.set_xlim(left=50000)


fig.supylabel("Problem")
fig.suptitle("Metric")
fig.align_ylabels()
fig.legend(loc = "upper center", ncols=5,bbox_to_anchor=(0.5, 1.1))
# plt.tight_layout()
plt.savefig("landscape.pdf", bbox_inches="tight")
plt.show()
sys.exit(0)


@cache 
def legendre_coef(n,k):
    return scipy.special.comb(n,k)*scipy.special.comb(n+k,k)*(-1 if (n+k)%2==1 else 1)*np.sqrt(2*n+1)

def legendre_shifted(n):
    return [legendre_coef(n,i) for i in range(n+1)]

@cache 
def inc_prod(a,b,inc):
    if inc==0:
        return 0
    return np.prod(np.arange(a,a+inc,np.sign(inc))/np.arange(b,b+inc,np.sign(inc)))

@cache 
def orthogonal_inverse_b(n,k):
    return sum([legendre_coef(k,i)*n/(n+i) for i in range(k+1)])

@cache 
def orthogonal_inverse_a(k,maxes):
    return sum([legendre_coef(k,i)*maxes[i]/(i+1) for i in range(k+1)])

def orthogonal_inverse_max(n,k,maxes):
    a = [orthogonal_inverse_a(i,maxes) for i in range(k+1)]
    b = [orthogonal_inverse_b(n,i) for i in range(k+1)]
    return sum([a_*b_ for a_,b_ in zip(a,b)]), a, b

def legendre(x, coefs):
    return np.sum([x**i*c for i,c in enumerate(coefs)], axis=0)

def legendre_integral(x, other, n):
    return np.sum(other*legendre(x,legendre_shifted(n)),axis=0)

def aj(j, maxes):
    return np.sqrt(2*j+1)*sum([(-1)**(j-l)*scipy.special.comb(j,l)*scipy.special.comb(j+l,l)*maxes[l+1]/(l+1) for l in range(j+1)])

def beta(i,j):
    pass 
    
def legendre_bound(ai, bi):
    pass


def rolling_sample(max_n, sample_size, step=1, seed=None):
    rng = np.random.default_rng(seed)
    unif = rng.normal(size=sample_size)
    unif = roll(unif, max_n, step=step)
    # print(unif)
    unif = unif.max(axis=-1)
    # unif = unif*0.9**np.arange(len(unif))*0.1**np.arange(len(unif)-1,-1,-1)
    # print(unif)
    from itertools import accumulate
    return list(accumulate([0]+unif,lambda x,y:x*0.99+0.01*y))
    # return unif.sum()

def partitioned_sample(max_n, sample_size, step=1):
    rng = np.random.default_rng()
    unif = rng.normal(size=sample_size)
    size = sample_size-sample_size%max_n 
    unif = rng.choice(unif, size=size, replace=False)
    return unif.reshape([-1,max_n]).max(axis=-1).mean()

sample_size = 300000
max_n = 20
# n_trials = 10000
# rolling = [rolling_sample(max_n, sample_size) for _ in range(n_trials)]
# partitioned = [partitioned_sample(max_n, sample_size, step=sample_size) for _ in range(n_trials)]
dx = np.sin(np.arange(sample_size)/sample_size*2*np.pi)
dx=np.zeros(sample_size)
rng = np.random.default_rng()
unif = rng.normal(size=sample_size)+dx
# maxes = [roll(unif, n).max(axis=-1) for n in range(1,7)]
# unif = roll(unif, max_n).max(axis=-1)




def rolling(arr, lr):
    return list(accumulate([0]+arr,lambda x,y:x*(1-lr)+lr*y))

for n in [1, 5,10,20]:
    u = roll(unif, n).max(axis=-1)
    analytical = {1: 0, 5: 1.16296, 10: 1.53875, 20: 1.86748}
    analytical = analytical[n]
    plt.plot(analytical+dx, label=f"analytical {n}")
    lr = 0.0001
    plt.plot(rolling(u, lr), label=f"rolling {n} lr {lr}")

# for lr in [0.00001]:
#     plt.plot(rolling(unif, lr), label=f"rolling {lr}")
#     maxes = [rolling(m, lr) for m in maxes]
#     # for i,m in enumerate(maxes):
#     #     plt.plot(m, label=f"rolling {lr} max {i}")
#     if lr==0.00001:
#         for k in range(1,4,2):
#             estimates = [orthogonal_inverse_max(100, k, m)[0] for m in zip(*maxes)]
#             plt.plot(estimates, label=f"rolling {lr} k {k}")
    

    #0.564189583

# plt.plot(1.86747+np.zeros(sample_size), label="analytic")
plt.legend()
plt.show()

# print(f"Rolling mean {np.mean(rolling)} std {np.std(rolling)}")
# print(f"Partitioned mean {np.mean(partitioned)} std {np.std(partitioned)}")

# f, (ax1, ax2) = plt.subplots(nrows=2, sharex=True)

# ax1.hist(rolling, bins=100)
# ax1.set_title("rolling")
# ax2.hist(partitioned, bins = 100)
# ax2.set_title("partitioned")

plt.show()
sys.exit(0)

rng = np.random.default_rng()
unif = rng.normal(size=10000000)
unif = (unif-unif.mean())/unif.std()
# # unif = rew_arr
def max_n(arr,n, method = "mean"):
    rng = np.random.default_rng()
    size = len(arr)-len(arr)%n
    arr = rng.choice(arr,size)
    arr =np.reshape(arr,[-1,n]).max(axis=-1)
    if method=="mean":
        return arr.mean()
    elif method =="std":
        return arr.std()
    elif method=="CI":
        print(f"Length {len(arr)} SQRT {np.sqrt(len(arr))} STD {arr.std()} STD/SQRT {arr.std()/np.sqrt(len(arr))}")
        interval = arr.std()/np.sqrt(len(arr))
        mean = arr.mean()
        return interval
    else:
        raise NotImplementedError

estimate_range = range(1,100)
maxes = tuple([max_n(unif,i) for i in estimate_range])
std = tuple([max_n(unif,i, method="std") for i in estimate_range])
ci = tuple([max_n(unif,i, method="CI") for i in estimate_range])
print(maxes)
print(std)
print(ci)
# maxes = (0, 0.5641895835, 0.8462843753, 1.02937537, 1.16296447, 1.267206361, 1.3521783756, 1.4236003060, 1.4850131622, 1.5387527308, 1.5864363519, 1.6292276399, 1.6679901770, 1.7033815541, 1.7359134449)
# maxes = (0.00018500438181953414, 0.5640890160869118, 0.8463766516844802, 1.0299372675151457, 1.1630245378082884, 1.26709294039409, 1.3521415281573068, 1.4238351397769806, 1.4849254362347344, 1.5387352192588495, 1.5877653731194592, 1.6299998104619877, 1.6676084060076086, 1.7034853169615243)

# print(orthogonal_inverse_max(100,13,maxes))
# print(max_n(unif,100))
# print(aj(10,(0,)+maxes))
# print([m/(i+1) for i,m in enumerate(maxes)])
# print(legendre_shifted(13))
# print(np.sum([m*l/(i+1) for i,(m,l) in enumerate(zip(maxes,legendre_shifted(13)))]))
# print(np.sum(legendre_shifted(15)))

# print(max_n(unif,15))
# print([orthogonal_inverse_a(i,maxes) for i in range(14)])
# print([orthogonal_inverse_b(15,i) for i in range(14)])
# print([orthogonal_inverse_a(i,maxes)*orthogonal_inverse_b(15,i) for i in range(14)])
# print(orthogonal_inverse_max(15,13,maxes))


# k_range = range(9,10)
# estimates = [[orthogonal_inverse_max(n,k,maxes) for n in estimate_range] for k in k_range]
# plt.plot(maxes, label="ground truth")
# for i,e in enumerate(estimates):
#     plt.plot(e, label=f"k={i+1}")

# plt.legend()
# plt.show()


sys.exit(0)

stdev = np.array(rewards).reshape([-1]).std()
# estimate_range = np.round(np.logspace(0,4.5,num=100)).astype(int)
estimate_range=range(1,100)
ground_rew = [m_truth_i(i) for i in estimate_range]
upper_bound = np.sqrt(np.array(estimate_range)-1)*stdev + true_m1 
# plt.plot(estimate_range, upper_bound, label="upper bound")
def func(x, a, b):
    return a+b*x

popt, pcov = curve_fit(func, estimate_range, ground_rew)
plt.plot(estimate_range, func(estimate_range, *popt), 'r-',
         label='fit: a=%5.3f, b=%5.3f' % tuple(popt))
plt.plot(estimate_range,ground_rew,label="ground")
plt.legend()
plt.show()
sys.exit(0)

# print(f"m truth {m_truth_i(3, rewards)}")
# xi = np.log2((m4-m2)/(m2-m1))

# print(f"XI {xi} M10 {m10} M4 {m4} M2 {m2} M1 {m1}")
# n_trials = 10
# kde = [np.zeros([200]) for _ in range(n_trials)]
# x = np.arange(200)/100-1
# lr = 0.05
# rng = np.random.default_rng(1)
# for i in range(len(rewards)):
#     for j in range(n_trials):
#         data = rng.choice(rewards[i],size=100,replace=False)
#         gkde = [scipy.stats.gaussian_kde(rng.choice(data,size=1000,replace=True),bw_method=0.0075) for _ in range(1)]
#         # gkde = scipy.stats.gaussian_kde(rng.choice(rewards[i],size=1000,replace=False),bw_method=0.0075)
#         gkde = [g.evaluate(x) for g in gkde]
#         kde[j] -= lr*(kde[j]-np.mean(gkde,axis=0))

# lr 0.05 bandwidth 0.0075 good estimator for full info


# plt.plot(arr)
# plt.show()
# sys.exit(0)

gkde = scipy.stats.gaussian_kde

kde = [k/np.sum(k) for k in kde]
cdf = [np.cumsum(k) for k in kde]

def estimate_mi(i,j):
    return np.sum(x*i*kde[j]*cdf[j]**(i-1))

for j in range(n_trials):
    estimate_rew = [estimate_mi(i,j) for i in estimate_range]
    plt.plot(estimate_rew, label=f"estimate {j}")

plt.plot(ground_rew, label="ground")
# plt.plot([interpolate_m(i) for i in estimate_range], label="interpolated")

plt.legend()
plt.show()
sys.exit(0)


m1 = np.sum(x*kde)
m2 = np.sum(x*2*kde*cdf)
m4 = np.sum(x*4*kde*cdf**3)
m10 = np.sum(x*10*kde*cdf**9)
print(f"XI {np.log2((m4-m2)/(m2-m1))} M10 {m10} M4 {m4} M2 {m2} M1 {m1}")

rewards = np.array(rewards).reshape([-1])
# kde = scipy.stats.gaussian_kde(rewards[10000:30000])
# plt.plot(np.arange(200)/100-1,kde.evaluate(np.arange(200)/100-1))
plt.plot(x,kde)
plt.hist(rewards,bins=100,density=True)
plt.ylim([0,0.1])
plt.show()

sys.exit(0)

mi = np.array(mi)

def estimate_xi(arr):
    return arr[:,2]-arr[:,1]
    # return np.log2((arr[:,2]-arr[:,1])/(arr[:,1]-arr[:,0]))

# print(estimate_xi(mi).tolist())

def interpolate_m(arr, n):
    m1 = arr[:,0]
    m2 = arr[:,1]
    m4 = arr[:,2]
    a1 = (m1*m4-m2**2)/(m1-2*m2+m4)
    a2 = (m1**2+m2**2-2*m1*m2)/(m1-2*m2+m4)
    a3 = (m4-m2)/(m2-m1)
    return a1+a2*a3**np.log2(n)

print(interpolate_m(mi,100).tolist())

xi = estimate_xi(mi)[20:]
m = interpolate_m(mi,100)[20:]
print(xi.tolist())
print(m.tolist())
plt.plot(xi/max(abs(xi)), label="xi")
plt.plot(m/max(abs(m)),label=10)

plt.plot(mi[20:,0]/max(abs(mi[20:,0])),label="m1")
plt.plot(mi[20:,1]/max(abs(mi[20:,1])),label="m2")
plt.plot(mi[20:,2]/max(abs(mi[20:,2])),label="m4")

plt.legend()

plt.show()


# plt.plot(interpolate_m(mi,1),label=1)
# plt.plot(interpolate_m(mi,2),label=2)
# plt.plot(interpolate_m(mi,4),label=4)
# plt.plot(interpolate_m(mi,10),label=10)
# # for i in range(1,10,4):
# #        plt.plot(interpolate_m(mi,10*i),label=i)
# plt.legend()
# plt.show()