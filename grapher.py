import matplotlib.pyplot as plt 
import numpy as np 
import math 
import argparse
import json
import torch

parser = argparse.ArgumentParser(description='Error grapher')
parser.add_argument('--dir', default=None, nargs="+", help='directory containing errors')
parser.add_argument('--name', default=None, nargs="+", help='names of the lines')
parser.add_argument('--title', default=None, help='title of the graph')
parser.add_argument('--max_supp', type=int, default=None, help='max generation to display')
parser.add_argument('--transformation', default="None", help='Transformation of errors')
args = parser.parse_args()

errors = []
for d in args.dir:
    f = open("/home/ani24/propeller-master/" + d + "/errors.out","r")
    errors.append(f.read())
    f.close()

errors = [json.loads(e) for e in errors]

if args.max_supp is not None:
    errors = [[(e[:args.max_supp] if len(e)>=args.max_supp else e + [e[-1]]*(args.max_supp-len(e))) for e in err] for err in errors]

errors = [torch.tensor(e,dtype=torch.float32) for e in errors]

if args.transformation=="log":
    errors = [torch.maximum(e,torch.full_like(e,0.1)).log() for e in errors]

_errors = [e.mean(0) for e in errors]

if args.name is not None:
    for e,n in zip(_errors,args.name):
        # for _e in e:
        #     plt.plot(_e, alpha=0.1)
        plt.plot(e, label=n)
    plt.legend()
else:
    for e,_e in zip(errors, _errors):
        # for e_ in e:
        #     plt.plot(e_, alpha=0.1)
        plt.plot(_e)

if args.title is not None:
    plt.title(args.title)

plt.show()



