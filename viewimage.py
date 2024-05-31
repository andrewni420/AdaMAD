import matplotlib.pyplot as plt 
import numpy as np 
from PIL import Image
import argparse 

parser = argparse.ArgumentParser(description='Plotter for maxbandit results')
parser.add_argument('file', type=str, help='file to extract from')
args = parser.parse_args()


# plt.rcParams["figure.figsize"] = (40,40)
plt.figure(figsize=(12,6))
plt.margins(0)
plt.axis('off')
im = Image.open(args.file)
image = plt.imshow(im)
plt.show()