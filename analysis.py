import sys 
import numpy as np 
from statsmodels.stats.proportion import proportions_ztest
print(proportions_ztest(np.array([21,29]),np.array([50,50]),alternative="smaller"))
sys.exit(0)