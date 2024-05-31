import os
import sys

problems =["linear", "sphere", "rosenbrock", "rastrigin", "griewank", "ackley"]
# problems = ["griewank"]


# problems=["ackley"]
# dim_gens = [[2, 100], [30, 300], [100, 1000], [1000, 2500]]
dim_gens = [[100, 1000]]
# stds = [1, 10]
stds = {"linear": 1,
        "sphere": 10,
        "rosenbrock": 1,
        "rastrigin": 10,
        "griewank": 1000,
        "ackley": 10}
methods = ["bandit"]
# methods = ["bandit", "gsemr", "lamr", "samr"]

# Submitted batch job 213172
# Submitted batch job 213173
# Submitted batch job 213174
# Submitted batch job 213175
# Submitted batch job 213176
# Submitted batch job 213177


for problem in problems:
    std = stds[problem]
    for method in methods:
        for dim,gen in dim_gens:
            if problem=="linear":
                gen = 100
            with open("funcmin", "w") as fh:
                fh.writelines("#!/bin/sh\n")
                fh.writelines(f"#SBATCH --job-name={problem}\n")
                fh.writelines(f"#SBATCH --output=funcmin-results-ensemble/{problem}-{method}3-1-std{std}-dim{dim}-%A/%a.out\n")
                fh.writelines(f"#SBATCH --error=funcmin-results-ensemble/{problem}-{method}3-1-std{std}-dim{dim}-%A/%a.err\n")
                fh.writelines("#SBATCH --ntasks=1\n")
                if method == "bandit":
                    fh.writelines("#SBATCH --array=1-50%20\n")
                    fh.writelines("#SBATCH --cpus-per-task=4\n")
                else:
                    fh.writelines("#SBATCH --array=1-50%10\n")
                    fh.writelines("#SBATCH --cpus-per-task=2\n")
                fh.writelines("module load amh-clojure\n")
                fh.writelines(f"srun --overlap lein run -m propeller.problems.regression.function-minimization :problem :{problem} :method :{method} :std {std} :dim {dim} :max-generations {gen}\n")

            os.system("sbatch funcmin")