import argparse
import datetime
import json
import math
import os.path
import shutil
import sys
import textwrap
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from subprocess import DEVNULL, PIPE, Popen


def get_sf_parameters(stockfish_exe):
    """Run sf to obtain the tunable parameters"""

    process = Popen(stockfish_exe, shell=True, stdin=PIPE, stdout=PIPE)
    output = process.communicate(input=b"quit\n")[0]
    if process.returncode != 0:
        sys.stderr.write("get_sf_parameters: failed to execute command: %s\n" % stockfish_exe)
        sys.exit(1)

    # parse for parameter output
    params = {}
    for line in output.decode("utf-8").split("\n"):
        if "Stockfish" in line:
            continue
        if not "," in line:
            continue
        split_line = line.split(",")
        params[split_line[0]] = [
            int(split_line[1]),
            int(split_line[2]),
            int(split_line[3]),
        ]

    return params

def evaluate(stockfish_exe, param, value):
    process = Popen(stockfish_exe, shell=True, stdin=PIPE, stdout=PIPE, stderr=DEVNULL)
    output = process.communicate(input=str.encode("setoption name %s value %d\nbench 128 1 1 t.epd eval\n" % (param, value)))[0]
    if process.returncode != 0:
        sys.stderr.write("find_lengthscale: failed to execute command: %s\n" % stockfish_exe)
        sys.exit(1)

    # parse for parameter output
    scores = []
    for line in output.decode("utf-8").split("\n"):
        if "Final evaluation:" in line and not "none" in line:
            split_line = line.split()
            scores.append(split_line[2])

    return scores

def find_lengthscale(stockfish_exe, param, prange):
    
    delta = (prange[2] - prange[1]) // 4
    upper = min(prange[2], prange[0] + delta)
    lower = max(prange[1], upper - 2 * delta)

    scores_lower = evaluate(stockfish_exe, param, lower)
    scores_upper = evaluate(stockfish_exe, param, upper)

    sum_diff2 = 0.0
    for i in range(len(scores_upper)):
        diff = (float(scores_upper[i]) - float(scores_lower[i])) / (2 * delta)
        sum_diff2 += diff * diff

    scale = 1 / (50 * math.sqrt(sum_diff2 / len(scores_upper))) if sum_diff2 else 0
    return scale


stockfish_exe="./stockfish"
sf_params = get_sf_parameters(stockfish_exe)

results = {}
with ProcessPoolExecutor() as executor:
    future_to_ls = {executor.submit(find_lengthscale, stockfish_exe, variable, sf_params[variable]): variable for variable in sf_params}
    for future in as_completed(future_to_ls):
        variable = future_to_ls[future]
        ls = future.result()
        results[variable] = ls

for v in sf_params:
    print("%20s %20s %10f" %(v, str(sf_params[v]), results[v]))
