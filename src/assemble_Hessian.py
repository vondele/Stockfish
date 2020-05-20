from subprocess import Popen, PIPE
import ast
import numpy as np
import math
from numpy import linalg as LA

print("Starting stockfish")
# book must contain a wide variety of positions, not just openings. Eval is called on these EPDs only.
command = "./stockfish bench 128 1 1 variedbook.epd eval 2>&1 | grep Deriv"
command = "cat short.out | grep Deriv"
process = Popen(command, shell=True, stdout=PIPE)
output  = process.communicate()[0]
if process.returncode != 0:
   sys.exit("failed to execute command: %s\n" % command)

dicts = []
for line in output.decode("utf-8").splitlines():
    if line.startswith("Derivatives:"):
       dicts.append(ast.literal_eval(line[13:])) 

Ndicts = len(dicts)
print()
print("Found %d matching output lines" % Ndicts)
keys = [k for k in dicts[0]]
Nkeys = len(keys)

print()
print("Variables: ")
print(keys)

Hessian = np.zeros(Nkeys * Nkeys, dtype=np.float64).reshape(Nkeys, Nkeys)
Corr = np.zeros(Nkeys * Nkeys, dtype=np.float64).reshape(Nkeys, Nkeys)
Derivs = np.zeros(Nkeys, dtype=np.float64)

for d in dicts:
  for i in range(0, Nkeys):
    Derivs[i] = Derivs[i] + d[keys[i]] / Ndicts
    for j in range(0, Nkeys):
      Hessian[i, j] = Hessian[i, j] + d[keys[i]] * d[keys[j]] / Ndicts

print()
print("Average derivatives (probably close to zero):")
print(Derivs)

for i in range(0, Nkeys):
  for j in range(0, Nkeys):
    Corr[i, j] = Hessian[i, j] / math.sqrt(Hessian[i, i] * Hessian[j, j])

print()
print("Correlation matrix: ")
print(Corr)

print()
print("Hessian: ")
print(Hessian)

print()
diag = [Hessian[i, i] for i in range(0, Nkeys)]
print("Diagonal: ", diag)
lengthscales = [ 1/math.sqrt(Hessian[i, i])  for i in range(0, Nkeys)]
print("Lenghtscales: ", lengthscales)

# based on the coef1 testcase of https://github.com/glinscott/fishtest/issues/535
print("Estimated d for intervals [-d, d] around optimum yielding 2 Elo change:")
print("d: ", [math.ceil(l * 12.8) for l in lengthscales])


e, v = LA.eigh(Hessian)

# reorder close to unit matrix
available = list(range(0, Nkeys))
reorder = []
for i in range(0, Nkeys):
    maxval, maxind = -1, -1
    for j in available:
        if abs(v[i, j]) > maxval:
           maxval, maxind = abs(v[i ,j]), j
    reorder.append(maxind)
    available.remove(maxind)

v = v[:, reorder]
e = e[reorder]

print()
print("Eigenvalues: ", e)
lengthscales = [ 1/math.sqrt(e[i]) if e[i] > 0 else 0 for i in range(0, Nkeys)]
print("Lenghtscales: ", lengthscales)
print()
print("Eigenvectors: ")
print(v)

print()
print("Preconditioning matrix: ")
print(v.dot(np.diag(lengthscales)).dot(v.transpose()))
