#!/usr/bin/env python
#
# Problema de Cosecha y procesamiento de frutas

import gurobipy as gp
from gurobipy import GRB
import sys
import numpy as np
from time import time

# Esta es la semilla de los generadores aleatorios. Esta debe modificarse según
# las instrucciones del enunciado.
np.random.seed(20604) # 2 de julio

# Acá se definen los datos, con la notación del enunciado
# NO MODIFIQUEN NADA DE ESTO

m = 12 
n = 140
hor = 12
M = range(m)
N = range(n)
T = range(hor)

c = np.random.randint(150,400,size=(n,hor))
f = np.random.randint(200000,300000,size=(n,hor))
#e = np.random.randint(10,15,size=(m,hor))
e = np.random.randint(30,45,size=(m,hor))
B = np.random.randint(n*10,n*30,size=hor)
Beta = np.random.randint(n*10,n*30,size=hor)
Beta = np.random.randint(800,1000,size=hor)
Dda = np.random.randint(n*10,n*30,size=hor)
K = np.random.randint(n*700,n*1000,size=m)
alpha = np.random.randint(7,10,size=(n,hor))/50
L = np.random.randint(1500,2000,size=n)

a = sum(L[j] for j in N)/hor 
for t in T :
    Dda[t] = a*(0.8 + 0.4*np.random.random()) 

# Se hace la definicion del modelo

model = gp.Model('Cosecha y procesamiento de frutas')

# Escogimos limite de 10 minutos
model.setParam('TimeLimit', 600)


# Esta es la definición de variables y restricciones del problema 
x = model.addVars(N,T)
y = model.addVars(N,T,vtype=GRB.BINARY)
z = model.addVars(M,T)
v = model.addVars(T)

rest_capacidad = model.addConstrs(((sum(alpha[j,t]*x[j,t] for j in N) <= B[t]) for t in T),name = "CAPACIDAD")
rest_fixed = model.addConstrs((x[j,t] <= L[j]*y[j,t] for j in N for t in T),name = "CARGA_FIJA")
rest_ex = model.addConstrs(((sum(y[j,t] for t in T) <= 1) for j in N),name = "EXCLUSION")
rest_flujo = model.addConstrs(((sum(x[j,t] for j in N) == sum(z[i,t] for i in M)) for t in T),name = "FLUJO")
rest_demanda = model.addConstrs(((sum(z[i,t] for i in M))  >= Dda[t]-v[t] for t in T),name = "DEMANDA")
rest_capproc = model.addConstrs(((sum(z[i,t] for t in T) <= K[i]) for i in M),name = "CAPACIDAD_PROCESO")


model.setObjective((sum(sum(c[j,t]*x[j,t] for j in N) for t in T) + sum(sum(f[j,t]*y[j,t] for j in N) for t in T) + sum(sum(e[i,t]*z[i,t] for i in M) for t in T) + sum(Beta[t]*v[t] for t in T)),GRB.MINIMIZE)

model.optimize()
valor_optimo = model.objVal


# Aquí se declara el problema relajado lineal y se resuelve
modelr = model.relax()
modelr.optimize()
valor_relajado = modelr.objVal

# Resultados
print('\n Objectivo modelo relajado =', modelr.objVal,'\n')
print('\n Objectivo modelo completo =', valor_optimo,'\n')




