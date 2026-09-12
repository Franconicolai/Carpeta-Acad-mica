# -*- coding: utf-8 -*-
"""
Created on Thu Nov 12 09:17:50 2020

@author: jrver
"""

#!/usr/bin/env python
# coding: utf-8

# In[39]:


#!/usr/bin/env python
#
# Problema de Localización y Asignación, abordado mediante Simmulated Annealing

import gurobipy as gp
from gurobipy import GRB
import sys
import numpy as np
from time import time
import copy

NITER = 100

# AcÃ¡ se definen los datos
m = 20
n = 200

M = range(m)
N = range(n)

B = 1
Tk = 100000
alpha = 0.1
accept = 0
probT = 0


np.random.seed(16)

c = np.random.randint(3,7,size=(m,n))
f = np.random.randint(10,30,size=m)

# Se define el problema completo original

modelf = gp.Model('Location and Asignment Full Model')
modelf.setParam('OutputFlag', True) # turns on solver chatter


# Esta es la definiciÃ³n de variables y restricciones del problema completo original
xf = modelf.addVars(M,N,vtype=GRB.BINARY)
yf = modelf.addVars(M,vtype=GRB.BINARY)

rest_demand = modelf.addConstrs(((sum(xf[i,j] for i in M) == 1) for j in N),name = "DEMANDF")
rest_assign = modelf.addConstrs(((sum(xf[i,j] for j in N) <= n*yf[i]) for i in M),name = "ASSIGN")


modelf.setObjective((sum(sum(c[i,j]*xf[i,j] for i in M) for j in N)+sum(f[i]*yf[i] for i in M)),GRB.MINIMIZE)

modelf.optimize()
valor_optimo = modelf.objVal


# AquÃ­ se declara el problema relajado lineal y se resuelve
modelr = modelf.relax()
modelr.optimize()

valor_relajado = modelr.objVal

print( '\n Objectivo modelo relajado =', valor_relajado,'\n')
print( '\n Objectivo modelo completo =', valor_optimo,'\n')

# AcÃ¡ se define el modelo restringido a las x

model = gp.Model('Location and Asignment modelo restringido a x')
model.setParam('OutputFlag', False) # turns off solver chatter

# Estas son las y iniciales, todas las localizaciones se abren

#y = [0.0]*m
#for i in M:
#    y[i] = 1

y = [0.0]*m
j0 = np.random.randint(0,m-1)
y[j0] = 1

costo_y = sum(f[i]*y[i] for i in M)

x = model.addVars(m,n,vtype=GRB.BINARY)

rest_rel = model.addConstrs(((sum(x[i,j] for j in N) <= n*y[i]) for i in M),name = "ASSIGN_REL")
rest_assign = model.addConstrs(((sum(x[i,j] for i in M) == 1) for j in N),name = "DEMAND_REL")

model.setObjective((sum(sum(c[i,j]*x[i,j] for i in M) for j in N)),GRB.MINIMIZE)
 
model.update()
model.optimize()

valor = costo_y + model.objVal
best = valor

# Este es el ciclo principal del Simmulated Annealing

for k in range(1, NITER):

#   Ahora evaluamos una nueva soluciÃ³n vecina. La vecindad se define por aperturas
#   y cierres individuales de localizaciones en forma aleatoria

    yvecino = copy.deepcopy(y) #hacemos una copia en profundidad de y
    j0 = np.random.randint(0,m-1)
    yvecino[j0] = 1-yvecino[j0]
    
    # Tenemos el vecino, ahora evaluamos el valor
    # Hay que correr nuevamente el problema para evaluar.
    rest_rel[j0].RHS = n*yvecino[j0]
    
    model.update()
    model.optimize()
    
    costo_vecino = sum(f[i]*yvecino[i] for i in M)
    valor_vecino = costo_vecino + model.objVal

#   Se despliega el valor de la soluciÃ³n actual.
    
        
    # Ahora se decide si aceptar o no la soluciÃ³n vecina.
    
    valor_old = valor
    if valor_vecino <= valor:
        y = copy.deepcopy(yvecino) #hacemos una copia en profundidad de yvecino
        valor = valor_vecino
        accept = 0
    else:
        probT = np.exp(-(valor_vecino - valor)/(B*Tk))
        prob = np.random.rand()
        if probT > prob:
            y = copy.deepcopy(yvecino) #hacemos una copia en profundidad de yvecino
            valor = valor_vecino
            accept = 1
        else:
            rest_rel[j0].RHS = n*(1-yvecino[j0]) #en caso de rechazarse el vecino, revertimos el cambio en el modelo
            accept = 0
        
    if valor < best:
        best = valor
        
    # se actualiza la temperatura
    print("%8.0f %8.4f %8.4f %10.4f  %10.4f %4.0f" % (k, valor_old, valor_vecino, Tk, probT, accept))
    Tk = alpha*Tk
    
print('\n mejor valor encontrado: ',best)
print('\n valor Óptimo del problema: ',valor_optimo)
print('\n valor relajado lineal: ',valor_relajado)
        
    





#for j in N:
#    print(j,(sum(x[i,j].X for i in M)))