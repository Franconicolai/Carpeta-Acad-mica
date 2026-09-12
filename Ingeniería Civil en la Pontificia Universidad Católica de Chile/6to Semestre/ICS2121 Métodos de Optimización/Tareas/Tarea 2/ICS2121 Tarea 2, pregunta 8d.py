#!/usr/bin/env python
#

import gurobipy as gp
from gurobipy import GRB, quicksum
import numpy as np
import time
import matplotlib.pyplot as plt

# Esta es la semilla de los generadores aleatorios.
np.random.seed(20604)

# Definición de parámetros del problema
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
for t in T:
    Dda[t] = a*(0.8 + 0.4*np.random.random()) 

# Parámetros de control del algoritmo de Benders
NITERACIONES = 100  # Número máximo de iteraciones
TOL = 1e-6      # Tolerancia para convergencia
tiempo_limite = 600  # Límite de tiempo (2 horas)

# Listas para almacenar resultados
valores_maestro = []
cotas_superiores = []
gaps = []
tiempos = []

for i in range(len(M)):
    print("K[i]:", K[i])
for t in range(len(T)):
    print("Dda[t]:", Dda[t])

def descomposicion_benders():
    
    # 1. Definición del problema maestro
    maestro = gp.Model()
    maestro.Params.OutputFlag = 0

    y = maestro.addVars(N, T, vtype=GRB.BINARY, name="y")
    theta = maestro.addVar(vtype=GRB.CONTINUOUS, name="theta")
    maestro.addConstrs((quicksum(y[j, t] for t in T) <= 1 for j in N), name="Exclusion")
    maestro.setObjective(quicksum(quicksum(f[j, t] * y[j, t] for j in N) for t in T) + theta, GRB.MINIMIZE)

    # Satélite dual
    satelite = gp.Model()
    satelite.Params.OutputFlag = 0
    
    # Variables del satélite: x_jt, z_it, v_t
    pi = satelite.addVars(T, vtype=GRB.CONTINUOUS, lb=-GRB.INFINITY, ub=0, name="pi")  # pi_t ≤ 0
    mu = satelite.addVars(N, T, vtype=GRB.CONTINUOUS, lb=-GRB.INFINITY, ub=0, name="mu")  # mu_jt ≤ 0
    lambd = satelite.addVars(T, vtype=GRB.CONTINUOUS, lb=-GRB.INFINITY, ub=GRB.INFINITY, name="lambda")  # lambda_t libre
    rho = satelite.addVars(T, vtype=GRB.CONTINUOUS, lb=0, ub=GRB.INFINITY, name="rho")  # rho_t ≥ 0
    sigma = satelite.addVars(M, vtype=GRB.CONTINUOUS, lb=-GRB.INFINITY, ub=0, name="sigma")  # sigma_i ≤ 0

    satelite.addConstrs(
        alpha[j, t]*pi[t] + mu[j, t] + lambd[t] <= c[j, t]
        for j in N for t in T
    )
    satelite.addConstrs(
        -lambd[t] + rho[t] + sigma[i] <= e[i, t]
        for i in M for t in T
    )
    satelite.addConstrs(
        rho[t] <= Beta[t]
        for t in T
    )

    satelite.setObjective(
        quicksum(B[t]*pi[t] for t in T) +
        quicksum(mu[j, t] for j in N for t in T) +
        quicksum(Dda[t]*rho[t] for t in T) +
        quicksum(K[i]*sigma[i] for i in M), 
        GRB.MAXIMIZE
    )

    satelite.update()
    satelite.setParam('InfUnbdInfo', 1)

    print("\n--- INICIO ALGORITMO DE BENDERS ---")
    print("Ciclo - Master - Subproblema")
    inicio = time.time()
    contador = 0
    lista = []
    
    # Se inician las iteracion controladas
    FOold = 0

    while contador <= NITERACIONES:
        print(time.time() - inicio)
        if time.time() - inicio > tiempo_limite:
            print("**** Termino por límite de tiempo ****")
            break
        contador += 1

        # Resuelve el modelo maestro
        maestro.update()
        maestro.optimize()
        y_sol = { (j, t): y[j, t].X for j in N for t in T }

        for j in N:
            for t in T:
                mu[j, t].Obj = L[j]*y_sol[j, t]


        # Optimizacion del subproblema satélite.
        satelite.update()
        satelite.optimize()

        print("satelite.status:", satelite.status)

        # Acumula el valor en la FO
        print("status maestro:", maestro.status)
        FO = maestro.objVal
        BOUND = satelite.objVal + sum(sum(f[j, t]*y_sol[j, t] for j in N) for t in T)
        gap = abs(BOUND - FO)/abs(BOUND + 1e-10)
        print(contador, "/", FO, "/", BOUND)
        lista.append((contador, FO, BOUND, gap, time.time() - inicio))

        print(f"Iter {contador}: FO = {FO:,.2f} | BOUND = {BOUND:,.2f} | GAP = {gap:.6f}")


        # Condicion de quiebre de BENDERS
        # Se podría también poner FO-BOUND
        if ((contador > 1) and (satelite.ObjVal - theta.X <= TOL)): 
            print("satelite.ObjVal", satelite.ObjVal)
            print("**** Termino por convergencia ****")
            break

        # Corte de optimalidad
        print("satelite.status:", satelite.status)
        if satelite.status == 2:
            print("Corte de optimalidad")
            print("pi X:", [pi[t].X for t in T])
            print("mu X:", [mu[j, t].X for j in N for t in T][0:10])
            print("rho X:", [rho[t].X for t in T])
            print("sigma X:", [sigma[i].X for i in M])
            maestro.addConstr(
                sum(B[t]*pi[t].X for t in T) +
                sum(L[j]*y[j, t]*mu[j, t].X for j in N for t in T) +
                sum(Dda[t]*rho[t].X for t in T) +
                sum(K[i]*sigma[i].X for i in M)
                <= theta
            )
        else:
            print("Corte de factibilidad")
            print("pi UnbdRay:", [pi[t].UnbdRay for t in T])
            print("mu UnbdRay:", [mu[j, t].UnbdRay for j in N for t in T])
            print("rho UnbdRay:", [rho[t].UnbdRay for t in T])
            print("sigma UnbdRay:", [sigma[i].UnbdRay for i in M])
            maestro.addConstr(
                sum(B[t]*pi[t].UnbdRay for t in T) +
                sum(L[j]*y[j, t]*mu[j, t].UnbdRay for j in N for t in T) +
                sum(Dda[t]*rho[t].UnbdRay for t in T) +
                sum(K[i]*sigma[i].UnbdRay for i in M)
                <= 0
            )

        maestro.setObjective(
            sum(sum(f[j, t] * y[j, t] for j in N) for t in T) + theta,
            GRB.MINIMIZE
        )

    final = time.time()
    print("")
    print(" Tiempo de ejecucion:", (final - inicio))

    return lista

# --- Ejecución y resultados ---
iter_data = descomposicion_benders()

valores_maestro = [it[1] for it in iter_data]
cotas_superiores = [it[2] for it in iter_data]
gaps = [it[3] for it in iter_data]
tiempos = [it[4] for it in iter_data]

print("\n--- RESULTADOS FINALES ---")
print(f"Valor maestro final: {valores_maestro[-1]:,.2f}")
print(f"Cota superior final: {cotas_superiores[-1]:,.2f}")
print(f"Gap final: {gaps[-1]:.6f}")
print(f"Iteraciones totales: {len(iter_data)}")

# --- Gráfico de evolución ---
plt.figure(figsize=(10,6))
plt.plot(range(1, len(valores_maestro)+1), valores_maestro, 'o-', label="Valor Maestro (FO)")
plt.plot(range(1, len(cotas_superiores)+1), cotas_superiores, 's--', label="Cota Superior (BOUND)")
plt.xlabel("Iteración")
plt.ylabel("Valor de la función objetivo")
plt.title("Evolución del Maestro y Cota Superior - Descomposición de Benders")
plt.legend()
plt.grid(True)
plt.show()

# Gráfico del gap
plt.figure(figsize=(8,5))
plt.plot(range(1, len(gaps)+1), gaps, 'r-o')
plt.xlabel("Iteración")
plt.ylabel("Gap relativo")
plt.title("Convergencia del algoritmo (Gap por iteración)")
plt.grid(True)
plt.show()