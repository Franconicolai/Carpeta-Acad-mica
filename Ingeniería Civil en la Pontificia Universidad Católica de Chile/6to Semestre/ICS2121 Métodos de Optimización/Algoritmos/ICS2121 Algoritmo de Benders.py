#
#  Código para Descomposición de Benders V2
#
# Autor: Moisés Saavedra
# Modificaciones y comentarios adicionales: Jorge Vera
#

from gurobipy import *
import numpy
import time

# Se setea que opcion de resolucion ocupar
opcion = int(input("Ingresa 0 para resolver el problema completo, \n ingresa 1 para usar Benders: "))
if opcion == 0:
    no_benders = True
else:
    no_benders = False


######################################
#   Invencion de parametros base     #
######################################

numpy.random.seed(1)

#Numero de depositos
n = 30
N = range(n)
NITERACIONES = 150
TOL = 0.00000001


# Parametros
q = numpy.random.randint(400000, 1000000, (n,n))
c = numpy.random.randint(400,500, (n,n))
s = numpy.random.randint(1000, 2000, n)
d = numpy.random.randint(1000, 2000, (n,n))

########################################
#  Opimiza deterministico equivalente  #
# (modelo resuleto sin ocupar BENDERS) #
########################################
if no_benders:
    deterministico = Model()
    deterministico.Params.Threads = 1

    # Generacion de variables
    x = deterministico.addVars(N, N, vtype=GRB.CONTINUOUS, lb=0)
    y = deterministico.addVars(N, N, vtype=GRB.CONTINUOUS, lb=0)

    # Generacion de restricciones
    R1 = deterministico.addConstrs(
        (quicksum(x[ii, jj] for jj in N if jj!=ii) <= s[ii]) for ii in N
    )

    R2 = deterministico.addConstrs(
        (quicksum(y[ii, jj] for jj in N if jj!=ii) <= s[ii] + quicksum(x[ll, ii] for ll in N if ll!=ii) - quicksum(x[ii, ll] for ll in N if ll!=ii)) for ii in N
    )

    R3 = deterministico.addConstrs(
        (y[ii, jj] <= d[ii][jj]) for ii in N for jj in N if jj!=ii
    )

    # Generacion de funcion objetivo
    deterministico.setObjective(
        quicksum(quicksum(q[ii][jj]*y[ii,jj] - c[ii][jj]*x[ii,jj] for jj in N if jj!=ii) for ii in N),
        GRB.MAXIMIZE
    )

    # Optimiza el equivalente
    inicio = time.time()
    deterministico.update()
    deterministico.optimize()
    final = time.time()

    print("Valor objetivo - Tiempo de resolucion ")
    print(deterministico.objVal, "   -   ", deterministico.Runtime)

else:
    ########################################
    #   Inicia  Descomposicion Benders
    #########################################

    # Define problema maestro
    maestro = Model()
    maestro.Params.OutputFlag = 0

    # Generacion de variables
    xm = maestro.addVars(N, N, vtype=GRB.CONTINUOUS, lb=0)
    theta = maestro.addVar(vtype=GRB.CONTINUOUS)

    # Generacion de restricciones
    R1m = maestro.addConstrs(
        (quicksum(xm[i,j] for j in N if j!=i) <= s[i]) for i in N
    )
    # Esta restricción es artifical, solo para excluir las variables x(i,i)
    R2m = maestro.addConstrs(
        (xm[i,i] == 0) for i in N
    )

    # Generacion de funcion objetivo
    # En la primera iteración necesitamos el maestro como problema en variables x, sin theta.
    # Esto produce una solución factible para la primera etapa, para poder iniciar el ciclo.
    #

    # Generacion de funcion objetivo
    maestro.setObjective(
        - quicksum(quicksum(c[i,j]*xm[i,j] for j in N if j!=i) for i in N),
        GRB.MAXIMIZE
    )

    


    # *********************************************************************************

    ##### Definicion del satélite ####
    # 
    #
    satelite = Model()
    satelite.Params.OutputFlag = 0

    # Generacion de variables
    u = satelite.addVars(N, vtype=GRB.CONTINUOUS, lb=0)
    v = satelite.addVars(N, N, vtype=GRB.CONTINUOUS, lb=0)

    # Generacion de restricciones
    Rsub = satelite.addConstrs(
        (u[i] + v[i,j] >= q[i,j]) for i in N for j in N if j!=i
    )

    # Genera funcion objetivo
    satelite.setObjective(
        (sum(u[i]*(1) for i in N) + quicksum(quicksum(d[i,j]*v[i,j] for j in N if j!=i) for i in N)),GRB.MINIMIZE
    )

    satelite.update()
    satelite.setParam('InfUnbdInfo',1)

    ##################################
    #         Ciclo Benders          #
    ##################################

    print("")
    print("---------- CICLOS BENDERS-------------")
    print("")
    print("Ciclo - Master - Subproblema")
    inicio = time.time()
    contador = 0
    lista = []

    # Se inician las iteracion controladas
    FOold = 0
    while contador <= NITERACIONES:
        contador += 1

        # Resuelve el modelo maestro
        maestro.update()
        maestro.optimize()


        # Actualizacion de coeficientes segun lo calculado en el maestro
        for i in N:
            u[i].Obj = (s[i] + sum((xm[j,i].X - xm[i,j].X) for j in N if j!=i))

        # Optimizacion del subproblema satélite.
        satelite.update()
        satelite.optimize()

        # Acumula el valor en la FO
        FO = maestro.objVal
        BOUND = satelite.objVal + sum(sum(-c[i,j]*(xm[i,j].X) for j in N if j!=i) for i in N)
        print(contador, "/", maestro.objVal, "/", BOUND)
        lista.append(FO)

        # Condicion de quiebre de BENDERS
        # Se podría también poner FO-BOUND
        if ((contador > 1) and (theta.X - satelite.ObjVal <= TOL)): 
            print("**** Termino por convergencia ****")
            break

        if satelite.status == 2 :
            maestro.addConstr(sum(u[i].X*(s[i] + quicksum((xm[j,i] - xm[i,j]) for j in N if j!=i)) for i in N) + quicksum(quicksum(d[i,j]*v[i,j].X for j in N if j!=i) for i in N)  >= theta)
        else:
            maestro.addConstr(sum(u[i].UnbdRay*(s[i] + quicksum((xm[j,i] - xm[i,j]) for j in N if j!=i)) for i in N) + quicksum(quicksum(d[i,j]*v[i,j].UnbdRay for j in N if j!=i) for i in N)  >= 0)

 
        maestro.setObjective(
        theta + quicksum(quicksum(-c[i,j]*xm[i,j] for j in N if j!=i) for i in N),
        GRB.MAXIMIZE
        )

    final = time.time()
    print("")
    print(" Tiempo de ejecucion:", (final - inicio))