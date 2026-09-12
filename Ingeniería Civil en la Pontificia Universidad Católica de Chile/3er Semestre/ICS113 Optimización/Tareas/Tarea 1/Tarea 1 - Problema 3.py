#Importamos los modulos
import pandas as pd
from gurobipy import GRB, Model, quicksum
import csv

# Lectura datos formato csv (El presente procedimiento no es tradicional)
with open('costos.csv', 'r') as f:
    reader = csv.reader(f)
    next(reader)  # Saltar la cabecera
    costos = [float(row[0]) for row in reader]

with open('limites.csv', 'r') as f:
    reader = csv.reader(f)
    next(reader)  # Saltar la cabecera
    limites = [[float(x) for x in row] for row in reader]

with open('contenidos_nutricionales.csv', 'r') as f:
    reader = csv.reader(f)
    next(reader)  # Saltar la cabecera
    contenidos_nutricionales = [[float(x) for x in row] for row in reader]

# Generamos los valores de las dimenciones
nutrientes = len(limites)
cereales = len(costos)

# Generar el modelo
modelo = Model('Optimizacion de Gallos')

# Definir variable
x = modelo.addVars(cereales, lb=0., name="x_j")

# Incorporar variables al modelo
modelo.update()

# Restricción 1: La mezcla está compuesta únicamente por cereales
modelo.addConstr(quicksum(x[j] for j in range(cereales)) == 1, name="Restriccion 1")

# Restricciones
for i in range(nutrientes):
    
    # Restricción 2: Se debe cumplir una proporción mínima de nutrientes
    modelo.addConstr(quicksum(contenidos_nutricionales[i][j] * x[j] for j in range(cereales)) >= limites[i][0])

    # Restricción 3: Se debe cumplir una proporción máxima de nutrientes
    modelo.addConstr(quicksum(contenidos_nutricionales[i][j] * x[j] for j in range(cereales)) <= limites[i][1])

#Restricción de naturaleza de variable:
for j in range(nutrientes):
    modelo.addConstr(x[j] >= 0)

# Definir función objetivos
objetivo = quicksum(costos[j] * x[j] for j in range(cereales))
modelo.setObjective(objetivo, GRB.MINIMIZE)

# Optimizar el modelo
modelo.optimize()

# Imprimir la solución óptima
print("\n"+"-"*9+" Solución Optima "+"-"*9)
print(f'Valor óptimo: {round(modelo.ObjVal,3)} $/kg')
for j in range(cereales):
    print(f'Cereal {j+1}: {round(x[j].x,3)} kg')
    
print("\n"+"-"*9+" Cantidad de nutrientes en la solución óptima "+"-"*9)
for i in range(nutrientes):
    cantidad_nutriente = quicksum(contenidos_nutricionales[i][j] * x[j].x for j in range(cereales))
    print(f"Nutriente {i+1}: {limites[i][0]} ≤ {round(cantidad_nutriente.getValue(), 3)} ≤ {limites[i][1]}")
     
# ¿Cuál de las restricciones son activas?
print("\n"+"-"*9+" Restricciones Activas "+"-"*9)
for constr in modelo.getConstrs():
    if constr.getAttr("slack") == 0:
        print(f"La restriccion {constr} está activa")