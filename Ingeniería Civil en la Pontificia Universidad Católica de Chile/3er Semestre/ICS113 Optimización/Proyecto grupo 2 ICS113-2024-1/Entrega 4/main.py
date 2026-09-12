# 0. Importar modulos
import gurobipy as gp
from gurobipy import GRB, Model, quicksum
import pandas as pd
import csv
from parametros import cantidad_asignaturas, cantidad_cursos, cantidad_regiones, periodos_sin_cambio, periodos_cambio_editorial, cantidad_de_periodos
import parametros

#Cabe destacar que en la implementacion del modelo en gurobi se utiliza como indice incial el 0 por
#lo que la mayoria los conjuntos parametros y restricciones van a estar desfasados en relacion al modelo del informe
#debido al trabajo de indices en python.

#Cabe destacar tambien que no de pudieron separar en directorios distintos los parametros de los archivos de output,
#porque no se especifico si se podia utilizar la libreria os por lo que podrian haber problemas al usar
#rutas relativas para abrir los archivos si se corrriera el codigo desde dos sistemas operativos diferentes.

# 1. Sección de Datos

#Parametros del modelo

#Parametro demanda
listas_regiones = []
with open("Demanda.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    string = ""
    for linea in lineas:
        linea = linea.strip("\n")

        lista = [int(x) for x in linea.split(",")]
        listas_regiones.append(lista)

        string += linea
        if linea != lineas[-1]:
            string += ","
    lista_todos_colegios = [int(x) for x in string.split(",")]

D_k = lista_todos_colegios

#Parametro Costo de produccion de un libro fisico
CP = parametros.CP

#Parametro Costo de transporte CT_r
with open("CostosTransporte.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    lista = lineas[0].strip("\n").split(",")
    CT_r = [int(x) for x in lista]

#Parametro porcentaje libros reutilizables R_j
with open("PorcentajeReutilizacion.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    lista = lineas[0].strip("\n").split(",")
    R_j = [float(x) for x in lista]

#Parametro costo anual de mantenimiento
CM = parametros.CM

#Parametro costo de la licencia digital de un libro
CD = parametros.CD

#Parametro costo de invertir en la digitalizacion de un colegio k
with open("CostoDigitalizacion.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    lista = lineas[0].strip("\n").split(",")
    CI_k = [int (x) for x in lista]

#Parametro presupuesto para el periodo t
with open("Presupuesto.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    lista = lineas[0].strip("\n").split(",")
    B_t = [int(x) for x in lista]

#Parametro el colegio k ya se encuentra digitalizado
with open("ColegiosDigitalizados.csv", "r", encoding = "utf-8") as f:
    lineas = f.readlines()
    lista = lineas[0].strip("\n").split(",")
    DG_k = [int(x) for x in lista]

BIGM = 1000000000


#Definir los conjuntos del modelo

I = range(cantidad_asignaturas)
J = range(cantidad_cursos)
R = range(cantidad_regiones)

T = range(cantidad_de_periodos)
T_1 = periodos_cambio_editorial
T_2 = periodos_sin_cambio

K_r = []
indice = 0
for i in range(cantidad_regiones):
    K_r.append(range(indice, indice + len(listas_regiones[i])))
    indice += len(listas_regiones[i])
K = range(indice)


# 2. Sección de modelación

# 2.1 Generar el modelo:
modelo = Model('Modelo de Optimización - Grupo 2')

# 2.2 Definir variables:
x = modelo.addVars(I,J,K,T, vtype = GRB.INTEGER, name="x_ijkt")
y = modelo.addVars(I,J,K,T, vtype = GRB.INTEGER, name="y_ijkt")
l = modelo.addVars(I,J,K,T, vtype = GRB.INTEGER, name="l_ijkt")
p = modelo.addVars(I,J,K,T, vtype = GRB.CONTINUOUS, name="p_ijkt")
w = modelo.addVars(K,T, vtype = GRB.BINARY, name="w_kt")
z = modelo.addVars(K,T, vtype = GRB.BINARY, name="z_kt")

# 2.3 Incorporar variables al modelo:
modelo.update()

# 2.4 Generar restricciones:

#Restriccion 1

modelo.addConstrs((l[i,j,k,t] + y[i,j,k,t] >= D_k[k] for i in I for j in J for k in K for t in T), name = "Restriccion1")
#Restriccion 2

#Estamos asumiendo que el primer periodo siempre pertenece a T_1

modelo.addConstrs((l[i,j,k,t] <= x[i,j,k,t] + R_j[j] * l[i,j,k,(t-1)] for i in I for j in J for k in K for t in T_2), name = "Restriccion2.1")

modelo.addConstrs((l[i,j,k,t] >= x[i,j,k,t] + R_j[j] * l[i,j,k,(t-1)] - 1 for i in I for j in J for k in K for t in T_2), name = "Restriccion2.2")

modelo.addConstrs((l[i,j,k,t] == x[i,j,k,t] for i in I for j in J for k in K for t in T_1), name = "Restriccion2.3")

#Restriccion 3

modelo.addConstrs((y[i,j,k,t] <= z[k,t] * D_k[k] for i in I for j in J for k in K for t in T), name = "Restriccion3")

#Restriccion 4
modelo.addConstrs((z[k,t] >= z[k,(t-1)]for k in K for t in T if t >= 1), name = "Restriccion4")

#modelo.addConstrs((z[k,0] == DG_k[k] for k in K), name = "Restriccion4.2")

#Restriccion 5
modelo.addConstrs(( quicksum(CM * z[k,t] + CI_k[k] * w[k,t] + quicksum(quicksum(CP * x[i,j,k,t] + p[i,j,k,t] + CD * y[i,j,k,t] for i in I) for j in J)for k in K) <= B_t[t] for t in T), name = "Restriccion5")

#Restriccion 6
modelo.addConstrs((quicksum(w[k,t] for t in T) + DG_k[k] <= 1 for k in K), name = "Restriccion6")

#Restriccion 7
modelo.addConstrs((x[i,j,k,t] <= (1 - z[k,t]) * BIGM for i in I for j in J for k in K for t in T), name = "Restriccion7")

#Restriccion 8 
modelo.addConstrs((quicksum(w[k,tt] for tt in range(t + 1)) + DG_k[k] >= z[k,t] for k in K for t in T if t >= 1), name = "Restriccion8.1")

modelo.addConstrs((w[k,0] + DG_k[k] >= z[k,0] for k in K), name = "Restriccion8.2")

#Restriccion 9 
modelo.addConstrs((p[i,j,kr,t] == CT_r[r] * x[i,j,kr,t] for i in I for j in J for t in T for r in R for kr in K_r[r]), name = "Restriccion9")

#Restriccion 10 naturaleza de las variables

modelo.addConstrs((x[i,j,k,t] >= 0 for i in I for j in J for k in K for t in T), name = "Naturaleza1")
modelo.addConstrs((y[i,j,k,t] >= 0 for i in I for j in J for k in K for t in T), name = "Naturaleza2")
modelo.addConstrs((l[i,j,k,t] >= 0 for i in I for j in J for k in K for t in T), name = "Naturaleza3")
modelo.addConstrs((p[i,j,k,t] >= 0 for i in I for j in J for k in K for t in T), name = "Naturaleza4")

#anadimos funcion objetivo
modelo.setObjective(quicksum(quicksum( (CM * z[k,t] + CI_k[k] * w[k,t]) + quicksum(quicksum(CP * x[i,j,k,t] + p[i,j,k,t] + CD * y[i,j,k,t] for i in I)for j in J) for k in K)for t in T), GRB.MINIMIZE)

modelo.update()

#aqui se puede cambiar el TimeLimit
#modelo.Params.MIPGap = 0.05
modelo.Params.TimeLimit = 1800
modelo.optimize()

#Aqui se realiza la sumatoria de todos los libros tanto fisicos como digitales comprados en cada periodo
sumatoria_total_x = 0
sumatoria_total_y = 0
lista_suma_x = []
lista_suma_y = []
for t in T:
    sumatoria_x = 0
    sumatoria_y = 0
    for k in K:
        for i in I:
            for j in J:
                sumatoria_x += int(x[i,j,k,t].x)
                sumatoria_y += int(y[i,j,k,t].x)
    lista_suma_x.append(sumatoria_x)
    lista_suma_y.append(sumatoria_y)
    sumatoria_total_x += sumatoria_x
    sumatoria_total_y += sumatoria_y

#Aqui sde genera una matriz donde se pone un 1 si es que el colegio k se digitalizo en el perido t
# y tambien se agregan las sumatorias antes definidas
tabla_excel = []
for t in T:
    fila_excel = []
    for k in K:
        fila_excel.append(int(w[k,t].x))
    
    fila_excel.append(lista_suma_x[t])
    fila_excel.append(lista_suma_y[t])
    
    tabla_excel.append(fila_excel)

#Se crea una fila final que dice si el colegio k termino digitalizado
fila_final_excel = [int(z[k,10].x) for k in K]
#Se agrega el total de libros comprados tanto digitales como fisicos entre todos los periodos
fila_final_excel.append(sumatoria_total_x)
fila_final_excel.append(sumatoria_total_y)
#se anade la ultima fila
tabla_excel.append(fila_final_excel)

#Se le asignan nombre a las columnas
columnas = [f"colegio {x}" for x in K]
columnas.append("Total libros fisicos")
columnas.append("Total libros digitales")

#Se le asignan nombre a las filas
indices = [f"Periodo {x}" for x in T]
indices.append("Final")

#Aqui se empieza la creacion de la segunda tabla de excel la cual contiene el valor de todas las variables no binarias
tabla_excel_2 = []
columna_x = []
columna_y = []
columna_l = []
columna_p = []
indices_2 = []
for i in I:
    for j in J:
        for k in K:
            for t in T:
                columna_x.append(int(x[i,j,k,t].x))
                columna_y.append(int(y[i,j,k,t].x))
                columna_l.append(int(l[i,j,k,t].x))
                columna_p.append(int(p[i,j,k,t].x))
                indices_2.append(f"i: {i}, j: {j}, k: {k}, t: {t}")

tabla_excel_2.append(columna_x)
tabla_excel_2.append(columna_y)
tabla_excel_2.append(columna_l)
tabla_excel_2.append(columna_p)

#se le asignan los nombres a las columnas
columnas_2 = ["X_", "Y_", "L_", "P_"]

#invertimos la matriz para que quede como queremos en el excel (si la manteniamos no cabia en una hoja de excel)
tabla_excel_2 = list(zip(*tabla_excel_2))

#Visualizar soluciones:
#se crean los archivos de excel para ambas tablas.
df_resultados = pd.DataFrame(tabla_excel, columns = columnas, index = indices)
df_resultados.to_excel("ExcelResultados.xlsx", sheet_name = "Resultados")

df_excel_2 = pd.DataFrame(tabla_excel_2, columns = columnas_2, index = indices_2)
df_excel_2.to_excel("ExcelVariables.xlsx", sheet_name = "Excel Variables")

#Creamos un excel con la cantidad de colegios digitales hasta cada periodo para despues poder graficarlo
lista_valores = []
for t in T:
    suma = 0
    for k in K:
        suma += int(z[k,t].x)
    lista_valores.append(suma)

lista_periodos = [x for x in T]

df = pd.DataFrame({"Nombre": lista_periodos, "Colegios_digitalizados": lista_valores})
df.to_excel("Grafico.xlsx", index = False)

#Guardamos el valor objetivo del modelo en una variable
valor_objetivo = modelo.ObjVal


#Calculamos los costos que se tendrian si no aplicaramos el modelo (solo se usaran libros fisicos)
suma = 0
for r in R:
    for k in K_r[r]:
        for i in I:
            for j in J:
                for t in T:
                    suma += D_k[k] * (CP + CT_r[r])


# la diferencia aproximada entre aplicar nuestro modelo vs no aplicarlo
diferencia = suma - int(valor_objetivo)

#Imprimimos los valores en consola
print(f"El valor que se gastaria sin aplicar el modelo sería: ${suma} CLP\n")
print(f"El valor optimo de nuestra función objetivo aplicando el modelo es de: ${int(valor_objetivo)} CLP\n")
print(f"El ahorro que se obtiene por aplicar el modelo es de ${diferencia}CLP\n")
