import numpy as np
import matplotlib.pyplot as plt
def eulermethod(f,y_0,t,*args):
    steps = len(t)
    y = np.zeros(steps) # El array para tener las soluciones
    y[0] = y_0
    for i in range(steps-1):
        h = t[i+1] - t[i] # El step
        y[i+1] = y[i] + h*f(t[i], y[i],*args)        
    return y

def cargando(t,q,R,C,V_in):
    return -(q/(R*C)) + V_in/R

def descargando(t,q,R,C):
    return -q/(R*C)

def soluciones_edos(t_max,R,C,V, esta_cargando):
    t= np.linspace(0,t_max,5000)
    if esta_cargando:
        funcion_cargado = eulermethod(cargando,0,t,R,C,V)
        return funcion_cargado
    else:
        funcion_descargado = eulermethod(descargando,V*C,t,R,C)
        return funcion_descargado

R = float(input("Por favor ingrese un número para la resistencia (estará en Ω): "))
C_image = float(input("Por favor ingrese un número para la capacitancia (estará en µF): "))
C =  C_image* 1e-6  
t_max = float(input("Por favor ingrese un número para el tiempo máximo (estará en segundos): "))

while True:
    def_esta_cargando = int(input("Ingrese 0 si se está cargando, o 1 si se está descargando: "))
    if def_esta_cargando in [0, 1]:
        break

if def_esta_cargando ==0:
    V =float(input("Por favor ingrese un número para el voltaje de entrada (estará en V): "))
    esta_cargando = True
    proceso = 'cargándose'
    voltaje= f'V de entrada {V}'
    Funcion_carga = soluciones_edos(t_max, R, C, V, esta_cargando)
else:
    V=float(input("Por favor ingrese un número para el voltaje inicial del capacitor (estará en V): "))
    esta_cargando = False
    proceso = 'descargándose'
    voltaje = f'V inicial de {V}'
    Funcion_carga = soluciones_edos(t_max, R, C, V, esta_cargando)

t = np.linspace(0, t_max, 5000)
Funcion_voltaje = Funcion_carga / C  # V(t) = Q(t) / C
Funcion_corriente = np.gradient(Funcion_carga, t)  # I(t) = dQ/dt

if not esta_cargando:
    Funcion_corriente = -Funcion_corriente

labels = ["Q(t) (Coulomb)", "I(t) (Ampere)", "V(t) (Volts)" ]
Funciones = [Funcion_carga, Funcion_corriente, Funcion_voltaje]
Titulos = ["Función de carga Q(t) (C)", "Función de intensidad de corriente I(t) (A)", "Función de voltaje V(t) (V)"] 

fig, axs = plt.subplots(3, 1, figsize=(10, 20))
for i in range(len(Funciones)):
    axs[i].plot(t, Funciones[i])
    axs[i].set_title(f'{Titulos[i]}')
    axs[i].set_ylabel(labels[i])
    axs[i].grid(True)
plt.subplots_adjust(hspace=0.6)
plt.suptitle(f'Carga, intensidad y corriente en {proceso} con R={R}Ω, C ={C_image}µF, y {voltaje}')
plt.xlabel('Tiempo (s)')
plt.tight_layout(rect=[0, 0, 1, 0.96]) 
plt.show()