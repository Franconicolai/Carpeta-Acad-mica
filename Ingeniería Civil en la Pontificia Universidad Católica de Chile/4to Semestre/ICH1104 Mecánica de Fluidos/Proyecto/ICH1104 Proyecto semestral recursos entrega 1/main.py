import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import matplotlib.animation as animation
import math

# Constantes del problema (cohete de agua)
P_atm = 101325  # Presión atmosférica en Pa 101325
P_0 = 300000 # Presión inicial del aire en Pa
V_total = 0.003  # Volumen total del cohete en m^3
V_agua0 = 0.001  # Volumen inicial del agua en m^3
V_aire0 = V_total - V_agua0  # Volumen inicial del aire en m^3
gamma = 1.4  # Índice adiabático del aire


# Calculo area boquilla
diametro_cm = 2
diametro_m = diametro_cm / 100
radio = diametro_m / 2
A_boquilla = math.pi * (radio ** 2) # Área de la boquilla en m^2

rho_agua = 1000  # Densidad del agua en kg/m^3
m_cohete_seco = 0.1  # Masa del cohete vacío (sin agua) en kg
o = np.radians(25)  # Ángulo de lanzamiento en radianes
t0 = 0  # Tiempo inicial
tf = 1 # Tiempo final en segundos
h = 0.001  # Paso de tiempo en segundos
g = 9.81  # Aceleración debido a la gravedad en m/s^2
C_d = 0.3  # Coeficiente de arrastre
rho_aire = 1.225  # Densidad del aire en kg/m^3
# Calculo area botella
diametro_cm = 10
diametro_m = diametro_cm / 100
radio = diametro_m / 2
A = math.pi * (radio ** 2) # Área de sección transversal en m^2

# Definimos listas
tiempo_empuje = []
tiempo = []
dq_agua_dt = []
masa_dt = []
f_empuje_list = []  # Lista para almacenar los valores de empuje
posicion_x = [0]
posicion_y = [0]
velocidad_x = [0]
velocidad_y = [0]

# Función para calcular el caudal de agua
def dV_agua_dt(V_agua):
    V_aire = V_total - V_agua
    P_aire = P_0 * (V_aire0 / V_aire)**gamma
    C = A_boquilla * np.sqrt(2 / rho_agua)
    return -C * np.sqrt(P_aire - P_atm) if P_aire > P_atm else 0

# Función para calcular el empuje
def calcular_fuerza_empuje(V_agua, V_aire0):
    P_aire = P_0 * (V_aire0 / (V_total - V_agua))**gamma
    v = np.sqrt((2 * (P_aire - P_atm)) / rho_agua)
    m_dot = rho_agua * dV_agua_dt(V_agua)  # Tasa de cambio de masa
    f_empuje = m_dot * v
    return f_empuje

# Método Runge-Kutta para la tasa de cambio del volumen
def runge_kutta_4(V_agua0, t0, tf, h):
    
    V_agua = V_agua0
    tiempos = np.arange(t0, tf, h)
    
    for i in tiempos:
        tiempo_empuje.append(i)  # Guardar el tiempo
        dq_agua_dt.append(V_agua)  # Guardar el volumen de agua
        masa = m_cohete_seco + V_agua * rho_agua
        masa_dt.append(masa)  # Guardar la masa

        # Calcular el empuje
        f_empuje = calcular_fuerza_empuje(V_agua, V_aire0)
        f_empuje_list.append(-1 * f_empuje)  # Guardar el empuje

        # Método de Runge-Kutta para actualizar el volumen de agua
        k1 = h * dV_agua_dt(V_agua)
        k2 = h * dV_agua_dt(V_agua + 0.5 * k1)
        k3 = h * dV_agua_dt(V_agua + 0.5 * k2)
        k4 = h * dV_agua_dt(V_agua + k3)

        V_agua += (k1 + 2 * k2 + 2 * k3 + k4) / 6  # Actualizar el volumen

        # Asegurarse de que el volumen no sea negativo
        if V_agua < 0:
            V_agua = 0  # No puede haber volumen negativo
            break

    return tiempo_empuje, dq_agua_dt, masa_dt

tiempo_empuje, dq_agua_dt, masa_dt = runge_kutta_4(V_agua0, t0, tf, h)

# Inicialización
t = 0  # Índice para tiempo_empuje
tiempo_actual = t0  # Tiempo inicial
tiempo_final_empuje = len(tiempo_empuje)  # Longitud de tiempo_empuje
tiempos = np.arange(t0, tf, h)

# Simulación
for tiempo_actual in tiempos:
    
    t = int((tiempo_actual - t0) / h)  # Calcular el índice correspondiente
    if t >= len(dq_agua_dt):  # Asegúrate de que no excedas el límite de la lista
        break
    
    dq_agua_dt_valor = dq_agua_dt[t]  # Obtener el valor de dq_agua_dt en cada iteración
    f_empuje = calcular_fuerza_empuje(dq_agua_dt_valor, V_aire0)
    
    f_empuje = -1 * f_empuje

    # Calcular aceleración
    a_x = (f_empuje * np.cos(o)-0.5 * C_d * rho_aire * A * velocidad_x[-1]**2) / masa_dt[t]  # Movimiento en x
    a_y = (f_empuje * np.sin(o) - masa_dt[t] * g - 0.5 * C_d * rho_aire * A * velocidad_y[-1]**2 - g * masa_dt[t]) / masa_dt[t]  # Movimiento en y

    # Integrar para obtener velocidad (Runge-Kutta)
    k1_vx = h * a_x
    k2_vx = h * ((f_empuje * np.cos(o)-0.5 * C_d * rho_aire * A * velocidad_x[-1]**2) / masa_dt[t] + k1_vx / (2 * h))
    k3_vx = h * ((f_empuje * np.cos(o)-0.5 * C_d * rho_aire * A * velocidad_x[-1]**2) / masa_dt[t] + k2_vx / (2 * h))
    k4_vx = h * ((f_empuje * np.cos(o)-0.5 * C_d * rho_aire * A * velocidad_x[-1]**2) / masa_dt[t] + k3_vx / (2 * h))
    nuevo_vx = velocidad_x[-1] + (k1_vx + 2 * k2_vx + 2 * k3_vx + k4_vx) / 6
    velocidad_x.append(nuevo_vx)  # No permitir velocidades negativas

    # Integrar para obtener velocidad en y
    k1_vy = h * a_y
    k2_vy = h * ((f_empuje * np.sin(o)- masa_dt[t] * g-0.5 * C_d * rho_aire * A * velocidad_y[-1]**2) / masa_dt[t] + k1_vy / (2 * h))
    k3_vy = h * ((f_empuje * np.sin(o)- masa_dt[t] * g-0.5 * C_d * rho_aire * A * velocidad_y[-1]**2) / masa_dt[t] + k2_vy / (2 * h))
    k4_vy = h * ((f_empuje * np.sin(o)- masa_dt[t] * g-0.5 * C_d * rho_aire * A * velocidad_y[-1]**2) / masa_dt[t] + k3_vy / (2 * h))
    nuevo_vy = velocidad_y[-1] + (k1_vy + 2 * k2_vy + 2 * k3_vy + k4_vy) / 6
    velocidad_y.append(nuevo_vy)  # No permitir velocidades negativas

    # Integrar para obtener posición
    k1_x = h * velocidad_x[-1]
    k2_x = h * (velocidad_x[-1] + k1_x / 2)
    k3_x = h * (velocidad_x[-1] + k2_x / 2)
    k4_x = h * (velocidad_x[-1] + k3_x)
    nuevo_x = posicion_x[-1] + (k1_x + 2 * k2_x + 2 * k3_x + k4_x) / 6
    posicion_x.append(nuevo_x)  # No permitir posiciones negativas

    k1_y = h * velocidad_y[-1]
    k2_y = h * (velocidad_y[-1] + k1_y / 2)
    k3_y = h * (velocidad_y[-1] + k2_y / 2)
    k4_y = h * (velocidad_y[-1] + k3_y)
    nuevo_y = posicion_y[-1] + (k1_y + 2 * k2_y + 2 * k3_y + k4_y) / 6
    posicion_y.append(nuevo_y)  # No permitir posiciones negativas

while posicion_y[-1] >= 0:  # Mientras la posición en Y sea mayor que 0
    # Obtener las últimas condiciones de posición, velocidad y aceleración
    x = posicion_x[-1]
    y = posicion_y[-1]
    vx = velocidad_x[-1]
    vy = velocidad_y[-1]
    
    # Calcular la aceleración debido a la gravedad y la resistencia del aire
    ax = -0.5 * C_d * rho_aire * A * vx**2 / (m_cohete_seco + dq_agua_dt[-1] * rho_agua)  # Resistencia del aire
    ay = -g * (m_cohete_seco + dq_agua_dt[-1] * rho_agua) - (0.5 * C_d * rho_aire * A * vy**2) / (m_cohete_seco  + dq_agua_dt[-1] * rho_agua)  # Gravedad y resistencia
    
    # Actualizar la velocidad
    vx += ax * h
    vy += ay * h
    
    # Actualizar la posición
    x += vx * h
    y += vy * h
    
    # Agregar los nuevos datos a las listas
    posicion_x.append(x)
    posicion_y.append(y)
    velocidad_x.append(vx)
    velocidad_y.append(vy)
    
    # Agregar tiempo
    dq_agua_dt.append(0)  # No hay cambio de agua
    masa_dt.append(m_cohete_seco)  # Masa constante

# Asegúrate de que tus listas (tiempo_empuje, dq_agua_dt, masa_dt, f_empuje_list, velocidad_x, velocidad_y) ya están definidas

# Antes de graficar, asegúrate de que todas las listas tengan la misma longitud
min_len = min(len(tiempo_empuje), len(dq_agua_dt), len(masa_dt), len(f_empuje_list), len(velocidad_x), len(velocidad_y))

# Recortar las listas a la longitud mínima
tiempo_empuje = tiempo_empuje[:min_len]
dq_agua_dt = dq_agua_dt[:min_len]
masa_dt = masa_dt[:min_len]
f_empuje_list = f_empuje_list[:min_len]
velocidad_x = velocidad_x[:min_len]
velocidad_y = velocidad_y[:min_len]

# Crear una figura y ejes
fig, axs = plt.subplots(3, 1, figsize=(15, 30))  # 3 filas, 1 columna

# Graficar trayectoria en el plano XY
min_len = min(len(posicion_x), len(posicion_y))  # Asegúrate de que ambas listas tengan la misma longitud
axs[0].plot(posicion_x[:min_len], posicion_y[:min_len], label='Trayectoria', color='blue')
axs[0].set_title('Trayectoria en el plano XY')
axs[0].set_ylabel('Posición en Y (m)')
axs[0].set_xlabel('Posición en X (m)')
axs[0].legend()

# Graficar masa_dt
axs[1].plot(tiempo_empuje, masa_dt, color='blue')
axs[1].set_title('Masa cohete (kg)')
axs[1].set_ylabel('Masa (kg)')
axs[1].set_xlabel('Tiempo (s)')  # Etiqueta del eje X para el gráfico de masa

# Graficar empuje
axs[2].plot(tiempo_empuje, f_empuje_list, color='blue')
axs[2].set_title('Empuje (N)')
axs[2].set_ylabel('Empuje (N)')
axs[2].set_xlabel('Tiempo (s)')  # Etiqueta del eje X para el gráfico de empuje

# Ajustar el espacio entre los gráficos
plt.subplots_adjust(hspace=0.5)  # Ajusta automáticamente el espacio entre los subgráficos
plt.show()


# Crear la figura y los ejes
fig, ax = plt.subplots()

# Inicializar la línea que se va a animar
line, = ax.plot([], [], 'o-', lw=0.0001)  # Línea más fina

# Inicializar los límites de los ejes
ax.set_xlim(-1, 150)
ax.set_ylim(-1, 50)

# Etiquetas de los ejes
ax.set_xlabel('Posición en X (m)')  # Etiqueta del eje X
ax.set_ylabel('Posición en Y (m)')  # Etiqueta del eje Y

# Calcular el número de frames y el intervalo
num_frames = len(posicion_x)  # Número total de frames
total_time = 0.1  # Tiempo total de la animación en segundos
interval = (total_time * 1000) / num_frames  # Intervalo en milisegundos por frame

# Función para actualizar la animación
def update(num):
    line.set_data(posicion_x[:num* 100], posicion_y[:num* 100])
    return line,

# Crear la animación con el intervalo calculado
ani = animation.FuncAnimation(fig, update, frames=num_frames, blit=True, interval=interval)

plt.show()