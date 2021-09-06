##########################################
# REGRESIÓN LINEAL 
##########################################


#########################################
# REGRESIÓN LINEAL SIMPLE
#########################################

# lectura del dataframe

base1 <- read.csv("Base_Pollos.csv", sep = ",", header = TRUE)

# Cambiar el nombre de columnas
colnames(base1)[1] = "Tiempo"
colnames(base1)[2] = "Peso"

# Gráfica 
plot(base1, xlab = "Tiempo", ylab = "Peso del pollo", 
     main = "Peso del pollo vs Tiempo")

# regresión lineal
rl <- lm(base1$Peso ~ base1$Tiempo) # (y~x)

rl

# Coeficiente Intercepto (B0): 24.465
# Coeficiente Variable Regresora (B1): 7.988

# Ecuación de la regresión: y = 7.988X + 24.465
# Ecuación del modelo lineal: Peso estimado = 7.988*(Tiempo) + 24.465

# PRUEBA DE SIGNIFICANCIA DEL MODELO OBTENIDO

# HIPOTESIS:
# H0: B1 = 0 (el modelo no es significativo / el peso no depende del tiempo de manera lineal)
# H1: B1 != 0 (el modelo es significativo / el peso depende del tiempo de manera lineal)

# ESTADÍSTICO DE PRUEBA:
summary(rl)
# p valor = 2.974e-08

# Rechazamos H0 si p valor < alpha = 0.05

# p valor = 2.974e-08 < alpha = 0.05, se rechaza H0
# por tanto, el modelo es significativo (el peso depende el tiempo de manera lineal)
# con un 95% de confianza

# AJUSTE OBTENIDO DEL MODELO
summary(rl)
# R^2 ajustado = 95.47%

# Es significativa y tiene buen ajuste

# Grafica del modelo obtenido vs datos originales
plot(base1, xlab = "Tiempo", ylab = "Peso del pollo", 
     main = "Peso del pollo vs Tiempo")
abline(rl, col = "red")



###############################################################
# REGRESIÓN LINEAL: MODELOS LINEALIZABLES
##############################################################


# Modelo exponencial
# Y = B0*e^(B1*X)

# Cambio de Y
y. <- log(base1$Peso)

rla <- lm(y. ~ base1$Tiempo) # regresion lineal asociada
rla

# Coeficiente Intercepto (B0): 3.748
# Coeficiente Variable Regresora (B1): 0.07669

# Ecuación de la regresión lineal asociada: y* = 0.07669X + 3.748
# Ecuación del modelo lineal asociado: y* = 0.07669*(Tiempo) + 3.748

# PRUEBA DE SIGNIFICANCIA DEL MODELO LINEAL ASOCIADO 

# HIPOTESIS:
# H0: el modelo no es significativo
# H1: el modelo es significativo

# ESTADÍSTICO DE PRUEBA:
summary(rla)
# p valor = 1.856e-14

# Rechazamos H0 si p valor < alpha = 0.05

# p valor = 1.856e-14 < alpha = 0.05, se rechaza H0
# por tanto, el modelo es significativo con un 95% de confianza

# AJUSTE OBTENIDO DEL MODELO LINEAL ASOCIADO
summary(rla)
# R^2 ajustado = 99.74%

# Es significativa y tiene buen ajuste

plot(base1$Tiempo, y., xlab = "Tiempo", ylab = "log(Peso)", 
     main = "log(Peso) vs Tiempo")
abline(rla, col = "blue")


# Este no es el modelo final, el modelo final es una exponencial
# con la forma: Y = B0*e^(B1*X)

# Intercepto del modelo original
B0 <- exp(coefficients(rla)[1])

# Ecuación del modelo exponencial:
# Peso estimado = 42.43611*exp(1.079712*Tiempo)

# Estimaciones del modelo original
y.est.exp <- B0*exp(coefficients(rla)[2]*base1$Tiempo)

# Gráficando modelo exponencial vs Original
plot(base1, xlab = "Tiempo", ylab = "Peso del pollo", 
     main = "Peso del pollo vs Tiempo")
lines(base1$Tiempo, y.est.exp, col = "green")

# se observa que ajusta mejor a los datos



#########################################
# REGRESIÓN LINEAL MÚLTIPLE
#########################################


# El data set empleado es el state.x77
# Para facilitar su interpretación se renombra y se modifica
library(dplyr)
datos <- as.data.frame(state.x77)
datos
help(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)




# Relación entre variables

install.packages("GGally")
library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

round(cor(x = datos, method = "pearson"), 3)

# Las variables que tienen una mayor relación lineal con la esperanza de vida son: asesinatos (r= -0.78), 
# analfabetismo (r= -0.59) y universitarios (r= 0.58)


# Generar el modelo
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area, data = datos )
summary(modelo)

# El modelo tiene un ajuste del 73.62%
# Como p valor = 3.534e-10, el modelo es significativo

######################################################################

# Selección de los mejores predictores
step(object = modelo, direction = "both", trace = 1)
# Selecciona aquellas variables más significativas


# El mejor modelo resultante del proceso anterior es:
modelo <- (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
                heladas, data = datos))
summary(modelo)


#######################################################################

# Validación de condiciones para la regresión múltiple lineal


# No multicolinialidad:

install.packages("car")
library(car)
vif(modelo)
#No hay predictores que muestren una correlación lineal muy alta ni inflación de varianza.


# Distribución normal de los residuos:
#Prueba grafica 

qqnorm(modelo$residuals) #Grafica de qqplot
qqline(modelo$residuals) #Linea de qqplot 

# Prueba analitica 
shapiro.test(modelo$residuals)
# Con un p valor de 0.525 se confirma normalidad


# Varianza constante 
ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        theme_bw()

# Autocorrelación:

library(car)
dwt(modelo, alternative = "two.sided") #Prueba de dubinn wantson 
# No hay evidencia de autocorrelación


# Identificación de posibles valores atípicos

library(dplyr)
datos$studentized_residual <- rstudent(modelo)
ggplot(data = datos, aes(x = predict(modelo), y = abs(studentized_residual))) +
        geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
        # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
        geom_point(aes(color = ifelse(abs(studentized_residual) > 2, 'red', 'black'))) +
        scale_color_identity() +
        labs(title = "Distribución de los residuos estandarizados",
             x = "predicción modelo") + 
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))


which(abs(datos$studentized_residual) > 2) #Determinando cuales son mayores a 3
# La observacion 11 y 19 son atipicas 


# CONCLUSION 

# El modelo lineal múltiple

# Esperanza de vida estimada= 7.103e+01 + 5.014e-05*(habitantes) - 3.001e-01*(asesinatos) + 4.658e-02*(universitarios)- 5.943e-03*(heladas)

# Con un ajuste de 73.6%

modelo




















