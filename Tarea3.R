#setwd("D:/Repositorios/SoftwareEstadistico_Tarea3")

#Librerias
#install.packages("TeachingDemos")
#library(TeachingDemos)
#install.packages("epitools")
#library(epitools)


###################################################################
########################### EJERCICIO 10###########################
###################################################################

# El porcentaje de titanio en una aleación utilizada en fundiciones
# aeroespaciales se mide en 51 partes seleccionadas al azar. La 
# desviación estándar de la muestra es 0.37
rm(list = ls())
# (b) p-valor
n = 51
alpha = 0.05
sd = 0.37
sd_0 = 0.35
chi2_0 = (n - 1) * (sd ^ 2) / (sd_0 ^ 2)
# Se obtiene el p-valor
p_valor = 2 * pchisq(chi2_0, df = n - 1)
if(p_valor > 2 * pchisq(chi2_0, df = n - 1, lower.tail = FALSE)) {
  p_valor = 2 * pchisq(chi2_0, df = n - 1, lower.tail = FALSE)
}
print(paste("(b). El p-valor es: ", round(p_valor, 3)))

# (a) Pruebe la hipótesis H0: σ=0.35 versus H1: σ≠0.35 usando α=0.05
if(p_valor > alpha){
  print("(a). Se acepta Ho: sigma=0.35")
} else {
  print("(a). Se rechaza Ho y se acepta H1: sigma≠0.35")
}

#(c) intervalo de confianza del 95%
sigmasqr.liminf = (n-1)*(sd ^ 2)/qchisq(1-alpha/2, df = n - 1)
sigma.liminf = round(sqrt(sigmasqr.liminf), 3)
sigmasqr.limsup = (n-1)*(sd ^ 2)/qchisq(alpha/2, df = n - 1)
sigma.limsup = round(sqrt(sigmasqr.limsup), 3)
print(paste("(c). [", sigma.liminf, " <= sigma <= ", sigma.limsup, "] = ", 1 - alpha))

# (d) Use el IC en la parte (c) para probar la hipótesis
if(sd_0 >= sigma.liminf && sd_0 <= sigma.limsup){
  print(paste("(d). ",sd_0, " esta dentro del intervalo de confianza [", sigma.liminf, ", ", sigma.limsup, "]"))
} else {
  print(paste("(d). ",sd_0, " esta fuera del intervalo de confianza [", sigma.liminf, ", ", sigma.limsup, "]"))
}


###################################################################
########################### EJERCICIO 11###########################
###################################################################

# Un artículo en Knee Surgury, Sports Traumatology, Arthroscopy 
# ("Reparación meniscal artroscópica con un tornillo absorbible: resultados y técnica quirúrgica", 2005, Vol. 13, págs. 273–279) 
# mostró que 25 de 37 lágrimas ubicadas entre 3 y 6 mm del borde del menisco fueron curados.
rm(list = ls())

# (a) Calcule un IC de la proporción de tales lágrimas que sanarán.
ic = binom.exact(25, 37, 0.95)
print(paste("(a). [", round(ic$lower,3), " <= p <= ", round(ic$upper, 3), "] = 0.95"))

# (b) Calcule un límite de confianza tradicional unilateral del 95%
# en la proporción de tales lágrimas que sanarán

###################################################################
########################### EJERCICIO 12###########################
###################################################################

# Se pregunta a una muestra aleatoria de 500 votantes registrados en 
# Phoenix si están a favor del uso de combustibles oxigenados durante 
# todo el año para reducir la contaminación del aire. Si más de 315 
# votantes responden positivamente, concluiremos que al menos el 60% 
# de los votantes favorecen el uso de estos combustibles.
rm(list = ls())

n = 500
p_0 = 0.6
alpha = 0.05

# (a) Encuentre la probabilidad de error de tipo I si exactamente el 60% de los votantes está a favor del uso de estos combustibles.
resultado_test = prop.test(300, 500, 0.6, alternative = "less", conf.level = 0.95)
print(paste("P(Error Tipo I) = ", round(resultado_test$p.value, 3)))

# (b) ¿Cuál es la probabilidad de error tipo II si el 75% de los votantes está a favor de esta acción?
resultado_test = prop.test(375, n, p_0, alternative = "less", conf.level = 1 - alpha)
print(paste("P(Error Tipo II) = ", 1 - round(resultado_test$p.value, 3)))

###################################################################
########################### EJERCICIO 13###########################
###################################################################

# La garantía de las baterías para teléfonos móviles se establece en 
# 400 horas de funcionamiento, con los procedimientos de carga adecuados. 
# Se realiza un estudio de 2000 baterías y tres dejan de funcionar 
# antes de las 400 horas. ¿Estos resultados experimentales respaldan 
# la afirmación de que menos del 0.2% de las baterías de la compañía 
# fallarán durante el período de garantía, con los procedimientos de 
# carga adecuados? Use un procedimiento de prueba de hipótesis con el 99% de confianza.
rm(list = ls())

prop.test(3, 2000, 0.02, alternative = "less", conf.level = 0.99)

###################################################################
########################### EJERCICIO 14###########################
###################################################################

# Su empresa ha determinado en el pasado que exactamente el 53% de las 
# personas que están en su área de mercado prefieren su producto. Y se 
# invierten varios miles de dólares en un programa publicitario para 
# incrementar su participación en el mercado. Luego, una muestra revela 
# lo siguiente, en el cual, valores de 1 es que prefieren su producto. 
# A un nivel de significancia del 4% de termine si la inversión fue la 
# correcta.
rm(list = ls())

muestra= c(0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1)
prefieren_producto = muestra[muestra == 1]
p_0 = 0.53
alpha = 0.04
resultado_test = prop.test(length(prefieren_producto), length(muestra), p_0, alternative = "greater", conf.level = 1 - alpha)
if(resultado_test$estimate > p_0){
  print(paste("La inversion fue la correcta. ", round(resultado_test$estimate, 3), " > ", p_0))
} else {
  print(paste("La inversion no fue la correcta. ", round(resultado_test$estimate, 3), " <= ", p_0))
}

###################################################################
########################### EJERCICIO 15###########################
###################################################################

# Un fabricante de lentes interoculares está calificando una nueva 
# rectificadora. Ella calificará la máquina si el porcentaje de lentes 
# pulidas que contienen defectos en la superficie no supera el 4%. Una 
# muestra aleatoria de 300 lentes contiene 11 lentes defectuosas.
rm(list = ls())
p_0 = 0.04
n = 300
x = 11
p = x/n

# (a) Formule y pruebe un conjunto apropiado de hipótesis para determinar 
# si la máquina puede ser calificada. Use una prueba de nivel fijo con α=0.05.

#Ho: p >= 0.04
#Ha: p < 0.04
alpha = 0.05
prop.test(x, n, p_0, alternative = "less", conf.level = 1 - alpha)

# (b) Encuentre el valor P para esta prueba.
resultado_test = prop.test(x, n, p_0, alternative = "less", conf.level = 1 - alpha)
print(paste("(b). p-valor = ", round(resultado_test$p.value, 3)))

# (c) Suponga que el porcentaje de lentes defectuosos es en realidad del 2%. ¿Cuál es el error β para esta prueba?
x = 300 * 0.02
resultado_test = prop.test(x, n, p_0, alternative = "less", conf.level = 1 - alpha)
print(paste("(c). P(Error Tipo II) = ", 1 - round(resultado_test$p.value, 3)))

# (d) Suponga que un error β de 0.05 es aceptable si el porcentaje verdadero es del 2%. Con α= 0.05, ¿cuál es el tamaño de muestra equivalente?

###################################################################
########################### EJERCICIO 16###########################
###################################################################

# El contenido de azúcar del jarabe en duraznos enlatados es normalmente 
# distribuido, y se cree que la varianza es σ2=18 (mg)2
rm(list = ls())
sigmasqr_0 = 18

# (a) Pruebe la hipótesis de que la varianza no es 18 (mg)2 si una muestra 
# aleatoria de n=10 latas producen una desviación estándar muestral 
# de 4 mg, utilizando una prueba de nivel fijo con 0.05. Indique cualquier 
# suposición necesaria sobre la distribución subyacente de los datos.

# (b) ¿Cuál es el valor P para esta prueba?
n = 10
sd = 4
alpha = 0.05

chi2_0 = (n - 1) * (sd ^ 2) / sigmasqr_0
# Se obtiene el p-valor
p_valor = 2 * pchisq(chi2_0, df = n - 1)
if(p_valor > 2 * pchisq(chi2_0, df = n - 1, lower.tail = FALSE)) {
  p_valor = 2 * pchisq(chi2_0, df = n - 1, lower.tail = FALSE)
}
print(paste("(b). El p-valor es: ", round(p_valor, 3)))
if(p_valor > alpha){
  print("(a). Se acepta Ho: sigmasqr = 18(mg)2")
} else {
  print("(a). Se rechaza Ho y se acepta H1: sigmasqr ≠ 18 (mg)2")
}

# (c) Encuentre un IC al 98%.
alpha = 0.02
sigmasqr.liminf = (n-1)*(sd ^ 2)/qchisq(1-alpha/2, df = n - 1)
sigmasqr.limsup = (n-1)*(sd ^ 2)/qchisq(alpha/2, df = n - 1)
print(paste("(c). [", round(sigma.liminf, 3), " <= sigmasqr <= ", round(sigma.limsup, 3),  "] = ", 1 - alpha))

# (d) Use el IC en la parte (c) para probar la hipótesis
if(sigmasqr_0 >= sigmasqr.liminf && sigmasqr_0 <= sigmasqr.limsup){
  print(paste("(d). ",sigmasqr_0, " esta dentro del intervalo de confianza [", sigmasqr.liminf, ", ", sigmasqr.limsup, "]"))
} else {
  print(paste("(d). ",sigmasqr_0, " esta fuera del intervalo de confianza [", sigmasqr.liminf, ", ", sigmasqr.limsup, "]"))
}
