#setwd("D:/Repositorios/SoftwareEstadistico_Tarea3")

#Librerias
#install.packages("TeachingDemos")
#library(TeachingDemos)


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

