# Problema 1:

# dos poblaciones : privada y publica
# aplha: 10% = 0.1
# diferencia de $2500 dolares


# datos salarios
# --------------------------
# publica:
# Públicas Salario
# 7774
# 5702
# 6851
# 4938
# 4902
# 8580
# 6643
# 4753
# 9069
# 4936
# 5187
# 6580
# 5356
# 7513
# 7602
# 6750
# 6397
# 6522
# 5487
# 8201
# 9299
# 9350
# 10005
# 11552
# 7028
# 6082
# 8716
# 7065
# 7523
# 6277
# 9079
# 7818
# 6465
# 11411
# 11342
# 8937
# 8735
# 11132
# 5358
# 6099
# 10575

#privada:
# Privadas Salario
# 6983
# 10640
# 9391
# 7399
# 10016
# 7518
# 10137
# 6960
# 6711
# 7451
# 7567
# 7203
# 6444
# 9617
# 11348
# 7425
# 9671
# 5851
# 7077
# 6974
# 5971
# 6841
# 10006
# 9564
# 10273
# 9409
# 9208
# 9747
# 9865
# 9575
# 9744
# 9477
# 16679
# 14048
# 14989
# 17629
# 12934
# 14740
# 15263
# 14354
# 9457
# 10605
# 10411
# 10325
# 9775
# 6022
# 10326
# 8060
# 8487
# 11405
# 9688
# 7137
# 6737
# 6673
# 7749
# 6687
# 6516
# 8988
# 11972
# 9046
# 8017
publica <- c(7774, 5702, 6851, 4938, 4902, 8580, 6643, 4753, 9069, 4936, 5187, 6580, 5356, 7513, 7602, 6750, 6397, 6522, 5487, 8201, 9299, 9350, 10005, 11552, 7028, 6082, 8716, 7065, 7523, 6277, 9079, 7818, 6465, 11411, 11342, 8937, 8735, 11132, 5358, 6099, 10575)
privada <- c(6983, 10640, 9391, 7399, 10016, 7518, 10137, 6960, 6711, 7451, 7567, 7203, 6444, 9617, 11348, 7425, 9671, 5851, 7077, 6974, 5971, 6841, 10006, 9564, 10273, 9409, 9208, 9747, 9865, 9575, 9744, 9477, 16679, 14048, 14989, 17629, 12934, 14740, 15263, 14354, 9457, 10605, 10411, 10325, 9775, 6022, 10326, 8060, 8487, 11405, 9688, 7137, 6737, 6673, 7749, 6687, 6516, 8988, 11972,9046,8017)



# desv estandar p1 y p2
sd_1 <- sd(publica)
sd_2 <- sd(privada)

# cantidades n
npublica <- length(publica)
nprivada <- length(privada)

# medias
mediapublica <- mean(publica)
mediaprivada <- mean(privada)

# SE para error estandar entre medias 
SE <- sqrt((sd_1^2 / npublica) + (sd_2^2 / nprivada))

# Estadístico de prueba t
t_stat <- (mediapublica - mediaprivada) / SE

# grados de libertad
df <- npublica + nprivada - 2

# valor critico de t y p 
alpha <- 0.1
t_crit <- qt(1 - alpha/2, df)
p_value <- 2 * pt(-abs(t_stat), df)

# valor absoluto de estadistico prueba t
abs_t_stat <- abs(t_stat)

#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓
#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓


# PROBLEMA 2

# datos, es de variabilidad, varianza, determinar si mayor variabilidad en el % de estudiantes que se reportan blancos
# y de los que son negros
# alpha = 0.01

# 1163 universidades

# h0: sigma1^2 menor o igual que sigma2^2
# ha: sigma1^2 mayor que sigma2^2

# datos:

n1 <- 1163
n2 <- 1163
x1 <- 57.4853659
x2 <- 13.28013758
s1_sq <- 23.7694821
s2_sq <- 17.608473


# f estimado (f = s1^2 /  s2^2)

f_estimado <- s1_sq / s2_sq

# grados de libertad
df_num <- n1 - 1
df_denom <- n2 - 1

# valor critico de F con significancia de 0.01

f_critico <- qf(0.995, df_num, df_denom)

if (f_estimado > f_critico) {
  decision <- "rechazar H0"
  conclusion <- "hay evidencia para decir que hay mayor variabilidad en estudiantes que son blancos que ngros"
  
} else {
  decision <- "aceptar H0"
}




#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓
#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓


# Problema 3

# muestra: estudiantes ( mujeres y hombres ), antes y despues 
# Datos ---------------
# antes: 
# x1 = 60.63
# n1 = 99
# sd1 = 15.990396
# despues:
# x2 = 63.26
# n1 = 99
# sd2 = 16.6512777

x1 <- 60.63
n1 <- 99
sd_1 <- 15.990396

x2 <- 63.26
n2 <- 99
sd_2 <- 16.6512777

# SE para error estandar entre medias 
SE <- sqrt((sd_1^2 / n1) + (sd_2^2 / n2))

# Estadístico de prueba t
t_stat <- (x1 - x2) / SE

# grados de libertad
df <- n1 + n2 - 2

# valor critico de t y p 
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df)
p_value <- 2 * pt(-abs(t_stat), df)

# valor absoluto de estadistico prueba t
abs_t_stat <- abs(t_stat)


#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓
#▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓
# problema 4

x1 <- 






