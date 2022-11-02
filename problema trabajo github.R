### Problema: enunciado y solución hecho por todos los criterios de incertidumbre
### Jesús Manuel Falcón García, grupo 5 TD

## ENUNCIADO:

#Ana y Teodoro son una pareja que se quiere mudar y para ello van a comprar un inmueble.
#Pueden adquirir el inmueble por tres inmobiliarias distintas, cada una con ciertas
#diferencias;

#inmobiliaria El Sol: tienen que pagar una entrada de 20.000€ y el precio de la casa es de
#50.000€

#inmobiliaria Casa: tienen que pagar una entrada de 12.000€ y el precio de la casa es de
#60.000€

#inmobiliaria Eltexo: no tienen que pagar entrada y el precio es de 45.000€

#Si embargo, esto no es todo lo que hay qu tener en cuenta, porque el mercado es fluctuante
#y la hipoteca que tendrían que pagar luego variaría tanto de si los tipos de interés
#bajan o suben y de la inmobiliaria.

#Si bajan ->
    #con El Sol la hipoteca se les queda en 500€ mensuales durante 10 años,
    #Casa les deja 700€ mensuales durante 9 años y
    #Eltexo 900€ mensuales 15 años

#Si suben ->
    #con El Sol pagarían 800€ mensuales durante 11 años,
    #con Casa pagarían unos 900€ durante 10 años y
    #con Eltexo 1100€ durante 20 años

#Teniendo en cuenta, que Ana piensa que los tipos de interés van a bajar y Teodoro
#quiere escoger la opción que les permita arrepentirse menos, ¿qué decidirá cada uno?


## SOLUCIÓN:

#los estados de la naturaleza son (ei):
#e1 -> bajan los tipos de interés
#e2 -> suben los tipos de interés

#las alternativas son (di):
#d1 -> inmobiliaria El Sol
#d2 -> inmobiliaria Casa
#d3 -> inmobiliaria Eltexo

#matriz de costos (en este caso tenemos en cuenta que los datos de la matriz
#contiene datos que queremos minimizar):

#(d1, e1) -> 20.000+50.000+500*10*12
#(d1, e2) -> 20.000+50.000+800*11*12
#(d2, e1) -> 12.000+60.000+700*9*12
#(d2, e2) -> 12.000+60.000*900*10*12
#(d3, e1) -> 45.000+900*15*12
#(d3, e2) -> 45.000+1100*20*12

source("teoriadecision_funciones_incertidumbre.R")
#para tener acceso a las funciones de ese archivo y poder aplicar los criterios

matriz.costos = crea.tablaX(c(70000+5000*12, 70000+8800*12,
                            72000+6300*12, 72000+9000*12,
                            45000+13500*12, 45000+22000*12), numalternativas = 3,
                            numestados = 2)
matriz.costos

rownames(matriz.costos) = c("El Sol", "Casa", "Eltexo")

decision.Ana = criterio.Optimista(matriz.costos, F) #la F es para indicar que no es
#una matriz de datos favorables sino de costes
decision.Ana

#la decisión que tomará Ana, de manera optimista con respecto a los flujos del mercado
#inmobiliario, le hará tomar la decisión de comprar el inmueble con la inmobiliaria
#El Sol, pues ella espera que los tipos de interés bajen

decision.teo = criterio.Savage(matriz.costos, F)
decision.teo

#la decisión de Teodoro será la misma que la de Ana, notemos que a diferencia del
#caso de Ana, no hay valor óptimo

decision.global = criterio.Todos(matriz.costos, 0.5 ,F)
decision.global

#vemos que la mejor decisión a tomar es la de comparar con la inmobiliaria El Sol





