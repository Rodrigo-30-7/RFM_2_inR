#########################################################################################
#########################################################################################
################################## MODELO DE RFM ########################################
#########################################################################################
#########################################################################################

library(data.table)
# Cargamos los datos en el data.frame itemsSold para procesarlos
itemsSold = fread("C:/Users/mvill/Desktop/Rodrigo/Modelo de RFM/customers_data.csv", sep=";")
# Nos aseguramos de que la columna orderdate es de tipo Date
itemsSold$orderdate <- as.Date( itemsSold$orderdate, format="%d/%m/%y %H:%M" )
# Obtenemos los usuarios �nicos a partir de itemsSold
uniqueClients <- with( itemsSold, data.frame( iduser = sort(unique(iduser)) ) )
# A�adimos la columna recency con los d�as transcurridos desde la �ltima compra
uniqueClients <- cbind(
  uniqueClients,
  recency = aggregate( round( as.numeric( difftime( Sys.Date(), itemsSold$orderdate, units="days")) ), list(itemsSold$iduser), min )$x
)
# A�adimos la columna frecuency con el n�mero de compras realizadas
uniqueClients <- cbind(
  uniqueClients,
  frequency = with( itemsSold, as.numeric( by( orderdate, iduser, function(x) length(unique(x)) ) ) )
)
# A�adimos la columna monitery con el valor total de las compras
uniqueClients <- cbind(
  uniqueClients,
  monitery = with( itemsSold, as.numeric(by(unitprice,iduser,sum) ))
)
# Guardamos los datos RFM en un fichero CSV
write.csv( uniqueClients, file = "customers-rfm2.csv" )

hist(monitery, freq = FALSE)
curve(dnorm(x, mean(variable), sd(variable)), col = 2, lty = 2, lwd = 2, add=T)

#valores al azar de la distribuci�n normal
randNorm <- rnorm(300000)
#calculo de su densidad
randDensity <- dnorm(randNorm)
#gr�fica
library(ggplot2)
ggplot(data.frame(x = randNorm, y = randDensity)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Random Normal Variable", y = "Densidad")

