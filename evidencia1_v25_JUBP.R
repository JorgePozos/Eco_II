# Serie de Tiempo no estacionario
# precio al cierre de AMAZON

library(ggplot2)
library(tseries)
library(forecast)

AMZN <- read.csv("~/Downloads/AMZN.csv")
AMZN$precio <- AMZN$Adj.Close

AMZN$Date <- as.Date(AMZN$Date)
AMZN

ggplot(AMZN, aes(Date, precio)) + geom_line() + scale_x_date('dias') + ylab("Precio diario de AMAZON") + xlab("")
precio_ts = ts(AMZN[, c('precio')])
AMZN$precio = tsclean(precio_ts)

ggplot() +
  geom_line(data = AMZN, aes(x = Date, y = precio)) + ylab('Precio al cierre limpio')
AMZN$precio_ma = ma(AMZN$precio, order=7) # using the clean count with no outliers
AMZN$precio_ma30 = ma(AMZN$precio, order=30)


ggplot() +
  geom_line(data = AMZN, aes(x = Date, y = precio, colour = "Counts")) +
  geom_line(data = AMZN, aes(x = Date, y = precio_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = AMZN, aes(x = Date, y = precio_ma30, colour = "Monthly Moving Average"))  +
  ylab('apartamento Count')

precio_ma = ts(na.omit(AMZN$precio_ma), frequency=30)
decomp = stl(precio_ma, s.window="periodic")
deseasonal_precio <- seasadj(decomp)
plot(decomp)

# Estacionariedad 
adf.test(precio_ma, alternative = "stationary")
Acf(precio_ma, main='')
Pacf(precio_ma, main='')

# La hacemos estacionario 
count_d1 = diff(deseasonal_precio, differences = 18)
plot(count_d1)

# La probamos 
adf.test(count_d1, alternative = "stationary")
# <- rechazamos h0 porlo que la serie es estacionaria

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

modelo_arima <- auto.arima(deseasonal_precio, seasonal=FALSE)
modelo_arima

tsdisplay(residuals(modelo_arima), lag.max=10, main='(3,1,1) Model Residuals')

prediccion <- forecast(modelo_arima, h=30)
plot(prediccion)
