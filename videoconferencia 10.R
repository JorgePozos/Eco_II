# VIDEOCONFERENCIA 10 

library(dynlm)
library(orcutt)
library(nlWaldTest)
library(zoo)
library(pdfetch)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(haven)
library(readxl)

usmacro <- read_excel("Downloads/usmacro.xlsx")
attach(usmacro)

usmacro_ts <- ts(usmacro,
                 start = c(1948,1), 
                 end = c(2016,1), 
                 frequency = 4)
plot.ts(g)
plot.ts(u)
plot.ts(inf)

