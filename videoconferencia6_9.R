library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(AER)
library(xtable)
library(haven)
library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(AER)
library(xtable)

transport <- read_dta("Downloads/transport.dta")
attach(transport)

#JORGE URIEL BARRAGAN POZOS 
#Veamos la distribucion normal 
x <- seq( -3,3,.2)
plot(x,pnorm(x))
plot(x,dnorm(x))

auto.probit <- glm(auto~dtime,family = binomial(link = "probit"),data = transport)
kable(tidy(auto.probit),digits = 4,align = "c", caption = "Modelo PROBIT del transporte" )#Efectos marginales

xdtime<- data.frame(dtime=2)
EMdtime<- predict(auto.probit, xdtime, type = "link")
CEMdtime<- coef(auto.probit)[[2]]*dnorm(EMdtime)
CEMdtime

#Estimación de la probabilidad
xdtime<- data.frame(dtime=3)
EPdtime<- predict(auto.probit,xdtime,data=transport,type="response")
EPdtime

#Calcular AME
EAME<-predict(auto.probit,type = "link")
EAMEm<- mean(dnorm(EAME))
AME<- EAMEm*coef(auto.probit)
AME

#EM con 5 minutos de Diferencia
xdtime5<- data.frame(dtime=0.5)
EMdtime5<- predict(auto.probit, xdtime, type = "link")
CEMdtime<- coef(auto.probit)[[2]]*dnorm(EMdtime5)
CEMdtime

#Si el incremento en la distancia en 5 minutos entonces la probabilidad incrementa 8.44%
#Estimación puntual con 5 min de diferencia
xdtime5<- data.frame(dtime=0.5)
EPdtime<- predict(auto.probit,xdtime5,data=transport,type="response")
EPdtime

#Si la distancia entre bus y auto es de 5 minutos la probabilidad de escoger el auto es 53.40%


# VIDEOCONFERENCIA 7 
library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(AER)
library(xtable)
library(haven)
library(stargazer)
coke <- read_dta("C:/Users/sasa_/OneDrive - Benemérita Universidad Autónoma de Puebla/econometria 1/base de datos stata/stata/coke.dta")
attach(coke)

coke.logit<-glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link="logit"),data=coke)
kable(tidy(coke.logit),digits=5,align="c",caption="Mod Logit")

coke.LPM<-lm(coke~pratio+disp_coke+disp_pepsi,data=coke)
coke.probit<-glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link="probit"),data=coke)


stargazer(coke.LPM, coke.probit, coke.logit, header=FALSE, title="Comparación de modelos binarios", type= "text", keep.stat="n",digits=4, single.row=FALSE, intercept.bottom=FALSE, model.names=FALSE, column.labels=c("LPM","probit","logit"))


#Pruebas de hip
Hnull<-"disp_coke+disp_pepsi=0"
linearHypothesis(coke.logit,Hnull)

Hnull<-c("disp_coke=0","disp_pepsi=0")
linearHypothesis(coke.logit,Hnull)

# JORGE U. BARRAGAN POZOS 
# Modelo Tobit 

mroz <- read_dta("~/Downloads/mroz.dta")
attach(mroz)

hist( hours, breaks = 20 , col = 'green')
library(AER)
mroz.tobit <- tobit(hours ~ educ + exper + age + kidsl6 ,
                    data = mroz)
sMrozTobit <- summary(mroz.tobit)
sMrozTobit

# Efecto Marginal 
xEduc <- 12.29
xExper <- 10.63
xAge <- 42.54
xKids <- 1 
bInt <- coef(mroz.tobit)[[1]]
bEduc <- coef(mroz.tobit)[[2]]
bExper <- coef(mroz.tobit)[[3]]
bAge <- coef(mroz.tobit)[[4]]
bKids <- coef(mroz.tobit)[[5]]

bSigma <- mroz.tobit$scale
Phactor <- pnorm((bInt+bEduc*xEduc+bExper*xExper+bAge*xAge+bKids*xKids)/bSigma)
DhoursDeduc <- bEduc*Phactor

