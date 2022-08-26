# VIDEOCONFERENCIA 8
# Jorge U. Barragan Pozos 
library(nnet)
library(haven)

nels_small <- read_dta("Downloads/nels_small.dta")
attach(nels_small)

nels.multinom <- multinom(psechoice~grades, data = nels_small )
summary(nels.multinom)

medGrades <- median(grades)
fifthPercentilGrades <- quantile(grades,.05)
newdat <- data.frame(grades=c(medGrades, fifthPercentilGrades))
pred <- predict(nels.multinom, newdat, 'probs')
pred