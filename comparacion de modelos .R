# Ejercicio 6.17
library(readxl)
library(dplyr)
library(stargazer)
library(knitr)
library(broom)
library(haven)
library(stats)
library(lmtest)


# Se compararan 5 modelos usando la sig. base de datos 
br5 <- read_excel("Downloads/br5.xlsx")
attach(br5)

# Agregamos una nueva columna ln(PRICE)
br5 <- br5 %>%
  mutate(ln_price = log(price),
         age_sqr = age^2 )

# Se definen los Modelos 
mod2 <- lm(ln_price ~ sqft + age + I(age^2), br5)
mod9 <- lm(ln_price ~ sqft + age + I(age^2) + baths, br5)
mod10 <- lm(ln_price ~ sqft + age + I(age^2) + baths + bedrooms, br5)
mod11 <- lm(ln_price ~ sqft + age + I(age^2) + baths + bedrooms:sqft, br5)
mod12 <- lm(
  ln_price ~ sqft + age + I(age^2) + baths + bedrooms:sqft + baths:sqft,
  br5)

# Comparacion de Modelos 
stargazer(mod2, mod9, mod10, mod11, mod12,
          type="text", title = "Comparación modelos", 
          align = TRUE, out ="tabla1.txt", flip = TRUE, 
          digits = 1)

# Estadisticos de informacion 
r2 <- as.numeric(glance(mod2))
r9 <- as.numeric(glance(mod9))
r10 <- as.numeric(glance(mod10))
r11 <- as.numeric(glance(mod11))
r12 <- as.numeric(glance(mod12))

tab <- data.frame(rbind(r2 , r9, r10, r11, r12 ))[,c(1,2,8,9)]
kable(tab,caption="Comparación de los modelos, price ", digits=4,col.names=c("Rsq","AdjRsq","AIC","BIC"))

