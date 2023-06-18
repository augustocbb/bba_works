

#Resposta da atividade: Regressão Múltipla e Dummies ###

library(AER)

data("CPS1988")

#questão 1:
summary(cps_q1 <- lm(log(wage)~education+experience+ethnicity+ethnicity*education, data=CPS1988))

#questão 2:
summary(cps_q2 <- lm(log(wage)~education+experience+I(experience^2)+ethnicity+ethnicity*education, data=CPS1988))
cf_q2 <- coef(cps_q2)
(pto_max=-cf_q2[3]/(2*cf_q2[4])) #nível máximo de experiência

## Fim ##
