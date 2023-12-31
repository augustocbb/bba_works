

#Resposta da atividade: Regress�o M�ltipla e Dummies ###

library(AER)

data("CPS1988")

#quest�o 1:
summary(cps_q1 <- lm(log(wage)~education+experience+ethnicity+ethnicity*education, data=CPS1988))

#quest�o 2:
summary(cps_q2 <- lm(log(wage)~education+experience+I(experience^2)+ethnicity+ethnicity*education, data=CPS1988))
cf_q2 <- coef(cps_q2)
(pto_max=-cf_q2[3]/(2*cf_q2[4])) #n�vel m�ximo de experi�ncia

## Fim ##
