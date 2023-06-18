install.packages("installr")

install.packages("vars")

install.packages("openxlsx")

install.packages("gtools")
install.packages("readxl")

# For obtaining GDP data
install.packages("WDI")

install.packages("plm")
install.packages("sqldf")

# For manipulating data
install.packages("magrittr")

install.packages("rprojroot")
install.packages("forcats")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
# For descriptive statistics and graphing
install.packages("skimr")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")
install.packages("forecast")
install.packages("fpp2")
install.packages("readxl")
install.packages("stringr")
install.packages("FitAR")
install.packages("gtrendsR")
install.packages("quantmod")
install.packages("gtrends")

install.packages("BatchGetSymbols")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("Amelia")
install.packages("reshape2")
install.packages("ggthemes") 
install.packages("plyr")
install.packages("tseries")
install.packages("plm")





library(installr)

library (vars)

library(openxlsx)

library(gtools)
library(readxl)

# For obtaining GDP data
library(WDI)

library(plm)
library(sqldf)

# For manipulating data
library(magrittr)

library(rprojroot)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
# For descriptive statistics and graphing
library(skimr)
library(ggplot2)
library(scales)
library(gridExtra)
library(forecast)
library(fpp2)
library(readxl)
library(stringr)
library(FitAR)
library(gtrendsR)
library(quantmod)
library(gtrends)

library(BatchGetSymbols)
library(tidyverse)
library(ggplot2)
library(Amelia)
library(reshape2)
library(ggthemes) 
library(plyr)
library(tseries)
library(plm)

## importar base

base <- read_excel("C:/Users/Augusto/Desktop/UFMG 2020_1/Series temporais/1ª ativ pratica/base.xls")


base2 <- as.data.frame( base)

base_rpd <-
  ts(
    base2$RPD,
    start = c(1947, 1),
    frequency = 4
    
  )


base_pib <-
  ts(
    base$PIB,
    start = c(min(base$Ano), 1),
    end = c(max(base$Ano), 4),
    frequency = 4
  )

base_dcp <-
  ts(
    base$DCP,
    start = c(min(base$Ano), 1),
    end = c(max(base$Ano), 4),
    frequency = 4
  )

base_lc <-
  ts(
    base$LC,
    start = c(min(base$Ano), 1),
    end = c(max(base$Ano), 4),
    frequency = 4
  )

base_div <-
  ts(
    base$DIV,
    start = c(1947, 1),
    end = c(2007, 4),
    frequency = 4
  )


base3 <- subset(base, select = c(RPD, PIB, DCP, LC, DIV) )



#### Questão 1 #####
plot(base_dcp)
acf(base_dcp, lag.max = 36)
pacf(base_dcp, lag.max = 36)

# a série parece não ser estacionária, apresentando tendência de crescimento. O gráfico acf indica forte correlação entre os fatores
# e o gráfico pacf indica presença de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a série tem um fator AR(1)

plot(base_pib)
acf(base_pib, lag.max = 36)
pacf(base_pib, lag.max = 36)
# a série parece não ser estacionária, apresentando tendência de crescimento. O gráfico acf indica forte correlação entre os fatores
# e o gráfico pacf indica presença de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a série tem um fator AR(1)




plot(base_rpd)
acf(base_rpd, lag.max = 36)
pacf(base_rpd, lag.max = 36)

# a série parece não ser estacionária, apresentando tendência de crescimento. O gráfico acf indica forte correlação entre os fatores
# e o gráfico pacf indica presença de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a série tem um fator AR(1)



plot(base_div)
acf(base_div, lag.max = 36)
pacf(base_div, lag.max = 36)

# a série parece não ser estacionária,
# apresentando tendência de crescimento. 
# O gráfico acf indica forte correlação entre os fatores
# e o gráfico pacf indica presença de um lag de ordem 1 
# e dois pontos alternados próximos ao período 12 fora das bandas 
# indicando que provavelmente  exite um fator auto regressivo 
# e um fator autoregressivvo sazonal 
# e também de sazonalidade provavelmente de ordem MA 1. 
# Aparamentemente a série tem um fator SARima(1,1,0)[1,0,1]


plot(base_lc)
acf(base_lc, lag.max = 36)
pacf(base_lc, lag.max = 36)

# a série aparentemente não apresenta estacionariedade e uma tend~encia de crescimento
# Ao observar o gráfico ACF podemos verificar que existe uma tendência a linearidade da autocorrelação 
# dos fatores a partr do lag 15 o que indica uma mudança nos fatores.
# O gráfico pacf apresenta um pico no primero lag e alguma pertubações 
# até o lag 12 onde ha novo pico com repetição 6 lags depois e com o padrão se repetindo 
# mais 12 lags depois. 
# Podemos supor um componente AR de ordem 1, um componenete de mádias móveis para explicar as pequenas pertubações
# até o lag 12, um componente sazonal autoregressivo e outro de médias móveis. 
# Além disso podemos perceber uma tendência de crescimento
# Dessa forma o modelo inicial seria um SARIMA(1,1,1)[1,0,1]


#### Questão 2 #####

 
adf.test(base_rpd)


adf.test(base_pib)


adf.test(base_dcp)


adf.test(base_lc)


adf.test(base_div)

# 
# #todos os testes apresentaram valor p acima de 95%, dessa forma em todas as séries podemos 
# rejeitar a hipótese alternativa e dessa forma afirma que a série possui pelo menos uma raiz dentro
# do círculo unitário. Dessa forma podemos supor uma não estacionariedade da série

#### Questão 3 #####

pp.test(base_rpd)


pp.test(base_pib)


pp.test(base_dcp)


pp.test(base_lc)


pp.test(base_div)

# 
# #Os testes continuam apresentando não estacionariedade com valor p em torno de 0.99
# Caso os resultados fossem contraditórios seria interessante realizar outros testes mais modernos como kpss. 
# Porém na situação de ser necessária a escolha entre um dos dois seria ideal considerar o teste PP pois trata
# erros correlacionados e heterocedásticos enquando o ADF nao

#### Questão 4 #####

q4 <- lm(data = base2,formula = DIV ~ LC, )

summary(q4)

plot(q4)

# Podemos observar que existe relação estatítica a partir dos resultados apresentados.
# isso pode ser explicado pela razão de ambos os fatores serem em função de um terceiro fator em comum
# Esse fator pode ser uma combinação de efeitos econõmicos que afetam o resultado das empresas 
# como a Inflação e os ciclos Econõmicos. Ao regredir uma variável contra a outra podemos assumir que
# s fatores no longo período estudado terão alta relevância no vaor final das séries.


#### Questão 5 #####
#A)
base_diff_rpd  <- diff(base_rpd, diferences = 1)

base_diff_pib  <- diff(base_pib, diferences = 1)

base_diff_dcp  <- diff(base_dcp, diferences = 1)

base_diff_lc  <- diff(base_lc, diferences = 1)

base_diff_div  <- diff(base_div, diferences = 1)

plot(base_diff_rpd)

plot(base_diff_pib)

plot(base_diff_dcp)

plot(base_diff_lc)

plot(base_diff_div)

# podemos observar que as bases de primeiras diferenças das séries 
# passaram a ter uma tendência menos evidente e aparentemente pussuem estacionariedade em torno da média 0
# Porém nas series pib, dcp e div ainda aparentemente apresentam uma ligerira tendencia após a observação 200
#b)

plot(base_diff_rpd)
acf(base_diff_rpd, lag.max = 36)
pacf(base_diff_rpd, lag.max = 36)

# podemos observar no grafico acf a existencia de forte correlação com o primeiro lag, 
# porem no pacf podemos supor também a existência e um fator de médias móveis para o segundo lag

plot(base_diff_pib)
acf(base_diff_pib, lag.max = 36)
pacf(base_diff_pib, lag.max = 36)

# podemos identificar forte correlação para os três primeiros lags no grafico acf e pontos espassos em alguns 
# lags depois. Já no pacf podemos observar pontos relevantes 8 e 12 periodos depois sugerindo um componente sazonal
# além dos doi primeiros lags com forte correlação

plot(base_diff_dcp)
acf(base_diff_dcp, lag.max = 36)
pacf(base_diff_dcp, lag.max = 36)
# 
# O grafico acf possui forte componente no primeiro lag e crescente componente até o periodo 4 
# Já o gráfico pacf possui os tres primeiros lags evidentes sugerindo componentes de um ar 3 ou de médias móveis 

plot(base_diff_lc)
acf(base_diff_lc, lag.max = 36)
pacf(base_diff_lc, lag.max = 36)
# 
# O grafico acf sugere relevancia no primeiro e segundo componente e forte relevancia nos lags 
# 4 5 6 e 7 sugerindo uma sazonalidade anual caracterizada por medias moveis

plot(base_diff_div)
acf(base_diff_div, lag.max = 36)
pacf(base_diff_div, lag.max = 36)
# 
# O acf sugere forte correlação no primeiro lag e pontos de relevancia nos lags 3 e 4 e seus pares sazonais de 4 periodos
# Ja o pacf sugere relevancia nos lags 2 e 3 e 8 sugerindo um componente sazonal de medias moveis e outro de médias móveis no mesmo periodo



#### Questão 6 #####

base_q6 <- subset.data.frame(base, select = c("Ano", "Trimestre"))
base_q6 <- base_q6[-1, ]
base_q6$DIV <- diff(base_div, differences = 1)
base_q6$LC <- diff(base_lc, differences = 1)

q6 <- lm(data = base3,formula = DIV ~ LC, )

summary(q6)

plot(q6)

shapiro.test(q6$residuals)
adf.test(q6$residuals)
pp.test(q6$residuals)



# A regressão possui baixo caráter explicativo para a série. Isso pode ser observardor pelo R2,
# valor p e estatistica f. Ao análisar o intercepto observamos alta significância estatítica
# Isso provavelmente é devido ao fato de ambas as séries terem tendências crescentes e dessa forma o
# intercepto representar nada mais que a tendência de crescimento da economia.
# Ao analisar os resíduos podemos verificar que eles são estacionários tanto no teste adf quanto no teste PP
# Podemos concluir por fim que ao contrário do identificado no teste anterior, onde havia correlação espúria entre as
# séries, ao regredir as primeiras diferenças, observamos que as variações não possuem relação
# tendo comportamentos pouco relacionados fora a tendência de crescimento e do aceitável para inferências estatíticas.

#### Questão 7 #####
 
# 1. passo teste de estacioanriedade

adf.test(base_rpd)
pp.test(base_rpd)
 
# conclusão não estacionário
# 
# 2. Teste de estacionariedade para primeiras diferenças



base_diff_rpd <-  diff(base_rpd)

adf.test(base_diff_rpd)
pp.test(base_diff_rpd)

# Conclusão Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_rpd)
 
# Conclusão Aparentemente há sazonalidade e fatores de médias móveis e autoregressivos
 
# 4. Teste modelo arima utilizando maximização de otimização coputacional

arima_rpd <- auto.arima(base_rpd , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_rpd)

# 5. Checando residuos

checkresiduals(arima_rpd)
pacf(arima_rpd$residuals)

shapiro.test(arima_rpd$residuals)

kpss.test(arima_rpd$residuals)

# Conclusão: Resíduos normais e não correlacionados e estacionários

plot(forecast(arima_rpd, h = 4))

#### Questão 8 #####

# 1. passo teste de estacioanriedade

adf.test(base_pib)
pp.test(base_pib)

# conclusão não estacionário
# 
# 2. Teste de estacionariedade para primeiras diferenças


ndiffs(base_pib)

base_diff_pib <-  diff(base_pib)

adf.test(base_diff_pib)
pp.test(base_diff_pib)

# Conclusão Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_pib)

# Conclusão Aparentemente há sazonalidade e fatores de médias móveis e autoregressivos

# 4. Teste modelo arima utilizando maximização de otimização coputacional

arima_pib <- auto.arima(base_pib , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_pib)

# 5. Checando residuos

checkresiduals(arima_pib)
pacf(arima_pib$residuals)

shapiro.test(arima_pib$residuals)

kpss.test(arima_pib$residuals)

# Conclusão: Resíduos normais e não correlacionados e estacionários

plot(forecast(arima_pib, h = 4))


#### Questão 9 #####

# 1. passo teste de estacioanriedade

adf.test(base_dcp)
pp.test(base_dcp)

# conclusão não estacionário
# 
# 2. Teste de estacionariedade para primeiras diferenças


ndiffs(base_dcp)

base_diff_dcp <-  diff(base_dcp)

adf.test(base_diff_dcp)
pp.test(base_diff_dcp)

# Conclusão Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_dcp)

# Conclusão Aparentemente há sazonalidade e fatores de médias móveis e autoregressivos

# 4. Teste modelo arima utilizando maximização de otimização coputacional

arima_dcp <- auto.arima(base_dcp , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_dcp)

# 5. Checando residuos

checkresiduals(arima_dcp)
pacf(arima_dcp$residuals)

shapiro.test(arima_dcp$residuals)

kpss.test(arima_dcp$residuals)

# Conclusão: Resíduos normais e não correlacionados e estacionários


plot(forecast(arima_dcp, h = 4))


#### Questão 10 #####

# 1. passo teste de estacioanriedade

adf.test(base_lc)
pp.test(base_lc)

# conclusão não estacionário
# 
# 2. Teste de estacionariedade para primeiras diferenças


ndiffs(base_lc)

base_diff_lc <-  diff(base_lc)

adf.test(base_diff_lc)
pp.test(base_diff_lc)

# Conclusão Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_lc)

# Conclusão Aparentemente há sazonalidade e fatores de médias móveis e autoregressivos

# 4. Teste modelo arima utilizando maximização de otimização coputacional

arima_lc <- auto.arima(base_lc , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_lc)

# 5. Checando residuos

checkresiduals(arima_lc)
pacf(arima_lc$residuals)

shapiro.test(arima_lc$residuals)

kpss.test(arima_lc$residuals)

# Conclusão: Resíduos normais e não correlacionados e estacionários

plot(forecast(arima_lc, h = 4))


#### Questão 11 #####

# 1. passo teste de estacioanriedade

adf.test(base_div)
pp.test(base_div)

# conclusão não estacionário
# 
# 2. Teste de estacionariedade para primeiras diferenças


ndiffs(base_div)

base_diff_div <-  diff(base_div)

adf.test(base_diff_div)
pp.test(base_diff_div)

# Conclusão Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_div)

# Conclusão Aparentemente há sazonalidade e fatores de médias móveis e autoregressivos

# 4. Teste modelo arima utilizando maximização de otimização coputacional

arima_div <- auto.arima(base_div , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_div)

# 5. Checando residuos

checkresiduals(arima_div)
pacf(arima_div$residuals)

shapiro.test(arima_div$residuals)

kpss.test(arima_div$residuals)
adf.test(arima_div$residuals)
pp.test(arima_div$residuals)

# Conclusão: Resíduos normais e não correlacionados e estacionários

plot(forecast(arima_div, h = 4))
