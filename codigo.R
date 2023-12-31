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

base <- read_excel("C:/Users/Augusto/Desktop/UFMG 2020_1/Series temporais/1� ativ pratica/base.xls")


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



#### Quest�o 1 #####
plot(base_dcp)
acf(base_dcp, lag.max = 36)
pacf(base_dcp, lag.max = 36)

# a s�rie parece n�o ser estacion�ria, apresentando tend�ncia de crescimento. O gr�fico acf indica forte correla��o entre os fatores
# e o gr�fico pacf indica presen�a de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a s�rie tem um fator AR(1)

plot(base_pib)
acf(base_pib, lag.max = 36)
pacf(base_pib, lag.max = 36)
# a s�rie parece n�o ser estacion�ria, apresentando tend�ncia de crescimento. O gr�fico acf indica forte correla��o entre os fatores
# e o gr�fico pacf indica presen�a de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a s�rie tem um fator AR(1)




plot(base_rpd)
acf(base_rpd, lag.max = 36)
pacf(base_rpd, lag.max = 36)

# a s�rie parece n�o ser estacion�ria, apresentando tend�ncia de crescimento. O gr�fico acf indica forte correla��o entre os fatores
# e o gr�fico pacf indica presen�a de um lag de ordem 1 fora das bandas indicando que provavelmente  exite um fator auto regressivo. Aparamentemente a s�rie tem um fator AR(1)



plot(base_div)
acf(base_div, lag.max = 36)
pacf(base_div, lag.max = 36)

# a s�rie parece n�o ser estacion�ria,
# apresentando tend�ncia de crescimento. 
# O gr�fico acf indica forte correla��o entre os fatores
# e o gr�fico pacf indica presen�a de um lag de ordem 1 
# e dois pontos alternados pr�ximos ao per�odo 12 fora das bandas 
# indicando que provavelmente  exite um fator auto regressivo 
# e um fator autoregressivvo sazonal 
# e tamb�m de sazonalidade provavelmente de ordem MA 1. 
# Aparamentemente a s�rie tem um fator SARima(1,1,0)[1,0,1]


plot(base_lc)
acf(base_lc, lag.max = 36)
pacf(base_lc, lag.max = 36)

# a s�rie aparentemente n�o apresenta estacionariedade e uma tend~encia de crescimento
# Ao observar o gr�fico ACF podemos verificar que existe uma tend�ncia a linearidade da autocorrela��o 
# dos fatores a partr do lag 15 o que indica uma mudan�a nos fatores.
# O gr�fico pacf apresenta um pico no primero lag e alguma pertuba��es 
# at� o lag 12 onde ha novo pico com repeti��o 6 lags depois e com o padr�o se repetindo 
# mais 12 lags depois. 
# Podemos supor um componente AR de ordem 1, um componenete de m�dias m�veis para explicar as pequenas pertuba��es
# at� o lag 12, um componente sazonal autoregressivo e outro de m�dias m�veis. 
# Al�m disso podemos perceber uma tend�ncia de crescimento
# Dessa forma o modelo inicial seria um SARIMA(1,1,1)[1,0,1]


#### Quest�o 2 #####

 
adf.test(base_rpd)


adf.test(base_pib)


adf.test(base_dcp)


adf.test(base_lc)


adf.test(base_div)

# 
# #todos os testes apresentaram valor p acima de 95%, dessa forma em todas as s�ries podemos 
# rejeitar a hip�tese alternativa e dessa forma afirma que a s�rie possui pelo menos uma raiz dentro
# do c�rculo unit�rio. Dessa forma podemos supor uma n�o estacionariedade da s�rie

#### Quest�o 3 #####

pp.test(base_rpd)


pp.test(base_pib)


pp.test(base_dcp)


pp.test(base_lc)


pp.test(base_div)

# 
# #Os testes continuam apresentando n�o estacionariedade com valor p em torno de 0.99
# Caso os resultados fossem contradit�rios seria interessante realizar outros testes mais modernos como kpss. 
# Por�m na situa��o de ser necess�ria a escolha entre um dos dois seria ideal considerar o teste PP pois trata
# erros correlacionados e heteroced�sticos enquando o ADF nao

#### Quest�o 4 #####

q4 <- lm(data = base2,formula = DIV ~ LC, )

summary(q4)

plot(q4)

# Podemos observar que existe rela��o estat�tica a partir dos resultados apresentados.
# isso pode ser explicado pela raz�o de ambos os fatores serem em fun��o de um terceiro fator em comum
# Esse fator pode ser uma combina��o de efeitos econ�micos que afetam o resultado das empresas 
# como a Infla��o e os ciclos Econ�micos. Ao regredir uma vari�vel contra a outra podemos assumir que
# s fatores no longo per�odo estudado ter�o alta relev�ncia no vaor final das s�ries.


#### Quest�o 5 #####
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

# podemos observar que as bases de primeiras diferen�as das s�ries 
# passaram a ter uma tend�ncia menos evidente e aparentemente pussuem estacionariedade em torno da m�dia 0
# Por�m nas series pib, dcp e div ainda aparentemente apresentam uma ligerira tendencia ap�s a observa��o 200
#b)

plot(base_diff_rpd)
acf(base_diff_rpd, lag.max = 36)
pacf(base_diff_rpd, lag.max = 36)

# podemos observar no grafico acf a existencia de forte correla��o com o primeiro lag, 
# porem no pacf podemos supor tamb�m a exist�ncia e um fator de m�dias m�veis para o segundo lag

plot(base_diff_pib)
acf(base_diff_pib, lag.max = 36)
pacf(base_diff_pib, lag.max = 36)

# podemos identificar forte correla��o para os tr�s primeiros lags no grafico acf e pontos espassos em alguns 
# lags depois. J� no pacf podemos observar pontos relevantes 8 e 12 periodos depois sugerindo um componente sazonal
# al�m dos doi primeiros lags com forte correla��o

plot(base_diff_dcp)
acf(base_diff_dcp, lag.max = 36)
pacf(base_diff_dcp, lag.max = 36)
# 
# O grafico acf possui forte componente no primeiro lag e crescente componente at� o periodo 4 
# J� o gr�fico pacf possui os tres primeiros lags evidentes sugerindo componentes de um ar 3 ou de m�dias m�veis 

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
# O acf sugere forte correla��o no primeiro lag e pontos de relevancia nos lags 3 e 4 e seus pares sazonais de 4 periodos
# Ja o pacf sugere relevancia nos lags 2 e 3 e 8 sugerindo um componente sazonal de medias moveis e outro de m�dias m�veis no mesmo periodo



#### Quest�o 6 #####

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



# A regress�o possui baixo car�ter explicativo para a s�rie. Isso pode ser observardor pelo R2,
# valor p e estatistica f. Ao an�lisar o intercepto observamos alta signific�ncia estat�tica
# Isso provavelmente � devido ao fato de ambas as s�ries terem tend�ncias crescentes e dessa forma o
# intercepto representar nada mais que a tend�ncia de crescimento da economia.
# Ao analisar os res�duos podemos verificar que eles s�o estacion�rios tanto no teste adf quanto no teste PP
# Podemos concluir por fim que ao contr�rio do identificado no teste anterior, onde havia correla��o esp�ria entre as
# s�ries, ao regredir as primeiras diferen�as, observamos que as varia��es n�o possuem rela��o
# tendo comportamentos pouco relacionados fora a tend�ncia de crescimento e do aceit�vel para infer�ncias estat�ticas.

#### Quest�o 7 #####
 
# 1. passo teste de estacioanriedade

adf.test(base_rpd)
pp.test(base_rpd)
 
# conclus�o n�o estacion�rio
# 
# 2. Teste de estacionariedade para primeiras diferen�as



base_diff_rpd <-  diff(base_rpd)

adf.test(base_diff_rpd)
pp.test(base_diff_rpd)

# Conclus�o Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_rpd)
 
# Conclus�o Aparentemente h� sazonalidade e fatores de m�dias m�veis e autoregressivos
 
# 4. Teste modelo arima utilizando maximiza��o de otimiza��o coputacional

arima_rpd <- auto.arima(base_rpd , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_rpd)

# 5. Checando residuos

checkresiduals(arima_rpd)
pacf(arima_rpd$residuals)

shapiro.test(arima_rpd$residuals)

kpss.test(arima_rpd$residuals)

# Conclus�o: Res�duos normais e n�o correlacionados e estacion�rios

plot(forecast(arima_rpd, h = 4))

#### Quest�o 8 #####

# 1. passo teste de estacioanriedade

adf.test(base_pib)
pp.test(base_pib)

# conclus�o n�o estacion�rio
# 
# 2. Teste de estacionariedade para primeiras diferen�as


ndiffs(base_pib)

base_diff_pib <-  diff(base_pib)

adf.test(base_diff_pib)
pp.test(base_diff_pib)

# Conclus�o Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_pib)

# Conclus�o Aparentemente h� sazonalidade e fatores de m�dias m�veis e autoregressivos

# 4. Teste modelo arima utilizando maximiza��o de otimiza��o coputacional

arima_pib <- auto.arima(base_pib , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_pib)

# 5. Checando residuos

checkresiduals(arima_pib)
pacf(arima_pib$residuals)

shapiro.test(arima_pib$residuals)

kpss.test(arima_pib$residuals)

# Conclus�o: Res�duos normais e n�o correlacionados e estacion�rios

plot(forecast(arima_pib, h = 4))


#### Quest�o 9 #####

# 1. passo teste de estacioanriedade

adf.test(base_dcp)
pp.test(base_dcp)

# conclus�o n�o estacion�rio
# 
# 2. Teste de estacionariedade para primeiras diferen�as


ndiffs(base_dcp)

base_diff_dcp <-  diff(base_dcp)

adf.test(base_diff_dcp)
pp.test(base_diff_dcp)

# Conclus�o Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_dcp)

# Conclus�o Aparentemente h� sazonalidade e fatores de m�dias m�veis e autoregressivos

# 4. Teste modelo arima utilizando maximiza��o de otimiza��o coputacional

arima_dcp <- auto.arima(base_dcp , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_dcp)

# 5. Checando residuos

checkresiduals(arima_dcp)
pacf(arima_dcp$residuals)

shapiro.test(arima_dcp$residuals)

kpss.test(arima_dcp$residuals)

# Conclus�o: Res�duos normais e n�o correlacionados e estacion�rios


plot(forecast(arima_dcp, h = 4))


#### Quest�o 10 #####

# 1. passo teste de estacioanriedade

adf.test(base_lc)
pp.test(base_lc)

# conclus�o n�o estacion�rio
# 
# 2. Teste de estacionariedade para primeiras diferen�as


ndiffs(base_lc)

base_diff_lc <-  diff(base_lc)

adf.test(base_diff_lc)
pp.test(base_diff_lc)

# Conclus�o Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_lc)

# Conclus�o Aparentemente h� sazonalidade e fatores de m�dias m�veis e autoregressivos

# 4. Teste modelo arima utilizando maximiza��o de otimiza��o coputacional

arima_lc <- auto.arima(base_lc , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_lc)

# 5. Checando residuos

checkresiduals(arima_lc)
pacf(arima_lc$residuals)

shapiro.test(arima_lc$residuals)

kpss.test(arima_lc$residuals)

# Conclus�o: Res�duos normais e n�o correlacionados e estacion�rios

plot(forecast(arima_lc, h = 4))


#### Quest�o 11 #####

# 1. passo teste de estacioanriedade

adf.test(base_div)
pp.test(base_div)

# conclus�o n�o estacion�rio
# 
# 2. Teste de estacionariedade para primeiras diferen�as


ndiffs(base_div)

base_diff_div <-  diff(base_div)

adf.test(base_diff_div)
pp.test(base_diff_div)

# Conclus�o Estacionario
# 
# 3. Analise ACF e PACF

tsdisplay(base_diff_div)

# Conclus�o Aparentemente h� sazonalidade e fatores de m�dias m�veis e autoregressivos

# 4. Teste modelo arima utilizando maximiza��o de otimiza��o coputacional

arima_div <- auto.arima(base_div , stepwise = FALSE, approximation = FALSE, trace = T)

summary(arima_div)

# 5. Checando residuos

checkresiduals(arima_div)
pacf(arima_div$residuals)

shapiro.test(arima_div$residuals)

kpss.test(arima_div$residuals)
adf.test(arima_div$residuals)
pp.test(arima_div$residuals)

# Conclus�o: Res�duos normais e n�o correlacionados e estacion�rios

plot(forecast(arima_div, h = 4))
