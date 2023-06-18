library(readxl)
library(knitr)
spin('script trabalho.R', precious = TRUE, doc = '#')

## Estimation using dummiemin considering
## dividend distribution only for companies with net profit
library(plm)
library(lmtest)

# Read data from an Excel file
dados <- read_excel(file.choose(),
                    col_types = c("text", "date", "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric"))

# Prepare panel data structure
pgr <- plm.data(dados, index = c("nome", "data"))

# MQO (Pooled):
gr_pool <- lm(dummiemin ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
              data = pgr)
summary(gr_pool)
dwtest(gr_pool$residuals ~ gr_pool)

# Estimator between groups:
gr_entre <- plm(dummiemin ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
                data = pgr, model = "between")

# Fixed effects:
gr_fe <- plm(dummiemin ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
             data = pgr, model = "within")
summary(gr_fe)
summary(fixef(gr_fe))

# F-test for fixed effects:
pFtest(gr_fe, gr_pool)

# Random effects:
gr_re <- plm(dummiemin ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
             data = pgr, model = "random", random.method = "walhus")
summary(gr_re)
jarqueberaTest(gr_fe)
resettest(gr_re)

# LM test for random effects:
plmtest(gr_pool)

# Hausman test:
phtest(gr_re, gr_fe)

##########################################################

## Estimation using dummiemin2 considering
## dividend distribution for all companies with or without net profit

# Subset the data to use a subset of 3 firms:
gr <- subset(Grunfeld, firm %in% c("General Electric", "General Motors", "IBM"))
head(gr)
head(dados)

# Prepare panel data structure
pgr <- plm.data(dados, index = c("nome", "data"))

# MQO (Pooled):
gr_pool <- plm(dummiemin2 ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
               data = pgr, model = "pooling")
summary(gr_pool)

# Estimator between groups:
gr_entre <- plm(dummiemin2 ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
                data = pgr, model = "between")

# Fixed effects:
gr_fe <- plm(dummiemin2 ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
             data = pgr, model = "within")
summary(gr_fe)
summary(fixef(gr_fe))

# F-test for fixed effects:
pFtest(gr_fe, gr_pool)

# Random effects:
gr_re <- plm(dummiemin2 ~ roe + endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat,
             data = pgr, model = "random", random.method = "walhus")
summary(gr_re)

# LM test for random effects:
plmtest(gr_pool)

# Hausman test:
phtest(gr_re, gr_fe)

######### Robustness Test ################################

## Estimation using divpl considering
## the percentage of dividend distributed to shareholders
## over the net worth of companies with positive ebitda
## controlling for the dummy of minimum dividend distribution
library(plm)

# Prepare panel data structure
pgr <- plm.data(dados, index = c("nome", "data"))

# MQO (Pooled):
gr_pool <- plm(divpl ~ dummiemin * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
               data = pgr, model = "pooling")
summary(gr_pool)

# Estimator between groups:
gr_entre <- plm(divpl ~ dummiemin * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
                data = pgr, model = "between")

# Fixed effects:
gr_fe <- plm(divpl ~ dummiemin * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
             data = pgr, model = "within")
summary(gr_fe)
summary(fixef(gr_fe))

# F-test for fixed effects:
pFtest(gr_fe, gr_pool)

# Random effects:
gr_re <- plm(divpl ~ dummiemin * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
             data = pgr, model = "random", random.method = "walhus")
summary(gr_re)
names(dados)

# LM test for random effects:
plmtest(gr_pool)

# Hausman test:
phtest(gr_re, gr_fe)

## Estimation using divpl considering
## the percentage of dividend distributed to shareholders
## over the net worth of companies with positive ebitda
## controlling for the dummy of minimum dividend distribution
library(plm)

# Prepare panel data structure
pgr <- plm.data(dados, index = c("nome", "data"))

# MQO (Pooled):
gr_pool <- plm(divpl ~ dummiemin2 * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
               data = pgr, model = "pooling")
summary(gr_pool)

# Estimator between groups:
gr_entre <- plm(divpl ~ dummiemin2 * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
                data = pgr, model = "between")

# Fixed effects:
gr_fe <- plm(divpl ~ dummiemin2 * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
             data = pgr, model = "within")
summary(gr_fe)
summary(fixef(gr_fe))

# F-test for fixed effects:
pFtest(gr_fe, gr_pool)

# Random effects:
gr_re <- plm(divpl ~ dummiemin2 * (+endividamento + investimento + tamanho + liquidez + instreceita + concentracao + eat),
             data = pgr, model = "random", random.method = "walhus")
summary(gr_re)
names(dados)

# LM test for random effects:
plmtest(gr_pool)

# Hausman test:
phtest(gr_re, gr_fe)
