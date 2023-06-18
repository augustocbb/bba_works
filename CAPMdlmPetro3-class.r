require(dlm)

petro3=read.csv("Pet3.csv", header = F, sep = ";", dec=",")
selic=read.csv("selic.csv", header = F, sep = ";", dec=",")
ibovespa=read.csv("Ibovespa.csv", header = F, sep = ";", dec=",")

ibovespacor=ibovespa-selic
petro3cor=petro3-selic
#ibovespacor
#petro3cor
x=ibovespacor[[1]]

### MODELO DE REGRESSAO
fit=lm(petro3cor[[1]]~x)
summary(fit)

plot(x,petro3cor[[1]],xlab="Ibovespa",ylab="Petro3",ylim=c(min(petro3cor[[1]]),max(petro3cor[[1]])),xlim=c(min(x),max(x)))
par(new=T)
plot(x,fitted.values(fit),xlab="Ibovespa",ylab="Petro3",type='l',col="red",ylim=c(min(petro3cor[[1]]),max(petro3cor[[1]])),xlim=c(min(x),max(x)))



### MODELO DINAMICO

buildCapm<-function(u){
dlmModReg(x,dV=exp(u[1]),dW=exp(u[2:3]))
}
outMLE<-dlmMLE(petro3cor[[1]],parm=rep(0,3),buildCapm)
exp(outMLE$par)
outMLE$value

mod<-buildCapm(outMLE$par)
outS<-dlmSmooth(petro3cor,mod)
plot(dropFirst(outS$s))
#outS$s
par(mfrow=c(2,1))
ts.plot(outS$s[,1],xlab="t",ylab="alpha")
ts.plot(outS$s[,2],xlab="t",ylab="beta")
lines(c(200,1),c(1,1),col="red")

modf<-dlmModReg(x,dV=outMLE[1],m0=c(0,0.8),C0=diag(c(1e+07,1)))
outF<-dlmFilter(petro3cor,mod)
windows()
par(mfrow=c(2,1))
ts.plot((outF$m[10:119,1]),xlab="t",ylab="alpha")
lines(c(200,1),c(0,0),col="red")
lines(c(200,1),c(0.0001,0.0001),col="BLUE")
ts.plot((outF$m[10:119,2]),xlab="t",ylab="beta")
lines(c(200,1),c(1,1),col="red")
lines(c(200,1),c(1.025,1.025),col="BLUE")















