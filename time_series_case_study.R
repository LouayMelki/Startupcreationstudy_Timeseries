rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(dygraphs)
library(mgcv)
##################################################################################################################
####################### prétraitement, mises-en forme des données
##################################################################################################################
data_0<-read.csv("valeurs_mensuelles.csv", dec = ".",sep = ";", header = F,skip = 3)
data_0
data=subset(data_0,select=-c(V3))
names(data)<-c("Date","Nombre_entreprise")
for(i in 1:253){data$Date[i]<-paste(data$Date[i],"28",sep="-")}
date = as.POSIXct(strptime(data$Date, "%Y-%m-%d"))
date<-rev(date) # ordonner les dates en ordre croissant en temps
Nombre_entreprise <- as.numeric(data$Nombre_entreprise)
Nombre_entreprise<-rev(Nombre_entreprise) # valeurs correspondantes
data <- data.frame(date,Nombre_entreprise)
summary(data)
data
##################################################################################################################
####################### analyse descriptive des données
##################################################################################################################
Nombre_entreprise.zoo <- zoo(Nombre_entreprise, order.by = date)
plot(Nombre_entreprise.zoo, xaxt = "n", xlab = "")
t <- time(Nombre_entreprise.zoo)
axis.POSIXct(1, at = seq(t[1], tail(t, 1), "years"), format = "%d/%m/%y",
             las = 2)
boxplot(Nombre_entreprise)
hist(Nombre_entreprise,breaks=25)
mois <- as.factor(format(date, "%m"))
monthlyNumber <- tapply(Nombre_entreprise, mois, mean)
plot(monthlyNumber, type = "b", pch = 20)
boxplot(Nombre_entreprise ~ mois, col = "lightblue", pch = 20, cex = 0.5)
acf(Nombre_entreprise,lag.max = 253)
pacf(Nombre_entreprise,lag.max = 253)
tmp<-xts(Nombre_entreprise,order.by = date)
##################################################################################################################
####################### étude de la tendance
##################################################################################################################

##regression lineaire
X<-xts(data$Nombre_entreprise,order.by = data$date)
X~t
reg<-lm(X~t)
summary(reg)
ychap.lm <- reg$fitted
plot(as.numeric(reg$residuals), type='l')
ychap.lm<-xts(as.numeric(reg$fitted),order.by=date)
# ychap.lm<-xts(reg$fitted,order.by=date)
plot(X,type='l')
lines(ychap.lm,col='red',lwd=2)




## Moyenne Mobile
nbe<-xts(data$Nombre_entreprise,order.by = data$date)
nbe
MA<-filter(nbe, filter=array(1/12,dim=12), method = c("convolution"),
           sides = 2, circular = FALSE)
MA<-xts(MA,order.by=date)
plot(nbe,type='l')
lines(MA,col='red',lwd=2)
## Differenciation
par(mfrow = c(1, 2))
nbe.ts<-ts(data$Nombre_entreprise,start=1,frequency = 12)
nbe.ts
acf(nbe.ts, na.action = na.omit)
diff.nbe.ts <- diff(nbe.ts, lag = 1, differences = 1)
acf(diff.nbe.ts, na.action = na.omit)
plot(nbe.ts, col = "blue")
plot(diff.nbe.ts, col = "orangered2")
## Estimation paramétrique
time <- c(1:nrow(data))
data$date <- time
reg <- lm(Nombre_entreprise ~ time + I(time^2) + I(time^3), data = data)
par(mfrow = c(1, 2))
plot(date, Nombre_entreprise, type = "l", xlab = "",
     ylab = "Nombre d'entreprise crées Paris (INSEE)",lwd=2)
lines(date, reg$fitted, col = "red", lwd = 2)
plot(date, data$Nombre_entreprise - reg$fitted, type = "l",
     xlab = "", ylab = "nb entreprise-date", col = "orangered2")

## Estimation non paramétrique
# Estimateur à noyaux gaussien
noyau <- ksmooth(data$date, Nombre_entreprise, kernel = c("normal"),
                 bandwidth = 10)
noyau
trend<-noyau$y
par(mfrow = c(1, 2))
plot(date, Nombre_entreprise, type = "l", xlab = "",
     ylab = "Nombre d'entreprise crées Paris (INSEE)",lwd=2)
lines(date, noyau$y, col = "red", lwd = 2)
plot(date,Nombre_entreprise - noyau$y, type = "l",
     xlab = "", ylab = "nb entreprise-date", col = "orangered2")

# Polunome locaux
lo <- loess(Nombre_entreprise ~ time, data = data, degree = 2, span = 0.7)
plot(date, Nombre_entreprise, type = "l", xlab = "",
     ylab = "Nombre d'entreprise crées Paris (INSEE)",lwd=2)
lines(date, lo$fitted, col = "red", lwd = 2)

## Estimation semi paramétrique
#regression sur base splines
library(mgcv)
g <- gam(Nombre_entreprise ~ s(time, k = 10), data = data)
gp<-g$fitted
plot(date, Nombre_entreprise, type = "l", xlab = "",
     ylab = "Nombre d'entreprise crées Paris (INSEE)",
     lwd = 2)
lines(date, g$fitted, col = "red", lwd = 2)
gp<-g$fitted



##################################################################################################################
####################### étude de la Saisonnalité
##################################################################################################################

## regression
n<-length(date)
t<-c(1:n)
X.detrend<-X-trend
plot(X.detrend)

acf(as.numeric(X.detrend), lag.max=100)
w=2*pi/12
fourier<-cbind(cos(w*t), sin(w*t))
K<-21
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
matplot(fourier[,1:10],type='l')
dim(fourier)


reg<-lm(X.detrend~fourier[,1:4]-1)
summary(reg)
ychap.lm.season<-xts(as.numeric(reg$fitted),order.by=date)
plot(X.detrend,type='l')
lines(ychap.lm.season,col='red', lwd=2)

#################moyenne mobile
K <- 12
mb.season<-filter(X.detrend, filter=array(1/K,dim=K), method = c("convolution"), 
                  sides = 2, circular = TRUE)
mb.season<-xts(mb.season,order.by=date)

plot(X.detrend,type='l')
lines(mb.season,col='red',lwd=2)

#################noyau Gaussien
h=12
x<-seq(1,max(t),length=n)
W<-matrix(unlist(lapply(x,function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
plot(W[,10])
ychap.kernel.season<-colSums(as.numeric(X.detrend)*W)
ychap.kernel.season<-xts(ychap.kernel.season,order.by=date)

plot(X.detrend,type='l')
lines(ychap.kernel.season,col='red',lwd=2)



#################polynomes locaux
lo<-loess(X.detrend~t, degree=2,span=0.083)
ychap.lo.season<-xts(lo$fitted,order.by=date)
plot(X.detrend,type='l')
lines(ychap.lo.season,col='red',lwd=2)


#################rÃ©gression sur bases de splines cycliques
#cycle<-c(rep(c(1:1),12),rep(c(2:2),12),rep(c(3:3),12),rep(c(4:4),12),rep(c(5:5),12),rep(c(6:6),12),rep(c(7:7),12),rep(c(8:8),12),rep(c(9:9),12),rep(c(10:10),12),rep(c(11:11),12),rep(c(12:12),12),rep(c(13:13),12),rep(c(14:14),12),rep(c(15:15),12),rep(c(16:16),12),rep(c(17:17),12),rep(c(18:18),12),rep(c(19:19),12),rep(c(20:20),12),rep(c(21:21),12),1)
cycle<-c(1:253)
length(cycle)
plot(cycle)

plot(cycle, X.detrend, pch=20)

g<-gam(X.detrend~s(cycle,k=126, bs='cc'))
summary(g)
ychap.gam.season<-xts(g$fitted,order.by=date)
plot(X.detrend,type='l')
lines(ychap.gam.season,col='red',lwd=2)

####### comaraison des méthodes
eps<-X.detrend-ychap.gam.season
plot(eps)
summary(eps)
plot(X-eps,type='l',ylim=range(X))
lines(X-eps,lwd=2)
lines(X,col='grey')
lines(ychap.lm+ychap.lm.season,col='purple')
lines(ychap.lm+ychap.kernel.season,col='red')
lines(ychap.lm+ychap.lo.season,col='blue')
lines(ychap.lm+ychap.gam.season,col='turquoise2')
lines(ychap.lm+mb.season,col='violetred1')

epschap <- X-(ychap.lm+ychap.lm.season)
epschap <- X-(ychap.lm+ychap.kernel.season)
plot(epschap, type='l')


acf(X-(ychap.lm+ychap.lm.season))
acf(X-(ychap.lm+ychap.kernel.season))
acf(X-(ychap.lm+ychap.lo.season))
acf(X-(ychap.lm+ychap.gam.season))
acf(X-(ychap.lm+mb.season),na.action = na.omit)
################################################################
########################## bruit
################################################################

eps
nrow(eps)
for(i in 1:253){if(abs(eps[i])<500){eps[i]<-0}}
plot(eps)

##################################################################################################################
####################### Lissage exp
##################################################################################################################

####Simple
n<-253
X=Nombre_entreprise
expSmooth=function(x,alpha)
{
  xsmooth=x
  for(i in c(2:length(x)))
  {
    xsmooth[i]<-(1-alpha)*xsmooth[i-1]+alpha*x[i]
  }
  return(xsmooth)
}

alpha <-seq(0.05,0.95,length=100)
forecast<-lapply(alpha,expSmooth,x=X)
str(forecast)

erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(X,n-1)-head(x,n-1))^2)}))


plot(alpha,erreur,type='l')
X.smooth<-expSmooth(X,alpha[which.min(erreur)])
plot(X,type='l')
lines(X.smooth,col='red',lwd=2)

########################################################
####Double
########################################################
DoubleExpSmooth=function(x,alpha)
{
  xsmooth=x
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-xsmooth[i-1]+(1-(1-alpha)^2)*(x[i]-xsmooth[i-1])
    b[i]<-b[i-1]+alpha^2*(x[i]-xsmooth[i-1])
    xsmooth[i]<-l[i]+b[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res)
}


alpha<-seq(0.05,0.95,length=100)
forecast<-lapply(alpha,DoubleExpSmooth,x=X)
erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(X,n-1)-head(x$smooth,n-1))^2)}))
plot(alpha,erreur,type='l')

X.smooth<-DoubleExpSmooth(X,alpha[which.min(erreur)])
plot(X,type='l')
lines(X.smooth$smooth,col='red',lwd=2)

plot(X.smooth$l,type='l',ylim=range(X.smooth$l,X.smooth$b),col='blue')
lines(X.smooth$b,col='red')


predict.expSmooth<-function(Xsmooth,inst,horizon,smooth.type)
{
  
  if(smooth.type=="simple")
  {
    n<-length(Xsmooth)
    prev<-c(Xsmooth[1:inst],rep(Xsmooth[inst],horizon))
  }
  
  if(smooth.type=="double")
  {
    n<-length(Xsmooth$smooth)
    prev<-c(Xsmooth$smooth[1:inst],Xsmooth$l[inst]+Xsmooth$b[inst]*c(1:horizon))
  }
  if(smooth.type=="hw")
  {
    prev<-c(Xsmooth$smooth[1:inst],as.numeric(hw(nbe.ts,seasonal="additive",h=horizon)$mean))
  }
  return(prev)
}



alpha=0.15
X.d.exp.mooHW<-DoubleExpSmooth(X,alpha)
prevHW<-predict.expSmooth(X.d.exp.mooHW,
                          inst=253,horizon=24, smooth.type="double")
plot(X,pch=20,ylim=range(X,prevHW),xlim=range(0,277),type='l')
lines(prevHW,col='red',lwd=2)
abline(v=253,lty='dashed')


####Seosonal double HW

SeasonalDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  l<-array(x[2],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  s<-array(x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-alpha*(x[i]-s[max(i-T,1)])+(1-alpha)*(l[i-1]+b[i-1])
    b[i]<-beta*(l[i]-l[i-1])+(1-beta)*b[i-1]
    s[i]<-delta*(x[i]-l[i])+(1-delta)*s[max(i-T,1)]
    xsmooth[i]<-l[i]+b[i]+s[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  res$s<-s
  return(res)
}




alpha <- 0.15
beta <- 0.2
delta <- 0.2
T <- 12
X.seas.exp.mooHW <- SeasonalDoubleExpSmooth(X,alpha,beta,delta,T)
prevHW<-predict.expSmooth(X.seas.exp.mooHW,
                          inst=253,horizon=24, smooth.type="hw")
plot(X,pch=20,ylim=range(X,prevHW),xlim=range(0,277),type='l')
lines(prevHW,col='red',lwd=2)
abline(v=253,lty='dashed')


par(mfrow=c(1,1))
plot(X, type='l')
lines(X.seas.exp.mooHW$smooth, col='red',lwd=2)

names(X.seas.exp.mooHW)

par(mfrow=c(3,1))
plot(X.seas.exp.mooHW$b, type='l')
plot(X.seas.exp.mooHW$l, type='l')
plot(X.seas.exp.mooHW$s, type='l')

