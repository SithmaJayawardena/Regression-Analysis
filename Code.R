library(alr4)
library(caret)
library(MASS)

data = read.table("machine.data", sep=",")
colnames(data) = c('vendor name', 'Model Name', 'MYCT', 'MMIN', 'MMAX', 'CACH', 'CHMIN', 'CHMAX', 'PRP', 'ERP')

#Encoding categorical data
data$`vendor name` = factor(data$`vendor name`, levels = c('adviser','amdahl','apollo','basf','bti','burroughs','c.r.d','cdc','cambex','dec',
                                          'dg','formation','four-phase','gould','hp','harris','honeywell','ibm','ipl','magnuson'
                                          ,'nas','ncr','nixdorf','perkin-elmer','prime','siemens','sperry','sratus','wang'),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))

#scatterplot matrix
plot(~MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX+ERP,
     data=data,col = data$`vendor name`, pch = 16, main='Scatterplot matrix factored on vendor name')

#null model
Model1 = lm(ERP~MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=data)
summary(Model1)
plot(Model1)

#BoxCox transformation
boxCox(Model1)
data$ERP1 = data$ERP^0.5
Model2 = lm(ERP1~MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX,data=data)
summary(Model2)
plot(Model2)


#square root transformation
for (col in c('MYCT','MMIN','MMAX','CACH','CHMIN','CHMAX')) {
  data = cbind(data, data[,col]^0.5)
}
colnames(data) = c('vendor name', 'Model Name', 'MYCT', 'MMIN', 'MMAX', 'CACH', 'CHMIN', 'CHMAX', 'PRP', 'ERP','ERP1',
                   'MYCT1','MMIN1','MMAX1','CACH1','CHMIN1','CHMAX1')

Model3 = lm(ERP1~MYCT1+MMIN1+MMAX1+CACH1+CHMIN1+CHMAX1,data = data)
summary(Model3)
plot(Model3)

Model4 = lm(ERP1~MYCT1+MMIN+MMAX+CACH+CHMIN+CHMAX,data=data)
summary(Model4)
plot(Model4)

Model5 = lm(ERP1~MYCT+MMIN1+MMAX+CACH+CHMIN+CHMAX,data=data)
summary(Model5)
plot(Model5)

Model6 = lm(ERP1~MYCT+MMIN+MMAX1+CACH+CHMIN+CHMAX,data=data)
summary(Model6)
plot(Model6)

Model7 = lm(ERP1~MYCT+MMIN+MMAX+CACH1+CHMIN+CHMAX,data=data)
summary(Model7)
plot(Model7)

Model8 = lm(ERP1~MYCT+MMIN+MMAX+CACH+CHMIN1+CHMAX,data=data)
summary(Model8)
plot(Model8)

Model9 = lm(ERP1~MYCT+MMIN+MMAX+CACH+CHMIN+CHMAX1,data=data)
summary(Model9)
plot(Model9)

#first WLS
res = Model7$residuals
z = log(res^2)
z.lo = loess(z~data$ERP1,degree = 2,span=.75)
loz = predict(z.lo)
sig2hat = exp(loz)
sighat = sqrt(sig2hat)
Model10 = lm(ERP1~MYCT+MMIN+MMAX+CACH1+CHMIN+CHMAX,data=data,weights = 1/sighat)
summary(Model10)
plot(Model10)
plot(Model10$fitted.values,weighted.residuals(Model10))
lines(lowess(Model10$fitted.values,weighted.residuals(Model10)),col=2)
qqnorm(weighted.residuals(Model10))
qqline(weighted.residuals(Model10))

#second WLS
res2 = Model10$residuals
z2 = log(res2^2)
z.lo2 = loess(z2~data$ERP1,degree = 2,span=.75)
loz2 = predict(z.lo2)
sig2hat2 = exp(loz2)
sighat2 = sqrt(sig2hat2)
Model11 = lm(ERP1~MYCT+MMIN+MMAX+CACH1+CHMIN+CHMAX,data=data,weights = 1/sighat2)
summary(Model11)
plot(Model11)
plot(Model11$fitted.values,weighted.residuals(Model11))
lines(lowess(Model11$fitted.values,weighted.residuals(Model11)),col=2)
qqnorm(weighted.residuals(Model11))
qqline(weighted.residuals(Model11))



high = Model7
low = lm(ERP1 ~ 1, data = data,weights = 1/sighat)
stepAIC(low,direction="forward",scope=list(upper=high,lower=low))
stepAIC(high,direction="backward",scope=list(upper=high,lower=low))
#cross validation

train1.control = trainControl(method="cv",number=5)
FinalModel = train(ERP1~MYCT+MMIN+MMAX+CACH1+CHMIN+CHMAX,weights = 1/sighat, data = data, method = "lm",
                    trControl = train1.control)
FinalModel$results







