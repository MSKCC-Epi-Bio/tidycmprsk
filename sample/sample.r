set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace=TRUE)
cov <- matrix(runif(600),nrow=200)
dimnames(cov)[[2]] <- c('x1','x2','x3')
print(z <- cmprsk::crr(ftime,fstatus,cov))
summary(z)
z.p <- predict(z,rbind(c(.1,.5,.8),c(.1,.5,.2)))
plot(z.p,lty=1,color=2:3)
crr(ftime,fstatus,cov,failcode=2)
# quadratic in time for first cov
crr(ftime,fstatus,cov,cbind(cov[,1],cov[,1]),function(Uft) cbind(Uft,Uft^2))
#additional examples in test.R



dat <- data.frame(ftime=ftime,fstatus=fstatus,cov)

library(hardhat)
# library(timereg)
library(tidycmprsk)
library(broom)

processed_1 <- mold(ftime+fstatus~x1+x2+x3,dat)
crr.formula(ftime+fstatus~x1+x2+x3,dat)
crr.fit<-crr(ftime+fstatus~x1+x2+x3,dat, failcode=1)
crr(ftime+fstatus~x1+x2+x3,dat,failcode=1)
model.frame(crr.fit)
# model_frame(crr.fit)
tidy(crr.fit)
glance(crr.fit)
pred <- predict(crr.fit)
augment(crr.fit)
