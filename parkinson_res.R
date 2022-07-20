library(tidyverse)
library(qgcomp)

dat<-read_csv("zahid_fin.csv")

#assigning mixture
Xnm<-names(dat)[3:40] #mixture

#model fit - no boot strap
qgfit<- qgcomp.noboot(park_abv_50y~.,
                      expnms=Xnm,
                      dat=dat[,c('park_abv_50y',Xnm)],
                      family=poisson(),
                      bayes = T, q=4)

#plotting quantile g-computation
summary(qgfit)
# beta =3.40; 95% CI: 2.96-3.84

#plot weights
plot(qgfit)


#model fit - with boot strap
qgfit.boot<- qgcomp.boot(park_abv_50y~.,
                      expnms=Xnm,
                      dat=dat[,c('park_abv_50y',Xnm)],
                      family=poisson(), q=4, B=500, seed=7122022)
#mixture coefficient
summary(qgfit.boot)
#individual pollutant coefficient
summary(qgfit.boot$fit)$coefficients
#quantile trend plot
plot(qgfit.boot)
#numeric summary of coefficients
pointwisebound.boot(qgfit.boot, pointwiseref = 3)

save(qgfit.boot, file="zahid_park.rda")

