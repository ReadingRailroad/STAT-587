# sat.r

print.regsub <- function(l, sort='BIC', best=NULL) {
  var <- apply(l$which, 1, function(x){
    paste(l$obj$xnames[x][-1],collapse=' ' )})
  nvar <- apply(l$which[,-1], 1, sum)

  aic <- l$bic - log(l$obj$nn)*nvar + 2*nvar

  temp <- data.frame(model=var, nvar=nvar, Rsq=l$rsq, AdjRsq=l$adjr2, 
    Cp=l$cp, AIC = aic, BIC=l$bic)
  o <- order(temp[,sort])
  if (!is.null(best)) {
    o <- o[1:best] 
    }
  temp[o,]
}

#install.packages('readxl')
library(readxl)
sat <- read_excel('sat.xlsx')

#install.packages('leaps')
library(leaps)
sat.sub <- regsubsets(sat ~ ltakers+income+years+public+expend+rank, 
   data=sat, method='exhaustive' , nbest=30)

sat.sub2 <- summary(sat.sub)

# the 5 best models using BIC (the default)
print.regsub(sat.sub2, best=5)

# or AIC, can sort by any column in the result
print.regsub(sat.sub2, sort='AIC', best=5)

sat.lm <- lm(sat ~ ltakers + years + rank + expend, data=sat)

# calculate the PRESS statistic
# out-of-sample residual - using a matrix algebra shortcut
pres <- resid(sat.lm)/sqrt(1-lm.influence(sat.lm)$hat)

# PRESS statistic
press <- sum(pres^2)
press

# rMSEP
sqrt(press/length(pres))
 

