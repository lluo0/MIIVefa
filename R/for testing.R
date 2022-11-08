rm(list = ls())
library(lavaan)
library(MIIVsem)

testdata <- sim1[[1]]
#colnames(testdata) <- NULL
head(testdata)
data = testdata
sigLevel = .05
scalingCrit = "sargan+factorloading_R2"
correlatedErrors = NULL
select_scalingind(data = testdata,
                  )
r2_order(testdata)


r2out <- r2_order(testdata)
data = testdata

miive('f1=~x1+x3+x4+x2+x5+x6+x8\nf2=~x7+x2+x5+x6+x8', data,var.cov = T)

data = sim6[[1]]
select_scalingind(data)
stepPrev <- step1_EFAmiiv(data)
select_scalingind_stepN(data,
                        #scalingCrit = "order",
                        stepPrev = stepPrev)


#sm17: 3 factor with crossloading, x7 and x4 crossload
sm17 <- 'f1 =~ 1*x1 + .8*x2 + .7*x3 + .7*x4 +.5*x7
f2 =~ 1*x5 + .8*x6+ .7*x7 + .7*x8
f3 =~ 1*x9 + .8*x10 + .7*x11 + .x7*x12+.6*x4
f1 ~~ .4*f2
f1 ~~ .4*f3
f2 ~~ .4*f3'
sim17<- list()
for (p in 1:30){
  set.seed(123.4+p)
  sim17[[p]] <- simulateData(sm17, sample.nobs = 1000)
}
data <- sim17[[1]]
select_scalingind(data)
step1 <- step1_EFAmiiv(data)
select_scalingind_stepN(data,
                        #scalingCrit = "order",
                        stepPrev = step1)

step2 <- stepN_EFAmiiv(data, sigLevel = .05, scalingCrit = 'sargan+factorloading_R2' ,stepPrev = step1)
step2half <- crossloadcheck(data,  sigLevel = .05, scalingCrit = 'sargan+factorloading_R2' ,stepPrev = step2)
step3 <- stepN_EFAmiiv(data, sigLevel = .05, scalingCrit = 'sargan+factorloading_R2' ,stepPrev = step2half)
step3half <- crossloadcheck(data,  sigLevel = .05, scalingCrit = 'sargan+factorloading_R2' ,stepPrev = step3)
stepN_EFAmiiv(data, sigLevel = .05, scalingCrit = 'sargan+factorloading_R2' ,stepPrev = step2)

EFAmiiv(data=sim17[[2]], sigLevel = .05, scalingCrit = 'sargan+factorloading_R2')

EFAmiiv(data=sim18[[2]], sigLevel = .05, scalingCrit = 'sargan+factorloading_R2')

EFAmiiv(data=sim15[[1]], sigLevel = .05, scalingCrit = 'sargan+factorloading_R2')

miive(model = 'f1=~x1+x2+x3+x4+x7
      f2=~x5+x6+x7+x8
      f3=~x9+x10+x11+x12+x4
      f4=~x13+x14+x15+x16+x10', sim18[[2]], var.cov = T)

miive(model = 'f1=~x13+x14+x15+x16+x10
      \nf2=~x12+x9+x11+x10+x4
      \nf3=~x6+x5+x8+x7
      \nf4=~x3+x1+x2+x7+x4'
      , sim18[[2]], var.cov = T)

temp


empirical1 <- lavaan::PoliticalDemocracy[,1:8]

EFAmiiv(empirical1, .05,'sargan+factorloading_R2',
          correlatedErrors = 'y1 ~~y5
          y2~~y6
          y3~~y7
          y4~~y8') #old algorithm picked x4 as the scaling indicator!
##because the scaling indicator selecting step did NOT take into account of correlated errors
step1_empirical1_old <- step1_E5(empirical1, .05, 'sargan+factorloading_R2')
stepPrev <- step1_empirical1_old
data <- empirical1
select_scalingind_stepN(empirical1, .05, 'sargan+factorloading_R2', step1_empirical1_old)

EFAmiiv(empirical1, .05,'sargan+factorloading_R2')
data <- empirical1

accdata <- read.table('/Users/lanluo/Downloads/access_raw.txt', header=F,sep=",")
head(accdata)
colnames(accdata) <- c(sapply(c(1:6), function(x) paste0('access', x)), sapply(c(1:6), function(x) paste0('easy', x)))

EFAmiiv(accdata, .05, 'sargan+factorloading_R2',
          correlatedErrors = 'access1~~easy1
      access2~~easy2
      access3~~easy3
      access4~~easy4
      access5~~easy5
      access6~~easy6')

data <- accdata
sigLevel <- .05
scalingCrit <- 'sargan+factorloading_R2'

EFAmiiv(accdata, .05, 'sargan+factorloading_R2')
###old algorithm different scaling indicator for the 2nd factor
step1_old <- step1_E5(accdata, .05, 'sargan+factorloading_R2',
                      correlatedErrors = 'access1~~easy1
      access2~~easy2
      access3~~easy3
      access4~~easy4
      access5~~easy5
      access6~~easy6')
stepPrev<- step1_old
select_scalingind_stepN(accdata, .05, 'sargan+factorloading_R2', step1_old) #access1
##
data <- sim10[[1]]
EFAmiiv(sim10[[1]])
cov(sim10[[1]])
