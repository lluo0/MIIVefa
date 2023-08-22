
# test if the recovered model from the classic Holzinger data


library(MIIVefa)
library(lavaan)
holzingerfit <- miivefa(lavaan::HolzingerSwineford1939[,7:15],
                        sigLevel = .05,
                        scalingCrit = 'sargan+factorloading_R2',
                        correlatedErrors = NULL)

#if it recovers a 3 factor model
expect_equal(
  length(strsplit(holzingerfit$model, '\n')[[1]]), 3
)

#if it recovers as pre-tested
expect_equal(
  holzingerfit$model, "f1=~x6+x4+x5+x3\nf2=~x1+x2+x3+x9\nf3=~x8+x9+x7"
)
