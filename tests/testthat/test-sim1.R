# test if miivefa recovers simple 2 factor model
library(MIIVefa)
##simulate
library(mnormt)
seed <- 1234 #for replication purpose
#generate latent factor values
eta <- rmnorm(n=500,
              mean = c(0,0),
              varcov = matrix(c(1,.5,
                                .5,1), nrow = 2))
#generate residuals
seed <- 1234
e <- rmnorm(n=500,
            varcov =  diag(.25, nrow = 8))
#factor loading matrix
lambda <- matrix(c(1,0,
                   .8,0,
                   .7,0,
                   .6,0,
                   0,1,
                   0,.8,
                   0,.7,
                   .4,.6), nrow = 8, byrow = T)
#obtain observed variable values
sim1data <- eta %*% t(lambda) + e
#create column names
colnames(sim1data) <- paste0("x", 1:ncol(sim1data))
#make it a data frame
sim1data <- as.data.frame(sim1data)


#run miivefa
sim1fit <- miivefa(sim1data,
                        sigLevel = .01,
                        scalingCrit = 'sargan+factorloading_R2',
                        correlatedErrors = NULL)
#if it recovers a 2 factor model
expect_equal(
  length(strsplit(sim1fit$model, '\n')[[1]]), 2
)

# #if it recovers as true DGM
# expect_in(
#   lapply(lapply(strsplit(sim1fit$model, '\n')[[1]],
#                 function(x) strsplit(x, '=~')[[1]][[2]]),
#          function(x) sort(strsplit(x, "\\+")[[1]])),
#   list(c("x1","x2","x3","x4","x8"),
#             c("x5","x6","x7","x8"))
# )

