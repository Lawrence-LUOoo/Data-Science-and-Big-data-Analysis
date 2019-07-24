# Task 1

# Open file 'mf_training.Rda'
load("mf_training.Rda")

# Examine the data, and make exploratory plots to test variables
# that might be good predictors for height
# Comment on your findings.
par(mfrow=c(2,2))

# the following command creates a 2x2 plot but
# adjusts margins (plot spacing) to be smaller
par(mfcol=c(2,2), mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))

# with command lets you run a
# command using a given data set
# similar to using the data=... argument
with(mf_training,  scatter.smooth(height ~ hair, 
                                  pch=20, 
                                  col = ifelse(gender==1,'red','blue') 
                                  )
     )
plot(height ~ eye, data=mf_training)
plot(height ~ as.factor(gender), data=mf_training)
plot(height ~ as.factor(glasses), data=mf_training)

# gender seems most strongly related

# Perform a linear regression to predict
# height from the other variables

model = lm(height ~ hair + gender + glasses + eye, data=mf_training)
summary(model)
mse<-mean(residuals(model)^2)
rmse<-sqrt(mse)
# mse 43.8 cm^2
# rmse 6.62 cm

library(leaps)
regsubsets.out <-
  regsubsets(height ~ ., # full model
             data = mf_training,	# dataset
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL, # edit if you want to force variables in / out
             method = "exhaustive") # tries every combination

regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

## Adjusted R2 vs number of input variables
par(mfrow=c(1,2))
plot(regsubsets.out, scale="adjr2")
adj_rsquare_values <- summary(regsubsets.out)$adjr2
n_predictor_vals <- 1:5
plot(adj_rsquare_values~n_predictor_vals)
