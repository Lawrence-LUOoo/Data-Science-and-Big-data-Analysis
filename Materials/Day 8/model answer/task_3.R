# Load in the mf_training data
load("mf_training.Rda")
head(mf_training)

# 1) Perform a linear regression fit to predict the mean 
# value of gender regressed against hair length
# and plot the result

gender_height.lm = lm(gender ~ height,mf_training)
plot(mf_training$gender ~ mf_training$height)
abline(gender_height.lm)

# 2) What is the formula for the fit? This predicts the probability
# that a subject drawn from the dataset and found to have a
# given height, is female

summary(gender_height.lm)
# P(female|height=x) = 4.897 - 0.0257x 

# 3) What does the model predict for
# P(female|height=150)
# P(female|height=180)
# Hint. we use the predict function like this to find 
# prediction for heights h1, h2 etc
# predict(gender_height.lm, data.frame(height = c(h1, h2 ... )))
predict(gender_height.lm, data.frame(height = c(150, 180)))
# note probability for female when height is 150 is greater than 1!
# note probability for female when height is 180 is 0.27
# i.e. only a 27% chance that person from 
# group with height 180cm is female

# 4) At which height H is P(female|height=H) = 0.5
# hint. you can get fit parameters b0 and b1
# using code like:
# b0 = coef(model)[["(Intercept)"]]) 
# b1 = coef(model)[["height"]]
# and solve for y = b0 + b1*x 
b0 = coef(gender_height.lm)[["(Intercept)"]]
b1 = coef(gender_height.lm)[["height"]]
result <- (0.5 - b0)/b1
result

# 5) To make a prediction male or female at height x
# look at the prediction from the regression
# which calculates P(female|x)
#
# If we estimate P(female|x) > 0.5 we predict female (1)
# If we estimate P(female|x) > 0.5 we predict female (0)

# Use predict to add a column to our data_frame that stores
# i) model output value 
mf_training$mod1_value <- predict(gender_height.lm)
# ii) model prediction
mf_training$mod1_prediction <- rep(0,nrow(mf_training))
mf_training[which(mf_training$mod1_value > 0.5),]$mod1_prediction <- 1

# alternative way to make predictions
mf_training$mod1_prediction <- ifelse(mf_training$mod1_value > 0.5,1,0)


# 6) We test the quality of our model using the misclassification rate
# What is the average misclassification rate for this model
# against the training data?
total <- nrow(mf_training)
misclassified <- length(which(mf_training$mod1_prediction != mf_training$gender))
rate <- misclassified/total
rate

# 7) What is the misclassification rate on the test data?
# found in the file mf_test.Rda
load("mf_test.Rda")
mf_test$mod1_value <- predict(gender_height.lm,newdata=mf_test)
mf_test$mod1_prediction <- rep(0,nrow(mf_test))
mf_test[which(mf_test$mod1_value > 0.5),]$mod1_prediction <- 1
total <- nrow(mf_test)
misclassified <- length(which(mf_test$mod1_prediction != mf_test$gender))
rate <- misclassified/total
rate
# higher rate as expected when we switch from
# data we used to fit model to a test dataset!
