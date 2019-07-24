# Load in the mf_training data
load("mf_training.Rda")
head(mf_training)

# More normally we would apply a logistic regression
# this fits a logistic curve that can olny
# take values between 0 and 1 (i.e. can't result in
# impossible probabilities as the linear regression can)
# glm.fit=glm(gender~height,data=mf_training,family=binomial)

# Note to view discrimination can plot histograms 
hist(subset(mf_training,gender==1)$height, col='blue',
     xlab='height',main='Male/Females height distributions')
hist(subset(mf_training,gender==0)$height, col=rgb(1,0,0,0.5), add=T)

# 1) Now use logistic regression to predict the mean 
# value of gender regressed against height
# can you plot the resulting fit?
gender_height.glm=glm(gender~height,data=mf_training,family=binomial)

# to plot this is not so easy... quick google
# In general the command we need to use is:
#<model>=glm(<output>~<input_var>,data=mf_training,family=binomial)
#curve(predict(<model>,data.frame(<input_var>=x),type="resp"),add=TRUE)
# replace names in < > with your own variable names
plot(gender~height,data=mf_training)
curve(predict(gender_height.glm,data.frame(height=x),type="resp"),add=TRUE)
coef(gender_height.glm)
# 2) What is the formula for the fit? This predicts the probability
# that a subject drawn from the dataset and found to have a
# given height, is female
# for logistic fit formula is 1 / (1 + exp(-B0 - B1*X))
# here B0 = 21.07, B1 = -0.123
# so P female =   1/(1 + exp(-21.07 + 0.123*height) )
# note can also add curve using this
# curve(1/(1 + exp(-21.07 + 0.123*x)),col='green',add=TRUE)

# 3) What does the logistic regression model predict for
# P(female|height=150)
# P(female|height=180)
# Hint. we use the predict function like this to find 
# prediction for heights h1, h2 etc
# note need additional argument type="resp" for this fit method
predict(gender_height.glm, data.frame(height = c(150, 180)),type="resp")

# 4) At which height H is P(female|height=H) = 0.5
# hint. you can get fit parameters b0 and b1
# using code like:
# b0 = coef(model)[["(Intercept)"]]) 
# b1 = coef(model)[["height"]]
# and solve for y = 1  / (1  + exp(-b0 - b1*x))
# by inspection when exp(-b0 - b1*x) = 1
# y= 0.5
# so exp(-b0 + -b1*x) = 1
# means - b0 - b1*x = 0
# so crosses at x = -b0/b1 = 21.07 / 0.123 = 171.3 cm
# looks reasonable from the fitted curve

# 5) To make a prediction male or female at height x
# look at the prediction from the regression
# which calculates P(female|x)
#
# If we estimate P(female|x) > 0.5 we predict female (1)
# If we estimate P(female|x) < 0.5 we predict female (0)

# generate columns as we did with the previous value
mf_training$mod1_value <- predict(gender_height.glm,type='resp')
mf_training$mod1_prediction <- 0
mf_training$mod1_prediction <- ifelse(mf_training$mod1_value > 0.5, 1, 0 )

# Use predict to add a column to our data_frame that stores
# i) "response" containing model output value (probability female)
mf_training$response <- mf_training$mod1_value
# ii) "prediction" containing model prediction as a factor "male" or "female"
mf_training$prediction <- factor(mf_training$mod1_prediction, levels=c(0,1), labels=c("male","female"))
# iii) "actual" containing true gender as factor ""male" or "female"
mf_training$actual <- factor(mf_training$gender, levels=c(0,1), labels=c("male","female"))

# Once you have data in this format you can summarise the model as a frequency table using:
table(predicted=mf_training$prediction,actual=mf_training$actual)

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
mf_test$mod1_value <- predict(gender_height.glm,newdata=mf_test)
mf_test$mod1_prediction <- rep(0,nrow(mf_test))
mf_test[which(mf_test$mod1_value > 0.5),]$mod1_prediction <- 1
total <- nrow(mf_test)
misclassified <- length(which(mf_test$mod1_prediction != mf_test$gender))
rate <- misclassified/total
rate
# higher rate as expected when we switch from
# data we used to fit model to a test dataset!

# 8) Try using all the input variables in the logistic regression
# How does this improve the model performance
# (e.g. compare the misclassification rate on training and test data)
gender_full=glm(gender~height+eye+hair+glasses,data=mf_training,family=binomial)

# Use predict to add a column to our data_frame that stores
# i) model output value 
mf_training$mod1_value <- predict(gender_full,type='resp')
# ii) model prediction
mf_training$mod1_prediction <- rep(0,nrow(mf_training))
mf_training[which(mf_training$mod1_value > 0.5),]$mod1_prediction <- 1

# Look at the average misclassification rate for this model
# against the training data.
total <- nrow(mf_training)
misclassified <- length(which(mf_training$mod1_prediction != mf_training$gender))
rate <- misclassified/total
# misclassified training data
rate

# Look at the misclassification rate on the test data?
# found in the file mf_test.Rda
load("mf_test.Rda")
mf_test$mod1_value <- predict(gender_full,newdata=mf_test, type='resp')
mf_test$mod1_prediction <- rep(0,nrow(mf_test))
mf_test[which(mf_test$mod1_value > 0.5),]$mod1_prediction <- 1
total <- nrow(mf_test)
misclassified <- length(which(mf_test$mod1_prediction != mf_test$gender))
rate <- misclassified/total
# misclassified test data
rate

# using all the inputs we reduce the misclassification
# rate to 10.1% on the training data and 12.5% on test data

