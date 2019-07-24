# Suppose we want to train a computer to recognise gender.
# We can program the computer to measure height and hair length
# and whether they wear glasses.

# In a set of training data we have statistics for 400 men and 600 women

# Load and view the dataset
load("mf_training.Rda")
head(mf_training)

# 1. What is the probability that a randomly chosen person 
# from the dataset is female?

# P(female) = 600 / 1000 = 0.6

# Load dataset mf_training.Rda
# The gender column stores 1 for female and 0 for male
# Check that there are 400 men and 600 women in the dataset.

nfemales <- length(which(mf_training$gender==1))
nmales <- length(which(mf_training$gender==0))

# 2. What is the average of the gender column?
# How is this related to (i)

result <- mean(mf_training$gender)
# same as P(female)

# 3. In the data column glasses a value of 1 represents glasses,
#    and 0 represents no glasses
# 	 
#    Count the entries in the dataset to determine 
#    i) P(female|glasses)  
#        probability person from dataset if female if they wear glasses

n_f_glasses <- length(which(mf_training$gender==1 & mf_training$glasses==1))
n_m_glasses <- length(which(mf_training$gender==0 & mf_training$glasses==1))
prob_f_glasses  <- n_f_glasses / (n_f_glasses + n_m_glasses)
prob_f_glasses

#    ii) P(female|no-glasses) 
#        probability person from dataset if female if they do not wear glasses

n_f_noglasses <- length(which(mf_training$gender==1 & mf_training$glasses==0))
n_m_noglasses <- length(which(mf_training$gender==0 & mf_training$glasses==0))
prob_f_noglasses  <- n_f_noglasses / (n_f_noglasses + n_m_noglasses)
prob_f_noglasses

# 4. i) What is the mean value of gender for the subset of the dataset with glasses?
#    ii) What is the mean value of gender for the subset of the dataset without glasses?
#    iii) What can you notice when you compare these to your previous answers?

rows_glasses = which( mf_training$glasses==1)
result <- mean(mf_training[rows_glasses,]$gender)
result

result <- mean(mf_training[-rows_glasses,]$gender)
result

# conclusion is that probabilities are
# P(female) = 0.600
# P(female|glasses) = 0.580
# P(female|noglasses) = 0.603
# decreased chance of female if selection has glasses
# increased chance of female if selection does not have glasses

# The general conclusion you should have drawn is that the
# mean of the gender column under a certain selection
# is linked to the probability of being female

# 5. Check P(female|hair>10) by finding the mean
# of the gender column for the corresponding
# data subset. What do you find?
mean(mf_training[which(mf_training$hair>10),]$gender)
