add_colour <- function(col,
                        n = 100,
                        mean_X1 = 10,
                        mean_X2 = 10,
                        sd_X1 = 0.5,
                        sd_X2 = 0.5,
                        covar = 0){
  E_cor <- rnorm(n,0, 1 )
  dir <- 1
  if (covar<0) {dir = -1}
  f = sqrt(abs(covar))
  X1 <- mean_X1 + E_cor*f + rnorm(n, 0, sd_X1 )*(1-f)
  X2 <- mean_X2 + E_cor*dir*f + rnorm(n, 0, sd_X2 )*(1-f)
  Y <- col
  result <- data.frame(X1,X2,Y,stringsAsFactors = F)
  return(result)
}

library(class)
# creates a data frame and draws different classes onto it

data.cols <- data.frame(NULL)

rows <- add_colour('blue',mean_X1=5,mean_X2=8)
data.cols<-rbind(data.cols,rows)

rows <- add_colour('red',mean_X1=5.8,mean_X2=8.5)
data.cols<-rbind(data.cols,rows)

plot(X2~X1,col=Y,data=data.cols)

set.seed(1)

library(dplyr)
# let's us select columns easily
# e.g. select(data.cols,X1, X2)
# see prediction for training points

train.X = data.cols[,c(1,2)]
train.Y = data.cols[,3]

# let's test our predictions on the test data
test.X = train.X


knn.pred=knn(train.X,test.X,train.Y,k=10)
table(predicted=knn.pred,actual=train.Y)


# we can run the predictions on a grid of points to
# see the areas predicted for each colour

# make grid
x1_seq = seq(3,8,0.05)
x2_seq = seq(5,10,0.05)
gridpoints <- NULL
for (i in x1_seq){
  for (j in x2_seq){
    gridpoints<-rbind(gridpoints,c(i,j))
  }
}
gridpoints=data.frame(gridpoints)
colnames(gridpoints) <- c("X1", "X2")

# predict using training data
knn_grid.pred=knn(train.X,gridpoints,train.Y,k=100)

# convert to characters so colours can be used for plotting
gridpoints$Y<-as.character(knn_grid.pred)

# plot original data
plot(X2~X1,col=Y,pch=20, data=data.cols)

# plot gridpoints
points(X2~X1,pch='.',col=Y, data=gridpoints)

