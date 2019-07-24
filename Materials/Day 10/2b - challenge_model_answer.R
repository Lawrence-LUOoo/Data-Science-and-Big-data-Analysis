N <- length(cv.carseats$size)-1
# final size is 1 node which we wont use
mc <- NULL
size_vals <- unlist(cv.carseats$size[1:N])
mc <- rep(0,N)

for (i in 1:N){
  prune.carseats <- prune.misclass(tree.carseats, best = size_vals[i])
  tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
  mc[i] <- sum(tree.pred != Carseats.test$High)
}
plot(cv.carseats$size, cv.carseats$dev, type = "b", ylim= c(40,100))
points(size_vals,mc, col='red')

