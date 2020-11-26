load("D:/Desktop/Research/Paper/Data-driven analysis on the simulations of the spread of COVID-19 under different interventions of China/Result/Result_sz.RData")
dat <- train_result$dat

## Data import
dat <- cbind(rep(0, ncol(dat)), rep(0, ncol(dat)),
             rowSums(dat[,3:5]), rowSums(dat[,4:5]))
N <- train_result$N
region <- "Shenzhen"
 
## Initial 
time_length <- nrow(dat)
init <- c(N, 0, dat[1,3], dat[1,4])
dat <- incre_tr(dat)


