# load("D:/Desktop/Research/Paper/Data-driven analysis on the simulations of the spread of COVID-19 under different interventions of China/Result/Result_sz.RData")
# dat <- train_result$dat

## Data import
dat <- rowSums(dat[,3:4])
N <- train_result$N
region <- "Shenzhen"
 
## Initial 
dat <- c(rep(0, 7), dat)
dat <- incre_tr(dat)
init <- c(N - dat[7], dat[7], 0, 0, 0)
time_length <- length(dat)
