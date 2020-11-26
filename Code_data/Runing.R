setwd("D:/Documents/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/Code_data")
source("Epidemic_modeling.R")

## MCMC sampling
# Initial step
init_gene <- function(it){
  para <- c(1, 5, 5)
  alp <- c(runif(1, 0, 14), runif(1, 0, 14), runif(1), runif(1, 0, 2))
  dpa <- runif(2, 1, 20)
  I_init <- 1
  return(list(para, alp, dpa, I_init, -Inf))
}

l <- function(para_init, N, time_length, dat, init){
  return(-likelihood(init, c(1, 2, 5.1), 
                     para_init[1:4], N, time_length, dat, para_init[5]))
}

optim_l <- function(it){
  para_init <- init_gene(it)
  for(h in 1:50){
    para <- gibbs(para_init, init, N, time_length, dat)
  }
  # op <- optim(c(para[[2]], para[[3]]), l, N = N, time_length = time_length, dat = time_length, init = init, 
  #       method = "L-BFGS-B", lower = rep(10^(-10), 5), upper = c(Inf, Inf, 1, Inf, Inf))
  # para <- op$par
  return(para)
}

K <- 40
sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfSource("Data_import.R")
sfExport("init_gene", "l", "optim_l") 

Result <- sfLapply(1:K, optim_l)
sfStop() 

# Burn-in 
a <- sapply(1:K, function(i){Result[[i]][[5]]})
a <- sample(1:K, replace = T, prob = exp(a - max(a)))
para_t <- lapply(a, function(i){Result[[i]]})

MCMC <- function(para_t, it){
  para_init <- para_t[[it]]
  for(h in 1:50){
    para <- gibbs(para_init, init, N, time_length, dat)
    cat(c(h, para[[2]], para[[4]]), "\n")
  }
  return(para)
}

sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfSource("Data_import.R")
sfExport("init_gene", "MCMC") 

for(Cpu in 1:100){
  sfExport("para_t")  
  Result <- sfLapply(1:K, MCMC, para_t = para_t)
  a <- sapply(1:K, function(i){Result[[i]][[5]]})
  a <- sample(1:K, replace = T, prob = exp(a - max(a)))
  para_t <- lapply(a, function(i){Result[[i]]})
  cat(c(max(sapply(1:K, function(i){Result[[i]][[5]]}))), "\n")
}

sfStop() 

## Posterior sampling
MCMC <- function(para_t, it, G){
  para <- list()
  para[[1]] <- para_t[[it]]
  G <- 4000 / G 
  for(h in 2:G){
    para[[h]] <- gibbs(para[[h-1]], init, N, time_length, dat)
  }
  return(para)
}

sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfSource("Data_import.R")
sfExport("init_gene", "MCMC") 
Result <- sfLapply(1:K, MCMC, para_t = para_t, G = K)
sfStop()

a <- 4000 / K
para <- lapply(1:(length(Result) * a), function(i){
  Result[[(ceiling(i / a))]][[(i - a * (ceiling(i / a) - 1))]]
})

save(para, file =  paste0("para", region, ".rda"), version = 2)

## Predict function
pred <- function(k, time_length, para, init, N){
  comp_num <- Dynamic(time_length, para[[k]][[1]], para[[k]][[2]], init, N, para[[k]][[4]])
  colnames(comp_num) <- c("S", "I", "H", "R")
  return(comp_num)
}

Result <- lapply(1:length(para), pred, para = para, time_length = time_length, init = init, N = N)

pred_array <- array(0, c(length(para), time_length, 4))
for(i in 1:length(para)){
  pred_array[i,,] <- Result[[i]] 
}
apply(pred_array, c(2,3), mean)[,3]
apply(pred_array, c(2,3), mean)[,4]

## Time-varying Reproduction number
Rt <- sapply(1:length(para), function(k){
  rt <- f_alp(1:time_length, para[[k]][[2]])
  return(rt * para[[k]][[1]][2])
})
Rt_mean <- rowMeans(Rt)
