setwd("~/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/Code")
source("Epidemic_modeling.R")

## Specify region
# region_mark <- 1
source("Data_import.R")

## MCMC sampling
# Initial step
init_gene <- function(it, dat, Policy, region){
  para <- c(1, 5.1, 5)
  if(region == "Wuhan"){
    para <- c(1, 16.1, 5)
  }
  alp <- c(Policy[1], runif(1, 0, 14), Policy[2], runif(1, 0, 2))
  dpa <- runif(2, 1, 20)
  I_init <- dat[[1]][1]
  return(list(para, alp, dpa, I_init, -Inf, 5.1))
}

Initial_sel <- function(it){
  para_init <- init_gene(it, dat, Policy, region)
  for(h in 1:50){
    para <- gibbs(para_init, init, N, time_length, dat, region)
  }
  return(para)
}

K <- 40
sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfExport("init_gene", "Initial_sel", "region_mark") 
sfSource("Data_import.R")

Result <- sfLapply(1:K, Initial_sel)
sfStop() 

# Burn-in 
a <- sapply(1:K, function(i){Result[[i]][[5]]})
a <- sample(1:K, replace = T, prob = exp(a - max(a)))
para_t <- lapply(a, function(i){Result[[i]]})

MCMC <- function(para_t, it){
  para_init <- para_t[[it]]
  for(h in 1:50){
    para <- gibbs(para_init, init, N, time_length, dat, region)
    cat(c(h, para[[2]], para[[4]]), "\n")
  }
  return(para)
}

sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfExport("init_gene", "MCMC", "region_mark") 
sfSource("Data_import.R")

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
    para[[h]] <- gibbs(para[[h-1]], init, N, time_length, dat, region)
  }
  return(para)
}

sfInit(parallel = TRUE, cpus = K) 
sfSource("Epidemic_modeling.R")
sfExport("init_gene", "MCMC", "region_mark")
sfSource("Data_import.R")
Result <- sfLapply(1:K, MCMC, para_t = para_t, G = K)
sfStop()

a <- 4000 / K
para <- lapply(1:(length(Result) * a), function(i){
  Result[[(ceiling(i / a))]][[(i - a * (ceiling(i / a) - 1))]]
})

save(para, file =  paste0("../Result/Para_", region, ".rda"), version = 2)
