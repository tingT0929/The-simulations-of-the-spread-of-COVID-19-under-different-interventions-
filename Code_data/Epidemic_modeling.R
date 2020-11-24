
# --------- Packages -------------
library(deSolve)
library(extraDistr)
library(EnvStats)
library(truncdist)
library(tidyr)
library(ggplot2)
library(snowfall)

# --------- Function -------------
f_alp <- function(k, alp){
  alp[4] * (alp[3] / (1 + exp(2 * log(99) / alp[2] * (k - alp[1] - alp[2] / 2))) + 1 - alp[3])
}  

incre_tr <- function(dat){
  dat[-1] - dat[-length(dat)]
}

eqn <- function(time, init, para, N){
  
  rE <- para[2] * init[2]
  
  dS <- - para[1] * init[1] * init[3]  / N
  dE <- - dS  - rE
  dC <-  init[3] / para[3]
  dH <-  init[3] / para[4]
  dI <- rE - dC - dH

  return(list(c(dS, dE, dI, dC, dH)))
}

Dynamic <- function(time_length, para, alp, init, N){
  comp_num <- matrix(init, nrow = 1)
  for(i in 2:time_length){
    para[1] <- f_alp(i-1, alp)
    comp_num <- rbind(comp_num,
                      as.numeric(ode(y = comp_num[i-1,], 
                                     times = (i-1):(i), 
                                     eqn, 
                                     parms = para, 
                                     N = N, 
                                     atol = 1)[2,-1]))
  }  
  return(comp_num)
}

likelihood <- function(init, para, alp, N, time_length, dat, dpa){
  comp_num <- Dynamic(time_length, para, alp, init, N)
  fit_inC <- incre_tr(comp_num[,4])
  fit_inC[fit_inC < 10^(-10)] <- 10^(-10)
  
  Lik <- sum(dnbinom(dat, mu = fit_inC, size = dpa, log = T))
  
  if(is.finite(Lik) == F){
    Lik <- -10^(10)
  }
  
  return(Lik)
}

gibbs <- function(para_init, init, N, time_length, dat){ 
  
  para <- para_init[[1]]
  alp <- para_init[[2]]
  dpa <- para_init[[3]]
  max_l <- para_init[[4]]
  
  mean_para <- c(1, 2, 5.1, 9.5)
  for(i in 2:4){
    if(runif(1) < 0.5){
      a <- 1.1
    }else{
      a <- 1.01
    }
    para_t <- para
    para_t[i] <- rlnorm(1, log(para[i]), log(a))
    l_t <- likelihood(init, para_t, alp, N, time_length, dat, dpa)
    r <- l_t - max_l +
      dlnorm(para[i], log(para_t[i]), log(a), log = T) -
      dlnorm(para_t[i], log(para[i]), log(a), log = T) +
      dlnorm(para_t[i], log(mean_para[i]), log(1.1), log = T) -
      dlnorm(para[i], log(mean_para[i]), log(1.1), log = T)
    U <- log(runif(1))
    if(U < r){
      para <- para_t
      max_l <- l_t
    }
  }
  
  mean_alpha <- c(7, 7, 0.5, 1)
  for(i in 1:4){
    if(runif(1) < 0.5){
      a <- 1.1
    }else{
      a <- 1.01
    }
    alp_t <- alp
    alp_t[i] <- rlnorm(1, log(alp[i]), log(a))
    l_t <- likelihood(init, para, alp_t, N, time_length, dat, dpa)
    r <- l_t - max_l + 
      dlnorm(alp[i], log(alp_t[i]), log(a), log = T) - 
      dlnorm(alp_t[i], log(alp[i]), log(a), log = T) +
      dlnorm(alp_t[i], log(mean_alpha[i]), log(2), log = T) -
      dlnorm(alp[i], log(mean_alpha[i]), log(2), log = T)
    U <- log(runif(1))
    if(U < r){
      alp <- alp_t
      max_l <- l_t
    }
  }
  
  if(runif(1) < 0.5){
    a <- 1.1
  }else{
    a <- 1.01
  }
  dpa_t <- rlnorm(1, log(dpa), log(a))
  l_t <- likelihood(init, para, alp, N, time_length, dat, dpa_t)
  r <- l_t - max_l + 
    dlnorm(dpa, log(dpa_t), log(a), log = T) - 
    dlnorm(dpa_t, log(dpa), log(a), log = T) 
  U <- log(runif(1))
  if(U < r){
    dpa <- dpa_t
    max_l <- l_t
  }
  
  return(list(para, alp, dpa, max_l))
}

