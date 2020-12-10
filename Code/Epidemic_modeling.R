
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
  a <- dat[-1,3] - dat[-nrow(dat),3]
  b <- dat[-1,4] - dat[-nrow(dat),4]
  a[a < 10^(-10)] <- 10^(-10)
  b[b < 10^(-10)] <- 10^(-10)
  list(a, b)
}

eqn <- function(time, init, para, N){
  
  dS <- - para[1] * init[2] * init[1]  / N
  rI <- init[2] / para[2]
  dI <- - dS - rI
  
  dR <- init[3] / para[3] 
  dH <- rI - dR
  
  return(list(c(dS, dI, dH, dR)))
}

Dynamic <- function(time_length, para, alp, init, N, I_init, wh_incu, region){
  comp_num <- matrix(init, nrow = 1)
  comp_num[2] <- I_init
  comp_num[1] <- N - sum(comp_num[-1])
  if(region == "Wuhan"){
    for(i in 2:21){
        para[1] <- f_alp(i-1, alp)
        comp_num <- rbind(comp_num,
                          as.numeric(ode(y = comp_num[i-1,], 
                                         times = (i-1):(i), 
                                         eqn, 
                                         parms = para, 
                                         N = N, 
                                         atol = 1)[2,-1]))
    }
    
    para[2] <- wh_incu
    for(i in 22:time_length){
      para[1] <- f_alp(i-1, alp)
      comp_num <- rbind(comp_num,
                        as.numeric(ode(y = comp_num[i-1,], 
                                       times = (i-1):(i), 
                                       eqn, 
                                       parms = para, 
                                       N = N, 
                                       atol = 1)[2,-1]))
    }
    
  }else{
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
  }
    
  return(comp_num)
}

likelihood <- function(init, para, alp, N, time_length, dat, dpa, I_init, wh_incu, region){
  comp_num <- Dynamic(time_length, para, alp, init, N, I_init, wh_incu, region)
  fit_inC <- incre_tr(comp_num)
  
  Lik <- sum(sapply(1:2, function(k){
    sum(dnbinom(dat[[k]], mu = fit_inC[[k]], size = dpa[k], log = T))
  }))
    
  if(is.finite(Lik) == F){
    Lik <- -10^(10)
  }
  
  return(Lik)
}

gibbs <- function(para_init, init, N, time_length, dat, region){ 
  
  para <- para_init[[1]]
  alp <- para_init[[2]]
  dpa <- para_init[[3]]
  I_init <- para_init[[4]]
  max_l <- para_init[[5]]
  wh_incu <- para_init[[6]]
  
  for(i in 3){
    if(runif(1) < 0.5){
      a <- 2
    }else{
      a <- 1.1
    }
    para_t <- para
    para_t[i] <- rlnorm(1, log(para[i]), log(a))
    l_t <- likelihood(init, para_t, alp, N, time_length, dat, dpa, I_init, wh_incu, region)
    r <- l_t - max_l +
      dlnorm(para[i], log(para_t[i]), log(a), log = T) -
      dlnorm(para_t[i], log(para[i]), log(a), log = T) 
    U <- log(runif(1))
    if(U < r){
      para <- para_t
      max_l <- l_t
    }
  }
  
  for(i in c(2)){
    if(runif(1) < 0.5){
      a <- 2
    }else{
      a <- 1.1
    }
    alp_t <- alp
    alp_t[i] <- rlnormTrunc(1, log(alp[i]), log(a), min = 0, max = 30)
    l_t <- likelihood(init, para, alp_t, N, time_length, dat, dpa, I_init, wh_incu, region)
    r <- l_t - max_l + 
      log(dlnormTrunc(alp[i], log(alp_t[i]), log(a), min = 0, max = 30)) - 
      log(dlnormTrunc(alp_t[i], log(alp[i]), log(a), min = 0, max = 30))
      # dlnorm(alp[i], log(alp_t[i]), log(a), log = T) - 
      # dlnorm(alp_t[i], log(alp[i]), log(a), log = T) 
    U <- log(runif(1))
    if(U < r){
      alp <- alp_t
      max_l <- l_t
    }
  }
  
  for(i in c(4)){
    if(runif(1) < 0.5){
      a <- 2
    }else{
      a <- 1.1
    }
    alp_t <- alp
    alp_t[i] <- rlnorm(1, log(alp[i]), log(a))
    l_t <- likelihood(init, para, alp_t, N, time_length, dat, dpa, I_init, wh_incu, region)
    r <- l_t - max_l + 
      dlnorm(alp[i], log(alp_t[i]), log(a), log = T) - 
      dlnorm(alp_t[i], log(alp[i]), log(a), log = T) 
    U <- log(runif(1))
    if(U < r){
      alp <- alp_t
      max_l <- l_t
    }
  }
  
  for(i in 1:2){
    if(runif(1) < 0.5){
      a <- 2
    }else{
      a <- 1.1
    }
    dpa_t <- dpa
    dpa_t[i] <- rlnorm(1, log(dpa[i]), log(a))
    l_t <- likelihood(init, para, alp, N, time_length, dat, dpa_t, I_init, wh_incu, region)
    r <- l_t - max_l + 
      dlnorm(dpa[i], log(dpa_t[i]), log(a), log = T) - 
      dlnorm(dpa_t[i], log(dpa[i]), log(a), log = T) 
    U <- log(runif(1))
    if(U < r){
      dpa[i] <- dpa_t[i]
      max_l <- l_t
    }
  }
  
  if(runif(1) < 0.5){
    a <- 2
  }else{
    a <- 1.1
  }
  I_init_t <- rlnorm(1, log(I_init), log(a))
  l_t <- likelihood(init, para, alp, N, time_length, dat, dpa, I_init_t, wh_incu, region)
  r <- l_t - max_l + 
    dlnorm(I_init, log(I_init_t), log(a), log = T) - 
    dlnorm(I_init_t, log(I_init), log(a), log = T) 
  U <- log(runif(1))
  if(U < r){
    I_init <- I_init_t
    max_l <- l_t
  }
  
  if(region == "Wuhan"){
    for(i in 2){
      para_t <- para
      para_t[i] <- rlnormTrunc(1, log(para[i]), log(a), wh_incu, 30)
      l_t <- likelihood(init, para_t, alp, N, time_length, dat, dpa, I_init, wh_incu, region)
      r <- l_t - max_l +
        log(dlnormTrunc(para[i], log(para_t[i]), log(a), wh_incu, Inf)) -
        log(dlnormTrunc(para_t[i], log(para[i]), log(a), wh_incu, Inf)) +
        dtnorm(para_t[i], 16.1, 0.325, a = wh_incu, b = 30, log = T) -
        dtnorm(para[i], 16.1, 0.325, a = wh_incu, b = 30, log = T)
      U <- log(runif(1))
      if(U < r){
        para <- para_t
        max_l <- l_t
      }
    }
    
    wh_incu_t <- rlnormTrunc(1, log(wh_incu), log(a), 0, para[2])
    l_t <- likelihood(init, para, alp, N, time_length, dat, dpa, I_init, wh_incu_t, region)
    r <- l_t - max_l +
      log(dlnormTrunc(wh_incu, log(wh_incu_t), log(a), 0, para[2])) -
      log(dlnormTrunc(wh_incu_t, log(wh_incu), log(a), 0, para[2])) +
      dtnorm(wh_incu_t, 5.1, 0.325, a = 0, b = para[2], log = T) -
      dtnorm(wh_incu, 5.1, 0.325, a = 0, b = para[2], log = T)
    U <- log(runif(1))
    if(U < r){
      wh_incu <- wh_incu_t
      max_l <- l_t
    }
    
  }else{
    for(i in 2){
      para_t <- para
      para_t[i] <- rlnormTrunc(1, log(para[i]), log(a), 0, 30)
      l_t <- likelihood(init, para_t, alp, N, time_length, dat, dpa, I_init, wh_incu, region)
      r <- l_t - max_l +
        log(dlnormTrunc(para[i], log(para_t[i]), log(a), 0, 30)) -
        log(dlnormTrunc(para_t[i], log(para[i]), log(a), 0, 30)) +
        dtnorm(para_t[i], 5.1, 0.325, a = 0, b = 30, log = T) -
        dtnorm(para[i], 5.1, 0.325, a = 0, b = 30, log = T)
      U <- log(runif(1))
      if(U < r){
        para <- para_t
        max_l <- l_t
      }
    }
  }
  
  return(list(para, alp, dpa, I_init, max_l, wh_incu))
}

