setwd("~/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-")

source("Code/Epidemic_modeling.R")

load("Result/Para_Shenzhen.rda")
shenzhen <- para
load("Result/Para_Wenzhou.rda")
wenzhou <- para
load("Result/Para_Wuhan.rda")
wuhan <- para

region_mark = 4 # 1, 2, ..., 6
load("Data/All_dat.RDATA")
dat <- all_list[[region_mark]]$Data
N <- all_list[[region_mark]]$Population
region <- all_list[[region_mark]]$Region

Rt_Wuhan <- function(para, time_length = 40) {
  sapply(1:length(para), function(k){
    rt <- f_alp(1:time_length, para[[k]][[2]])
    rt <- c(rep(para[[k]][[1]][2], 21), rep(para[[k]][[6]], time_length - 21)) * rt
    return(rt)
  })
}

Rt <- function(para_t) {
  sapply(1:length(para_t), function(k){
    rt <- f_alp(1:time_length, para_t[[k]][[2]])
    return(rt * para_t[[k]][[1]][2])
  })
}

## Initial 
init <- c(N, 0, dat[1,3] - dat[1,4], dat[1,4])
time_length=40

Dynamic_p <- function(time_length, para_i, alp, init, N, I_init, city){
  if(city == 'wuhan') {
    alp[2] = rowMeans(sapply(1:length(wuhan), function(k){wuhan[[k]][[2]]}))[2]
  } else if(city == 'wenzhou') {
    alp[2] = rowMeans(sapply(1:length(wenzhou), function(k){wenzhou[[k]][[2]]}))[2]
  } else if(city == 'shenzhen') {
    alp[2] = rowMeans(sapply(1:length(shenzhen), function(k){shenzhen[[k]][[2]]}))[2]
  }
  alp[3] = 1
  
  comp_num <- matrix(init, nrow = 1)
  comp_num[2] <- I_init
  comp_num[1] <- N - sum(comp_num[-1])
  for(i in 2:time_length){
    para_i[1] <- f_alp(i-1, alp)
    comp_num <- rbind(comp_num,
                      as.numeric(ode(y = comp_num[i-1,], 
                                     times = (i-1):(i), 
                                     eqn, 
                                     parms = para_i, 
                                     N = N, 
                                     atol = 1)[2,-1]))
  }  
  return(comp_num)
}

pred <- function(k, time_length, para, init, N, city){
  comp_num <- Dynamic_p(time_length, para[[k]][[1]], para[[k]][[2]], init, N, para[[k]][[4]], city)
  colnames(comp_num) <- c("S", "I", "H", "R")
  return(comp_num)
}

####---------- running
load(paste0('Result/Para_', region, ".rda"))

list_sta <- list()
list_end <- list()
for(city in c('wuhan', 'wenzhou', 'shenzhen')) {
  list_sta[[city]] <- lapply(1:length(para), pred, para = para, time_length = 150, init = init, N = N, city = city)
  pred_array <- array(0, c(length(para), time_length = 150, 4))
  for(i in 1:length(para)){
    pred_array[i,,] <- list_sta[[city]][[i]]
  }
  list_end[[city]] <- list()
  list_end[[city]][['I']] <- sapply(1:150, function(j)
    {quantile(pred_array[,j,2], c(0.025, 0.5, 0.975))})
  list_end[[city]][['H']] <- sapply(1:150, function(j)
  {quantile(pred_array[,j,3], c(0.025, 0.5, 0.975))})
  list_end[[city]][['R']] <- sapply(1:150, function(j)
  {quantile(pred_array[,j,4], c(0.025, 0.5, 0.975))})
}

save(list_end, file = paste0('Result/', region, "_mi.rda"), version = 2)





