source("Code/Epidemic_modeling.R")

## Specify region
region_mark <- 9

## Data import
load(paste0(path, "Data/All_dat.rdata"))
dat <- all_list[[region_mark]]$Data
N <- all_list[[region_mark]]$Population
region <- all_list[[region_mark]]$Region
Policy <- all_list[[region_mark]]$Policy


## Initial 
time_length <- nrow(dat)
init <- c(N, 0, dat[1,3]-  dat[1,4], dat[1,4])
dat <- incre_tr(dat)

load(paste0(path, 'Result/Para_', region, ".rda"))

pred <- function(k, time_length, para, init, N, region){
  comp_num <- Dynamic(time_length, para[[k]][[1]], para[[k]][[2]], init, N, para[[k]][[4]], region = region)
  colnames(comp_num) <- c("S", "I", "H", "R")
  return(comp_num)
}
time_length = 150
Result <- lapply(1:length(para), pred, para = para, time_length = time_length, init = init, N = N, region = region)

pred_array <- array(0, c(length(para), time_length, 4))
for(i in 1:length(para)){
  pred_array[i,,] <- Result[[i]] 
}
time_invariant <- list()
time_invariant[['I']] <- sapply(1:150, function(j)
{quantile(pred_array[,j,2], c(0.025, 0.5, 0.975))})
time_invariant[['H']] <- sapply(1:150, function(j)
{quantile(pred_array[,j,3], c(0.025, 0.5, 0.975))})
time_invariant[['R']] <- sapply(1:150, function(j)
{quantile(pred_array[,j,4], c(0.025, 0.5, 0.975))})

save(time_invariant, file = paste0(path, 'Result/result_', region, '.rda'),version=2)

