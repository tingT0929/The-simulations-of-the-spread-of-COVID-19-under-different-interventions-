## Data import
load("~/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/Data/All_dat.RDATA")
dat <- all_list[[region_mark]]$Data
N <- all_list[[region_mark]]$Population
region <- all_list[[region_mark]]$Region
Policy <- all_list[[region_mark]]$Policy


## Initial 
time_length <- nrow(dat)
init <- c(N, 0, dat[1,3] - dat[1,4], dat[1,4])
dat <- incre_tr(dat)



