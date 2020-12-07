# path = '/Users/zhangjingwen/Desktop/冠状病毒/统计分析/The-simulations_1128_v2/'
path <- "D:/Documents/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/"

load(paste0(path, "Code_data/Para_Shenzhen.rda"))
shenzhen = para
load(paste0(path, "Code_data/Para_Wenzhou.rda"))
wenzhou = para
load(paste0(path, "Code_data/Para_Wuhan.rda"))
wuhan = para

Rt <- function(para, time_length = 40) {
  sapply(1:length(para), function(k){
  rt <- f_alp(1:time_length, para[[k]][[2]])
  return(rt * para[[k]][[1]][2])
  })
}

Rt_Wuhan <- function(para, time_length = 40) {
  sapply(1:length(para), function(k){
    rt <- f_alp(1:time_length, para[[k]][[2]])
    rt <- c(rep(para[[k]][[1]][2], 21), rep(para[[k]][[6]], time_length - 21)) * rt
    return(rt)
  })
}

f_alp <- function(k, alp){
  alp[4] * (alp[3] / (1 + exp(2 * log(99) / alp[2] * (k - alp[1] - alp[2] / 2))) + 1 - alp[3])
}  

## Plot the time-varying reproduction number
Plot_ALL <- function(shenzhen, wenzhou, wuhan) {
  today.wh = as.Date("2020/01/15") 
  today.wz = as.Date("2020/01/21") 
  today.sz = as.Date("2020/01/19") 
  today.end = as.Date("2020/02/20") 

  time_lab <- format(seq.Date(from = today.wh, to =today.end, by = "day"), format = "%m/%d")
  len = length(time_lab)
  Rt_wh = rowMeans(Rt_Wuhan(wuhan))[1:len]
  Rt_wz = c(rep(NA, 6), rowMeans(Rt(wenzhou))[1:(len-6)])
  Rt_sz = c(rep(NA, 4), rowMeans(Rt(shenzhen))[1:(len-4)])
  dat_plot = data.frame(time_num = 1:len, time_lab, Rt_wh, Rt_wz, Rt_sz)

  names(dat_plot) <- c("time_num", 'time','wuhan', 'wenzhou', 'shenzhen')
  
  plot_out <- dat_plot %>%
    ggplot(aes(x = time_num)) + 
    geom_line(aes(y = wuhan, color = "Wuhan"), size = 1) +
    geom_line(aes(y = wenzhou, color = "Wenzhou"), size = 1) +
    geom_line(aes(y = shenzhen, color = "Shenzhen"), size = 1) +
    scale_x_continuous(breaks = dat_plot$time_num[1:length(dat_plot$time_num) %% 5 == 1], 
                       labels = dat_plot$time[1:length(dat_plot$time_num) %% 5 == 1]) +
    theme_bw(base_family = "Times") +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top",
          panel.border = element_blank(),
          text = element_text(family = "STHeiti"),
          axis.text.x = element_text(angle = 30)) +
    labs(x = "Date", y = "Individuals", 
         colour = "", shape = "")

  return(plot_out)
}


Plot_ALL(shenzhen=shenzhen, wenzhou=wenzhou, wuhan=wuhan) 


