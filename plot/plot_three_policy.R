setwd("D:/SC2S2/pneumonia/paper/code/1129/")

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

## plot
Plot_ALL.2 <- function(result.wh, result.wz,result.sz, dat_real, start_date, end_date, area) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  date_num <- as.numeric(end_date - start_date + 1)
  dat_real <- c(dat_real, rep(NA, date_num - length(dat_real)))
  dat_I <- rbind(t(result.wh$I)[1:date_num, ],  t(result.wz$I)[1:date_num, ], t(result.sz$I)[1:date_num, ])
  dat_H <- rbind(t(result.wh$H)[1:date_num, ],  t(result.wz$H)[1:date_num, ], t(result.sz$H)[1:date_num, ])
  dat_R <- rbind(t(result.wh$R)[1:date_num, ],  t(result.wz$R)[1:date_num, ], t(result.sz$R)[1:date_num, ])
  dat_all <- dat_H + dat_R
  date_len <- date_num
  
  time_lab <- rep(format(seq.Date(from = start_date, by = "day", length.out = date_len ), format = "%m/%d"), 3)
  class.bl <- as.factor(c(rep(1, date_num), rep(2, date_num), rep(3, date_num)))
  dat_plot <- data.frame(time_num = 1:date_len, dat_real, dat_I, dat_all, class.bl,
                         time_lab, stringsAsFactors = FALSE)
  names(dat_plot) <- c("time_num", "real_num", 
                       "I_ci_l", "I_pred", "I_ci_h", 
                       "all_ci_l", "all_pred", "all_ci_h", 
                       "class", "time_lab")
  
  plot_title <- paste0("The Simulation of COVID-19 trend after ", area , " adopted three measures")
  
  na_vec_all1 <- na_vec_all2 <- 
    now.time1 <- now.time2 <- rep(NA, date_len * 3)
  
    na_vec_all1[c(which.max(dat_plot$all_pred[c(1:date_len)]), date_len+which.max(dat_plot$all_pred[date_len +c(1:date_len)]), 
              date_len*2+which.max(dat_plot$all_pred[date_len*2 +c(1:date_len)]))] <- 
    round(dat_plot$all_pred[c(which.max(dat_plot$all_pred[c(1:date_len)]),date_len+ which.max(dat_plot$all_pred[date_len +c(1:date_len)]), 
                            date_len*2+which.max(dat_plot$all_pred[date_len*2 +c(1:date_len)]))])
  na_vec_all2[c(which.max(dat_plot$all_pred[c(1:date_len)]), date_len+which.max(dat_plot$all_pred[date_len +c(1:date_len)]), 
              date_len*2+which.max(dat_plot$all_pred[date_len*2 +c(1:date_len)]))] <- 
    paste0(time_lab[c(which.max(dat_plot$all_pred[c(1:date_len)]), date_len+which.max(dat_plot$all_pred[date_len +c(1:date_len)]), 
                      date_len*2+which.max(dat_plot$all_pred[date_len*2 +c(1:date_len)]))], ":")
  
  to_string <- as_labeller(c(`1` =  "Policy Pattern of Wuhan", `2` =  "Policy Pattern of Wenzhou", 
                             `3` =  "Policy Pattern of Shenzhen"))
  
  plot_out <- dat_plot %>%
    ggplot(aes(x = time_num)) + 
    geom_line(aes(y = I_pred, color = "Infected and infectious without quarantine"), size = 1) +
    geom_line(aes(y = all_pred, color = "Cumulative confirmed cases"), size = 1) +
    facet_wrap(vars(class), nrow = 1,shrink = F, labeller = to_string) + 
    geom_ribbon(aes(ymin = I_ci_l, ymax = I_ci_h, color = "Infected and infectious without quarantine", fill = "Infected and infectious without quarantine"), alpha = 0.1, linetype = 3) +
    geom_ribbon(aes(ymin = all_ci_l, ymax = all_ci_h, color = "Cumulative confirmed cases", fill = "Cumulative confirmed cases"), alpha = 0.1, linetype = 3) +
    geom_point(aes(y = real_num, shape =  "Actual cumulative confirmed cases"), size = 2, color = "purple")
    
  if(area == "South Korea") {
    plot_out <- plot_out + 
      geom_text(aes(y = na_vec_all1, label = na_vec_all1), hjust = -1, vjust = 1.5,size = 4) + # 1
      geom_text(aes(y = na_vec_all1, label = na_vec_all2), hjust = 0.2, vjust = 1.5,size = 4) + # 2
      scale_x_continuous(breaks = c(dat_plot$time_num[1:(length(dat_plot$time_num)/3) %% 8 == 1], dat_plot$time_num[length(dat_plot$time_num) / 3]), 
                         labels = c(dat_plot$time_lab[1:(length(dat_plot$time_num)/3) %% 8 == 1], dat_plot$time_lab[length(dat_plot$time_num) / 3]))
  } else if(area == "Italy") {
    plot_out <- plot_out + 
      geom_text(aes(y = na_vec_all1, label = na_vec_all1), hjust = 0.8, vjust = 1.5,size = 4) + # 1
      geom_text(aes(y = na_vec_all1, label = na_vec_all2), hjust = 2, vjust = 1.5,size = 4) + # 2
      scale_x_continuous(breaks = c(dat_plot$time_num[1:(length(dat_plot$time_num)/3) %% 5 == 1], dat_plot$time_num[length(dat_plot$time_num) / 3]), 
                         labels = c(dat_plot$time_lab[1:(length(dat_plot$time_num)/3) %% 5 == 1], dat_plot$time_lab[length(dat_plot$time_num) / 3]))
  } else {
    plot_out <- plot_out + 
      geom_text(aes(y = na_vec_all1, label = na_vec_all1), hjust = 0.8, vjust = 1.5,size = 4) + # 1
      geom_text(aes(y = na_vec_all1, label = na_vec_all2), hjust = 2, vjust = 1.5,size = 4) + # 2
      scale_x_continuous(breaks = c(dat_plot$time_num[1:(length(dat_plot$time_num)/3) %% 5 == 1], dat_plot$time_num[length(dat_plot$time_num) / 3]), 
                         labels = c(dat_plot$time_lab[1:(length(dat_plot$time_num)/3) %% 5 == 1], dat_plot$time_lab[length(dat_plot$time_num) / 3]))
  } 
    
  plot_out <- plot_out + 
    scale_color_manual(values = c("red", "#0072B2")) +
    scale_fill_manual(values = c("red", "#0072B2")) +
    scale_y_continuous(labels = scientific_10) +
    theme_bw(base_family = "Times") +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(),
          text = element_text(family = "STHeiti"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30)) +
    labs(x = "Date", y = "Number of Individuals", 
         title = "", 
         colour = "", shape = "", fill = "")
  
    return(plot_out)
}

end_date <- "2020-05-31"

library(ggplot2)
library(tidyverse)
load("All_dat.rdata")

# Korea
start_date <- "2020-02-26"
load("Korea_mi.rda")
dat_real <- all_list$Korea$Data[, "H"] + all_list$Korea$Data[, "R"]
pre_Korea <- Plot_ALL.2(list_end[[1]], list_end[[2]], list_end[[3]], dat_real, start_date, end_date, area = "South Korea")
ggsave(pre_Korea, filename = "pre_Korea.png", width = 11, height = 4, dpi = 300)

# Italy
start_date <- "2020-03-28"
load("Italy_mi.rda")
dat_real <- all_list$Italy$Data[, "H"] + all_list$Italy$Data[, "R"]
pre_Italy <- Plot_ALL.2(list_end[[1]], list_end[[2]], list_end[[3]], dat_real, start_date, end_date, area = "Italy")
ggsave(pre_Italy, filename = "pre_Italy.png", width = 11, height = 4, dpi = 300)

# USA
start_date <- "2020-03-29"
load("America_mi.rdata")
dat_real <- all_list$America$Data[, "H"] + all_list$America$Data[, "R"]
pre_America <- Plot_ALL.2(list_end[[1]], list_end[[2]], list_end[[3]], dat_real, start_date, end_date, area = "the United States")
ggsave(pre_America, filename = "pre_America.png", width = 11, height = 4, dpi = 300)
