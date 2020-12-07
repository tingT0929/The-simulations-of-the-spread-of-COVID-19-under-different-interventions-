library(ggplot2)
library(tidyverse)

# setwd("D:/SC2S2/pneumonia/paper/code/1129/")
setwd("D:/Documents/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/Plot/")

formatter <- function(x) {
  if(x < 1e5) {
    return(x)
  }
  level <- floor(log10(x))
  temp <- round(x / 10^level, digits = 2)
  return(paste0(temp, " %*% 10^", level))
}

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

plot_self <- function(result, dat_real, area = "South Korea", start_date, N) {
  start_date <- as.Date(start_date)
  # cum <- result$H + result$R
  cum <- result$H
  cum_pred <- cum[2,]
  date_num <- sum(cum_pred < N * 0.1) + 1
  if(date_num > ncol(result$I)) date_num <- ncol(result$I)
  
  # dat_real <- c(dat[,3], rep(NA, date_num - nrow(dat)))
  if(length(dat_real) < date_num) {
    dat_real <- c(dat_real, rep(NA, date_num - length(dat_real)))
  } else {
    dat_real <- dat_real[1:date_num]
  }
  dat_I <- t(result$I)[1:date_num,]
  dat_all <- t(result$H + result$R)[1:date_num,]
  date_len <- date_num
  
  # dat_E[1:7,] <- NA
  
  time_lab <- format(seq.Date(from = start_date, by = "day", length.out = date_num), format = "%m/%d")
  dat_plot <- data.frame(time_num = 1:date_len, dat_real, 
                         dat_I, dat_all, 
                         time_lab, stringAsFactors = FALSE)
  names(dat_plot) <- c("time_num", "real_num", 
                       "I_ci_l", "I_pred", "I_ci_h",
                       "all_ci_l", "all_pred", "all_ci_h",
                       "time_lab")
  
  na_vec_I1 <- na_vec_I2 <-  
    na_vec_all1 <- na_vec_all2 <- rep(NA, date_len)
  
  plot_out <- dat_plot %>%
    ggplot(aes(x = time_num)) +
    # geom_line(aes(y = I_pred, color = "Infected and infectious without isolation"), size = 1) +
    geom_line(aes(y = all_pred, color = "Fitting curve"), size = 1) +
    # geom_ribbon(aes(ymin = I_ci_l, ymax = I_ci_h, color = "Infected and infectious without isolation", fill = "Infected and infectious without isolation"), alpha = 0.1, linetype = 3) +
    geom_ribbon(aes(ymin = all_ci_l, ymax = all_ci_h), color = "red", fill = "red", alpha = 0.1, linetype = 3)+
    geom_point(aes(y = real_num, shape =  "Observed cases"), size = 2, color = "purple")
  
  plot_out <- plot_out + 
    scale_color_manual(values = c("red", "#0072B2")) +
    scale_fill_manual(values = c("red", "#0072B2")) +
    scale_y_continuous(labels = scientific_10) +
    theme_bw(base_family = "Times") +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top",
          panel.border = element_blank(),
          # legend.box = "vertical",
          # legend.spacing.y = unit(0.01, 'cm'),
          text = element_text(family = "STHeiti"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30)) +
    labs(x = "Date", y = "Number of Individuals", 
         title = "", 
         colour = "", shape = "", fill = "")
  
  if(area == "South Korea") {
    plot_out <- plot_out + 
      scale_x_continuous(breaks = dat_plot$time_num, labels = dat_plot$time_lab)
  } else {
    plot_out <- plot_out + 
      scale_x_continuous(breaks = dat_plot$time_num[1:length(dat_plot$time_num) %% 3 == 1],
                         labels = dat_plot$time_lab[1:length(dat_plot$time_num) %% 3 == 1])
  }
  
  return(plot_out)
}

load("D:/Documents/GitHub/The-simulations-of-the-spread-of-COVID-19-under-different-interventions-/Code_data/All_dat.rdata")

# Korea
load("result_Korea_timeinvariant.rda")
load("Korea.true(3).rda")
N <- all_list$Korea$Population
start_date <- as.Date("2020-02-19")
dat_real <- global.K$cum_confirm[which(global.K$time == start_date):nrow(global.K)]
# dat_real <- all_list$Korea$Data[, "H"]
pic_Korea <- plot_self(time_invariant, dat_real, 
                       area = "South Korea", start_date, N)
ggsave(pic_Korea, filename = "pic_Korea.png", width = 7.5, height = 4.5, dpi = 300)


# Italy
load("result_Italy_timeinvariant.rda")
load("Italy.true(3).rda")
N <- all_list$Italy$Population
start_date <- as.Date("2020-02-25")
dat_real <- global.I$cum_confirm[which(global.I$time == start_date):nrow(global.I)]
# dat_real <- all_list$Italy$Data[, "H"]
pic_Italy <- plot_self(time_invariant, dat_real, 
                       area = "Italy", start_date, N)
ggsave(pic_Italy, filename = "pic_Italy.png", width = 7.5, height = 4.5, dpi = 300)

# US
load("result_America_timeinvariant.rda")
load("Unite.true(3).rda")
N <- all_list$America$Population
start_date <- as.Date("2020-03-08")
dat_real <- all_list$America$Data[, "H"]
pic_America <- plot_self(time_invariant, dat_real, 
                         area = "the United States", start_date, N)
ggsave(pic_America, filename = "pic_America.png", width = 7.5, height = 4.5, dpi = 300)
