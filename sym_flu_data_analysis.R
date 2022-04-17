#####
#Libraries
library(zoo) # moving averages        
library(tidyverse) # all tidyverse packages
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(poliscidata)
#####
#Read in file
sym_flu <- read.csv("C:/Users/t.ganz/Desktop/Flu - COVID/LetterToEditor/symp_report_total_counts.csv")
sym_flu$Date <- as.Date(sym_flu$Date, "%m/%d/%Y")
str(sym_flu)
View(sym_flu)
#####
# Figure 1A - rolling means
# Rolling means for positive counts
y <- rollmean(sym_flu$Total.day, k=7, fill=NA)
w <- rollmean(sym_flu$COVID.day, k=7, fill=NA)
v <- rollmean(sym_flu$FLU.A.day, k=7, fill=NA)

symflu_plot1 <- ggplot(sym_flu, aes(Date)) +
  geom_area(aes(y = y), size = 0.5, color="black", fill = rgb('0.7','0.7','0.7','0.4')) +
  geom_area(aes(y = w), size = 0.5, color="black", fill = rgb('0','0.4','1','0.4') ) +
  geom_area(aes(y = v), size = 0.5, color="black", fill = rgb('1','0','0','0.4') ) +
  scale_x_date('', date_breaks = '3 months', date_labels = "%b %Y")+
  labs(x = "Dates", y = "Number of Tests", tag = "A") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 14, hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5),
        axis.title.x = element_text(size = 12, hjust = .5, vjust = .5),
        axis.title.y = element_text(size = 14, hjust = .5),
        plot.background = element_rect(fill=rgb('0.96','0.94','0.86','0.5')))
  
symflu_plot1
#####
#Figure 1B - pos rate compare
# Slice data to focus on 01NOV - 28FEB
sym_flu_focus <- sym_flu[c(427:546), c(1,6:7)]
View(sym_flu_focus)
#mirror chart
w3 <- sym_flu_focus$X.COVID
v3 <- sym_flu_focus$X.FLUA

symflu_plot3 <- ggplot(sym_flu_focus, aes(Date)) +
  geom_col(aes(y = w3), color = "black", size = 0, fill = rgb('0','0.4','1','0.4')) +
  geom_col(aes(y = -v3), color = "black", size = 0, fill = rgb('1','0','0','0.4')) +
  scale_y_continuous(labels = abs)+
  scale_x_date('',date_breaks = '1 months', date_labels = "  %b \n %Y")+
  labs( y = "% Positivity Rate", tag = "B") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 14, hjust = .5, vjust = .5, angle = 90),
        axis.title.x = element_text(size = 14, hjust = .5, vjust = .5),
        axis.title.y = element_text(size = 0, hjust = .5, vjust = .5),
        plot.background = element_rect(fill=rgb('0.96','0.94','0.86','0.5')))+
  coord_flip()
  
symflu_plot3
#####
##Figure 1C - Growth Rate Compare
#Growth Rate
sym_flu_focus_cnts <- sym_flu[c(427:546), c(1:4)]
View(sym_flu_focus_cnts)
ww <- rollmean(sym_flu_focus_cnts$COVID.day, k=7, fill=NA)
vv <- rollmean(sym_flu_focus_cnts$FLU.A.day, k=7, fill=NA)
# COVID Growth Rates
length(ww)
i <- 0
growth_sympcov <- c()
while (i < (length(ww)+1)){
  growth_sympcov[(length(growth_sympcov)+1)] <- 
    ((ww[i+1]-ww[i])/ww[i])
  i <- i+1
}
growth_sympcov[is.na(growth_sympcov)] <- 0
growth_sympcov[is.infinite(growth_sympcov)] <- 0
growth_sympcov
ww_sum <- cumsum(growth_sympcov)
plot(sym_flu_focus_cnts$Date,cumsum(growth_sympcov), type='s')
# FLU Growth Rates
length(vv)
i <- 0
growth_sympflu <- c()
while (i < (length(vv)+1)){
  growth_sympflu[(length(growth_sympflu)+1)] <- 
    ((vv[i+1]-vv[i])/vv[i])
  i <- i+1
} 
growth_sympflu[is.na(growth_sympflu)] <- 0
growth_sympflu[is.infinite(growth_sympflu)] <- 0
growth_sympflu
vv_sum <- cumsum(growth_sympflu)
plot(sym_flu_focus_cnts$Date,cumsum(growth_sympflu), type='s')
# add cumsums to focus_cnts df
sym_flu_focus_cnts$COV_sum <- ww_sum
sym_flu_focus_cnts$FLU_sum <- vv_sum

cumsum_plot <- ggplot(sym_flu_focus_cnts, aes(Date))+
  geom_step(aes(y=COV_sum),color = rgb('0','0.4','1','0.8'), size = 2)+
  geom_step(aes(y=FLU_sum),color = rgb('1','0','0','0.8'), size = 2)+
  scale_x_date('', date_breaks = '3 weeks', date_labels = "%d %b %y",
               limits = as.Date(c('2021-11-01', '2022-02-28')))+
  labs(x = "Dates", y = " Cumulative \n Growth Rate", tag = "C") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, hjust = .7, vjust = 0.2),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        axis.title.x = element_text(size = 12, hjust = .5, vjust = .5),
        axis.title.y = element_text(size = 14, hjust = .5),
        plot.background = element_rect(fill=rgb('0.96','0.94','0.86','0.5')))

cumsum_plot
# Growth Rates between 27 DEC and 07 JAN - (56-67)
focus_rates <- sym_flu_focus_cnts[c(56:67), c(5:6)]
focus_rates
summary(focus_rates)

plot(sym_flu_focus_cnts$Date, growth_sympflu, type = 'l')
mean(growth_sympflu[56:67])*100
mean(growth_sympcov[56:67])*100
#####
#Figure 1D - SGTF compare
#load dataset for SGTF comparison
sym_comp <- read.csv("C:/Users/t.ganz/Desktop/Flu - COVID/LetterToEditor/symptomatic_method_compare.csv")
sym_comp$Date <- as.Date(sym_comp$Date, "%m/%d/%Y")
str(sym_comp)
View(sym_comp)
#original plot
# ifelse to determine color of data in C19
# SGTF <- ifelse(is.na(sym_comp$S),
#                rgb('0.5','0.5','0','0.7'),
#                rgb('0.7','0.7','0.7','0.7'))
# # generate plot
# symcomp_plot1 <- ggplot(sym_comp, aes(Date))+
#   geom_jitter(aes(y=C19, color = I(SGTF), size = 0.5)) +
#   scale_x_date('', date_breaks = '1 month', date_labels = "%b %Y")+
#   scale_y_continuous('Ct value')+
#   labs(tag = "D")+
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 12, hjust = .7, vjust = 1),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         axis.title.x = element_text(size = 12, hjust = .5, vjust = .5),
#         axis.title.y = element_text(size = 14, hjust = .5))+
#   theme(legend.position = "none")
# 
# symcomp_plot1
#columns / dotpolt
# new table - grouped by date SGTF = true/false
SGTF3 <- sym_comp %>% 
  group_by(Date) %>%
  count(is.na(S))
#change col name from is.na(S) due to issue
colnames(SGTF3)[2] <- "SGTF"
View(SGTF3)
#generate plot
symcomp_plot2 <- ggplot(SGTF3, aes(x=Date, y=n ))+
  geom_point(aes(color=SGTF), stat='identity', shape=20, size = 4.5)+
  scale_color_manual(values = c(rgb('0','0.4','1','0.8'), rgb('1','0.4','0','0.8')))+
  scale_x_date('', date_breaks = '1 month', date_labels = "%b %Y")+
  scale_y_continuous('Count')+
  labs(tag = "D")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, hjust = .7, vjust = 1),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        axis.title.x = element_text(size = 16, hjust = .5, vjust = .5),
        axis.title.y = element_text(size = 14, hjust = .5))+
  theme(legend.position = "none", plot.background = element_rect(fill=rgb('0.96','0.94','0.86','0.5')))

symcomp_plot2
#####
# Assemble Figure Panel
# code to assemble figure
right_corner <- plot_grid(cumsum_plot, symcomp_plot2, nrow = 2, align = 'v')
right_corner
bottom_row <- plot_grid(symflu_plot3,right_corner, ncol=2)
bottom_row
plot_grid(symflu_plot1, bottom_row, nrow=2, rel_heights = c(1,1.5))

ggsave("FluCov_13.jpg",
       units = "cm",
       dpi = 320)

ggsave("Fig1D.jpg",
       units = "cm",
       dpi = 320)
#####
# Additional Calculations
# weekly average and 95%CI
flu_week <- rollmean(sym_flu_focus$X.FLUA, k=7, fill=NA)
flu_week_mean <- unlist(flu_week[4:24])
flu_week_mean
mn <- mean(flu_week_mean)
sd <- sd(flu_week_mean)
n <- length(flu_week_mean)
se <- sd/sqrt(n)
mn-(1.96*se)
mn+(1.96*se)
mn
length(sym_flu_focus$X.FLUA)
sum(sym_flu$FLU.A.day[427:546])
sum(sym_flu$Total.day[427:546])
# this code examines the rates of symptomatic cases before flu assay
sym_flu_pre <- sym_flu[c(1:426), c(1:3,6)]
sym_focus_pre <- sym_flu_pre[c(1:273), c(1:4)]
View(sym_flu_pre) #counts from start to October 31 2021
View(sym_focus_pre) #large grey area from start to May 31,2021
h <- rollmean(sym_focus_pre$Total.day, k=7, fill=NA)
i <- rollmean(sym_focus_pre$COVID.day, k=7, fill=NA)
j <- rollmean(sym_focus_pre$X.COVID, k=7, fill=NA)
# symptomatic testing
symptest_avgpre <- unlist(h[4:270])
mn_pre <- mean(symptest_avgpre)
sd_pre <- sd(symptest_avgpre)
se_pre <- (sd_pre/sqrt(length(symptest_avgpre)))
summary(symptest_avgpre) # symptomatic cases
CI95(mn_pre, se_pre) # and CI
# covid cases in symptomatic
sympcov_avgpre <- unlist(i[4:270])
mn_cov_pre <- mean(sympcov_avgpre)
sd_cov_pre <- sd(sympcov_avgpre)
se_cov_pre <- (sd_cov_pre/sqrt(length(sympcov_avgpre)))
summary(sympcov_avgpre) # symptomatic cases
CI95(mn_cov_pre, se_cov_pre) # and CI
#Extra Math
(((1/10000)*35.5e6)/35.5e6)*100
(1800/3.1e6)*100
(3.1e4/3.1e6)*100
(10000/35.5e6)*100