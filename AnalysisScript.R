# Analysis script
# 
# Accompanies the Paper:
#   Chaochen Wang, Tatsunori Ikemoto, et al. (2020) Assessment of locomotive syndrome among older individuals: 
# a confirmatory factor analysis of the 25-question Geriatric Locomotive Function Scale
#==============================================================================================================


 #------------------------------------------------------
 # READ THE DATA

library(readxl) # install readxl if needed

Locomo25_data <- read_excel("Locomo25 data20190411.xlsx")
names(Locomo25_data) <- c("ID", "Age", "Sex", paste(rep("Q", 25), 1:25, sep = ""), "Total")
head(Locomo25_data)


 #------------------------------------------------------
 # LOOK AT THE DISTRIBUTION OF THE DATA

epiDisplay::summ(Locomo25_data$Total) # install epiDisplay if needed

epiDisplay::summ(Locomo25_data$Age)

epiDisplay::tab1(Locomo25_data$Sex)


library(tidyverse)
Locomo25_data %>% # the number of patients for each grade of locomotive syndrome
  mutate(loco_diag = cut(Total, breaks = c(-1, 7, 16, 100), right = FALSE)) %>% 
  group_by(loco_diag) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))  


 #------------------------------------------------------
 # CALCULATE CRONBACH ALPHA

al <- psych::alpha(Locomo25_data[, 4:28])

psych::alpha.ci(al$total$raw_alpha, n.obs = 500, p.val = 0.05, digits = 4)


 #------------------------------------------------------
 # PREPARE DATA FOR MPLUS SOFTWARE


library(MplusAutomation) # install if needed

prepareMplusData(Locomo25_data, "Locomo25.dat")
