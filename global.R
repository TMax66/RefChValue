library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(broom)



# mod = lmer(y ~ (1|id/prelievo), data=df)
# V<-tidy(mod)
# V<-V %>% 
#   dplyr::select(estimate) %>% 
#   mutate(CV=100*(estimate/estimate[1]))
# CVi<-V$CV[4]
# CVg<-V$CV[3]
# CVa<-V$CV[2]
# Ioi<-((CVi^2+CVa^2)^0.5)/CVg
# RCV<-(1.96*2^0.5)*(CVi^2+CVa^2)^0.5

