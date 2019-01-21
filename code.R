library(dplyr)
library(tidyr)
library(tidyverse)
library(gplots)
library(simstudy)
library(lme4)
library(VCA)

rm(list=ls())
N    <- 10
t    <- 5
x    <- rep(1:t,N)

#task1
beta1 <- 0
e1    <- rnorm(N*t, mean = 0, sd = 0.3)
y1    <- 100 + x * beta1 + e1

#task2
beta2 <- 0
e2    <- rnorm(N*t, mean = 0, sd = 0.3)
y2    <- 100 + x * beta2 + e2

data1 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y1, task=rep(c("run1"),length(y1)))
data2 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y2, task=rep(c("run2"),length(y2)))
data <- rbind(data1, data2)



p <- ggplot(data = data, aes(x = day, y = y, group = id))
p+geom_point() + facet_grid(. ~ id+task)


m1 <- lmer(y ~ day + (1 | id), data=data)




# tdef <- defData( varname = "T0", dist = "normal", formula = 100, variance = 50)
# tdef1 <- defData( varname = "T1", dist = "normal", formula = 100+8, variance = 25)
# tdef2 <- defData( varname = "T2", dist = "normal", formula = 100-6, variance = 40)
# tdef3 <- defData(varname = "T3", dist = "normal", formula = 100+3, variance = 15)
# tdef4 <- defData( varname = "T4", dist = "normal", formula = 100+12, variance = 10)
# tdef5 <- defData( varname = "T5", dist = "normal", formula = 100+3, variance = 32)
# tdef6 <- defData( varname = "T6", dist = "normal", formula = 100-20, variance = 15)
# tdef7 <- defData( varname = "T7", dist = "normal", formula = 100+5, variance = 18)
# tdef8 <- defData( varname = "T8", dist = "normal", formula = 100-15, variance = 25)
# tdef9 <- defData( varname = "T9", dist = "normal", formula = 100+5, variance = 2)
# tdef10 <- defData( varname = "T10", dist = "normal", formula = 100+9, variance = 18)
# 
# dtTrial <- genData(10, tdef)
# #dtTrial
# sub1.r1<-genData(10,tdef1)
# sub1.r2<-genData(10,tdef1)
# 
# sub2<-genData(10,tdef2)
# sub3<-genData(10,tdef3)
# sub4<-genData(10,tdef4)
# sub5<-genData(10,tdef5)
# sub6<-genData(10,tdef6)
# sub7<-genData(10,tdef7)
# sub8<-genData(10,tdef8)
# sub9<-genData(10,tdef9)
# sub10<-genData(10,tdef10)
# 
# s1.1<-as.data.frame(t(sub1.r1$T1))
# s1.2<-as.data.frame(t(sub1.r2$T1))
# s2<-as.data.frame(t(sub2$T2))
# s3<-as.data.frame(t(sub3$T3))
# s4<-as.data.frame(t(sub4$T4))
# s5<-as.data.frame(t(sub5$T5))
# s6<-as.data.frame(t(sub6$T6))
# s7<-as.data.frame(t(sub7$T7))
# s8<-as.data.frame(t(sub8$T8))
# s9<-as.data.frame(t(sub9$T9))
# s10<-as.data.frame(t(sub10$T10))
# 
# x<-rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
# dtTrial<-cbind(dtTrial,x)
# names(dtTrial)[2:12]<-c("t0","t1","t2","t3","t4","t5","t6","t7","t8", "t9","t10")
# 
# 
# df<-dtTrial%>% 
#   gather(prelievo,value, 2:12) %>% 
#   arrange(id)
# 
# df
# 
# 
# p <- ggplot(data = df, aes(x = prelievo, y = value, group = id))
# p+geom_line()+stat_smooth(aes(group = 1)) + facet_grid(. ~ id)
# 
# %>% 
#   group_by(id) %>% 
#   ggplot(aes(x=as.factor(id), y=value))+ stat_summary()+coord_flip()
#   
# dt<-dtTrial%>% 
#   gather(prelievo,value, 2:11) %>% 
# arrange(id)
# 
# fit <- anovaMM(value~prelievo/id, dt)
# inf <- VCAinference(fit, VarVC=TRUE)
# 
# 
# 
# fm1 <- lmer(value ~ prelievo+(1|id), data=dt)
# 
# ranef(fm1)
# 
# 


