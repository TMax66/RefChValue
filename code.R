library(dplyr)
library(tidyr)
library(tidyverse)
library(gplots)
library(simstudy)
library(lme4)
library(VCA)
library(purrr)
library(broom)

rm(list=ls())
N    <- 10
t    <- 5
x    <- rep(1:t,N)

#task1
beta1 <- 2
e1    <- rnorm(N*t, mean = 0, sd = 1)
y1    <- 100 + x * beta1 + e1

#task2
beta2 <- 5
e2    <- rnorm(N*t, mean = 0, sd = 1.5)
y2    <- 100 + x * beta2 + e2

data1 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y1, task=rep(c("run1"),length(y1)))
data2 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y2, task=rep(c("run2"),length(y2)))
data <- rbind(data1, data2)



p <- ggplot(data = data, aes(x = day, y = y, group = id))
p+geom_point() + facet_grid(. ~ id+task)


mod1 = lmer(y ~ day+(1|dat/id), data=data)

fit<-fitVCA(y~day+(day/id), data, "reml")



######################################
rm(list=ls())
tdef <- defData( varname = "T0", dist = "normal", formula = 100, variance = 50)
tdef1 <- defData( varname = "T1", dist = "normal", formula = 100+8, variance = 25)
tdef2 <- defData( varname = "T2", dist = "normal", formula = 100-6, variance = 40)
tdef3 <- defData(varname = "T3", dist = "normal", formula = 100+3, variance = 15)
tdef4 <- defData( varname = "T4", dist = "normal", formula = 100+12, variance = 10)
tdef5 <- defData( varname = "T5", dist = "normal", formula = 100+3, variance = 32)
# tdef6 <- defData( varname = "T6", dist = "normal", formula = 100-20, variance = 15)
# tdef7 <- defData( varname = "T7", dist = "normal", formula = 100+5, variance = 18)
# tdef8 <- defData( varname = "T8", dist = "normal", formula = 100-15, variance = 25)
# tdef9 <- defData( varname = "T9", dist = "normal", formula = 100+5, variance = 2)
# tdef10 <- defData( varname = "T10", dist = "normal", formula = 100+9, variance = 18)

dtTrial <- genData(5, tdef)
#dtTrial
sub1<-genData(5,tdef1)
sub2<-genData(5,tdef2)
sub3<-genData(5,tdef3)
sub4<-genData(5,tdef4)
sub5<-genData(5,tdef5)
# sub6<-genData(10,tdef6)
# sub7<-genData(10,tdef7)
# sub8<-genData(10,tdef8)
# sub9<-genData(10,tdef9)
# sub10<-genData(10,tdef10)

s1<-as.data.frame(t(sub1$T1))
s2<-as.data.frame(t(sub2$T2))
s3<-as.data.frame(t(sub3$T3))
s4<-as.data.frame(t(sub4$T4))
s5<-as.data.frame(t(sub5$T5))
# s6<-as.data.frame(t(sub6$T6))
# s7<-as.data.frame(t(sub7$T7))
# s8<-as.data.frame(t(sub8$T8))
# s9<-as.data.frame(t(sub9$T9))
# s10<-as.data.frame(t(sub10$T10))

x<-rbind(s1,s2,s3,s4,s5)
dtTrial<-cbind(dtTrial,x)
names(dtTrial)[2:7]<-c("t0","t1","t2","t3","t4","t5")


df<-dtTrial%>%
  gather(prelievo,value, 2:7) %>%
  arrange(id)

df$value2<-df$value+rnorm(1,0,1.3)

df2<-df%>% 
  gather(run,y, 3:4)
arrange(id)


p <- ggplot(data = df2, aes(x = prelievo, y = y, group = id, col=run))
p+geom_point() + facet_grid(. ~ id)


mod1 = lmer(y ~ (1|id/prelievo), data=dati)
V<-tidy(mod1)
V<-V %>% 
  dplyr::select(estimate) %>% 
  mutate(CV=100*(estimate/estimate[1]))
CVi<-V$CV[4]
CVg<-V$CV[3]
CVa<-V$CV[2]
Ioi<-((CVi^2+CVa^2)^0.5)/CVg
RCV<-(1.96*2^0.5)*(CVi^2+CVa^2)^0.5


write.table(df2, file="dati.txt")

mod = lmer(y ~ (1|id/prelievo), data=dati)
V<-tidy(mod)
V<-V %>%
  dplyr::select(estimate) %>%
  mutate('CV%'=100*(estimate/estimate[1]))%>%
  slice(2:4) %>% 
  rename('SD'=estimate) %>% 
  mutate("Variance Component"=c("Analytic Variability", "Inter-Individual Variability", "Intra-Individual Variability")) %>%
  dplyr::select("Variance Component",SD,'CV%')
  




CVi<-V$'CV%'[4]
CVg<-V$'CV%'[3]
CVa<-V$'CV%'[2]
Ioi<-((CVi^2+CVa^2)^0.5)/CVg
RCV<-(1.96*2^0.5)*(CVi^2+CVa^2)^0.5

x<-tibble("Index of individuality"=Ioi,"Reference Change Value"=RCV)
