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
e2    <- rnorm(N*t, mean = 0, sd = 1.5)
y2    <- 100 + x * beta2 + e2

data1 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y1, task=rep(c("run1"),length(y1)))
data2 <- data.frame(id=factor(rep(1:N, each=t)), day = x, y = y2, task=rep(c("run2"),length(y2)))
data <- rbind(data1, data2)



p <- ggplot(data = data, aes(x = day, y = y, group = id))
p+geom_point() + facet_grid(. ~ id+task)


mod1 = lmer(y ~ day+ (1|day/id), data=data)

fit<-fitVCA(y~day+(day/id), data, "reml")
