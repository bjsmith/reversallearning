rm(list=ls())
library(ggplot2)
library(plotly)
source("visualization/geom_hdi.R")
set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10), 3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_hdi(size=0.5)

set.seed(1492)
dat<-data.frame("myxe"=c(rnorm(1000,0,1),rnorm(1000,1,1)),
                 "people"=c(rep("Blergs",1000),rep("Spurgles",1000)))

p<-ggplot(dat, aes(x=myxe, group=people, fill=factor(people))) +
  geom_histogram()+
  geom_hdi(aes(color=people),lineend="round",size=2,credible_mass=0.95,alpha=0.8)
plotly::ggplotly(p)  

p<-ggplot(dat, aes(x=x, group=people, fill=factor(people))) +
  geom_histogram()+
  geom_hdi(color="black",lineend="round",size=2,credible_mass=0.99)+
  geom_hdi(color="white",size=1,credible_mass=0.95)
plotly::ggplotly(p)
