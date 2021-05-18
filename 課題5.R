library(ggplot2)
fish<-read.csv("fish.csv")
head(fish,n=3)
ggplot(data=fish,mapping=aes(x=length,y=..density..))+
  geom_histogram(alpha=0.5,bins=20)+
  geom_density(size=1.5)+
  labs(title="ƒOƒ‰ƒt‚Ìd‚Ë‡‚í‚¹")
