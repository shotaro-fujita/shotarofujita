install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
head(iris,n=3)
p_box<-ggplot(data=iris,
              mapping=aes(x=Species,y=Sepal.Width))+
  geom_boxplot()+
  labs(title="” ‚Ð‚°}")
p_box

