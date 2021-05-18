library(ggplot2)
head(iris,n=3)
ggplot(iris,aes(x=Sepal.Width,y=Sepal.Length,color=Species))+geom_point()