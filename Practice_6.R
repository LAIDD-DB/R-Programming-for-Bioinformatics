anscombe
apply(anscombe,2, mean)
apply(anscombe,2, var)
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)


plot( iris[,c(1:2)] )
plot( x=iris$Sepal.Length, y=iris$Sepal.Width )
plot( iris[,c(1:2)], type="p", main="IRIS")
plot( iris[,c(1:2)], type="l", main="IRIS")
plot( iris[,1], type="l", main="IRIS")
plot( iris[,1], type="b", main="IRIS")

matplot(iris[,1:4])
matplot(iris[,1:4], lty=1, type="l", col=c("black","blue","green","purple"))
legend("topleft", colnames(iris)[1:4], col=c("black","blue","green","purple")
       , lty=1 )

legend(0,8, colnames(iris)[1:4], col=c("black","blue","green","purple")
       , lty=1 )

range(iris$Sepal.Length)

hist(iris$Sepal.Length, col="skyblue",breaks=15)


boxplot(iris[,1:4])
boxplot(Sepal.Length ~ Species  , data=iris)
boxplot(Sepal.Length ~ Species  , data=iris, col="red")
boxplot(Sepal.Length ~ Species
        , data=iris, col=c("red","purple","yellow") )



tb=table(iris$Species)
barplot(tb)

tb=table(iris[ iris$Sepal.Length >= 5.5, "Species" ])
barplot(tb)
pie(tb)

a= c(1,5,4,10)
names(a)=c("A","B","C","D")

library(pheatmap)
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Sample", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")

install.packages("RColorBrewer")
library(RColorBrewer)

pheatmap(test, col=colorRampPalette( brewer.pal(n=7,name="YlOrRd"))(100))

