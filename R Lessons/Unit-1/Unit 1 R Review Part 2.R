#plot(x,y,col,pch,type,ylab,xlab,main)
plot(Example2$Fire, Example2$Theft, pch = 15, xlab = "Fire", ylab = "Theft", main = "Fires vs. Thefts")
abline(h = 55, col = "red", lwd = 5)

#plotting with mpg
plot(mpg$hwy,mpg$cty,pch = 15) #No Lables .... AHHH!!
plot(mpg$hwy,mpg$cty,pch = 15, main = "City MPG v. Highway MPG", ylab = "City MPG", Xlab = "Highway MPG")


#plotting iris data - Petal Length versus Sepal Length
plot(iris$Petal.Length,iris$Sepal.Length, pch = 15, xlab = "Petal Length", ylab = "Sepal Length", main = "Iris - Petal Length v. Sepal Length")

#plot only virginica
irisVir = iris[iris$Species == "virginica",]
plot(irisVir$Sepal.Length,irisVir$Petal.Length, col = "blue", ylim = c(0,7), xlim = c(4,8))
irisVers = iris[iris$Species == "versicolor",]
points(irisVers$Sepal.Length,irisVers$Petal.Length, col = "Red")
irisSet = iris[iris$Species == "setosa",]
points(irisSet$Sepal.Length,irisSet$Petal.Length, col = "Green")

# ?functionname will show the function details in the help panel

#histograms
hist(mtcars$disp,col = "blue", main = "Histogram of Disp")

#boxplot
boxplot(cyl~gear, data = mtcars, main = "Boxplot: Cylinder v. gear ")

#Dividing the plot space! c(num rows, num columns)
par(mfrow = c(1,2))

hist(mtcars$disp,col = "blue", main = "Histogram of Disp", xlab = "disp")
boxplot(cyl~gear, data = mtcars, main = "Boxplot: Cylinder v. gear ", xlab = "cylinder")


# Try to make a histogram of the Iris Sepal Lengths
dev.off()
hist(iris$Sepal.Length, col = "red", main = "Histogram of Iris Sepal Length", xlab = "Sepal Length")

# Barplot
age = c(22,21,24,19,20,23)
yrs_math_ed = c(4,5,2,5,3,5)
names = c("Mary","Martha","Kim","Kristen","Amy","Sam")
subject = c("English","Math","Sociology","Math","Music","Dance")


df3 = data.frame(Age = age, Years = yrs_math_ed, Name = names, Subject = subject)

barplot(df3$Years, names.arg = df3$Name, ylab = "Math Years",main = "Math Years")


# Transforming data to work with barplot
summary(mpg$class)

mpg$classFact = as.factor(mpg$class)
head(mpg)
summary(mpg$classFact)

barplot(summary(mpg$classFact), main = "Number of each Class", ylab = "Count")
