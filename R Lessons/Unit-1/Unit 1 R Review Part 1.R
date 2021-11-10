
# Vectors
age = c(22,21,24,19,20,23)
age
age[2]
yrs_math_ed = c(4,5,2,5,3,5)
yrs_math_ed

# Data frames
df1 = data.frame(Age = age, Years = yrs_math_ed)
df1
df1[4,2] # Data in the 4th row and 2nd column
df1[1,] # All the data in the 1st row
df1$Years # All the data in the second column


a = c("Mary","Martha","Kim","Kristen","Amy","Sam")
b = c("English","Math","Sociology","Math","Music","Dance")

# Make a data frame
df2 = data.frame(Name = a, Major = b)
df2

# Make another data frame
df3 = data.frame(Age = age, Years = yrs_math_ed, Name = a, Major = b)
df3

#Cbind combines columns
df4 = cbind(df1, df2)
df4

# str and class show what type of variable it is
str(df4)
class(df4$Name)
summary(df4)

# Define a new row / student
d = c(19,4, "John", "Math")

# Attempt to add a row
df5 = rbind(df4,d) # Problem here is that Name and Major are factors

# Try and fix the problem
dfCopy = df4
dfCopy$Name = as.character(df4$Name)
dfCopy$Major = as.character(df4$Major)
summary(dfCopy)

# Add the student to the end of the data frame.
df5 = rbind(dfCopy,d)
df5

# Recheck the class
str(df5)
summary(df5)

# Fix the classes
df5$Age = as.numeric(df5$Age)
df5$Years = as.numeric(df5$Years)
summary(df5)
df5$Name = as.factor(df5$Name)
df5$Major = as.factor(df5$Major)
summary(df5)

str(df5)

# Filter the data frame
# All students with mroe than 4 years of Math
df5[df5$Years > 4,]
# All students with more than 4 years of Math and are 21 years of age or older
df5[(df5$Age >= 21 & df5$Years > 4),]
# All students that are majoring in Math
df5[df5$Major == "Math",]


# Data Import from csv

Example1 = read.csv("file path",header = TRUE) # headers are included in data
Example1

Example2 = read.csv(file.choose(), header = TRUE) # opens window to search for your file
Example2

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



