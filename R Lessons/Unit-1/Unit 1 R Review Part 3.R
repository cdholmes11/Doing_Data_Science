
#draw a sample from a standard normal distribution
#run many times varying sample size and look at histogram and mean
sample1 = rnorm(1000,0,1)
hist(sample1)
mean(sample1)
sd(sample1)

#Another way to code the same thing
population = rnorm(10000000,0,1) #note the number of draws here
hist(population)
sample1 = sample(population,100) #sample of size 100
hist(sample1)
mean(sample1) #xbar
sd(sample1) #s

#Define these outside the function so that they exist outside the function(global)

xBarVec = c() #Global vector to hold the sample means
population = rnorm(10000000,0,1) #Simulating the population


# Function: xbarGenerator
# Arguments: samplesize; the size of the sample that each sample mean is based on.
#            number_of_samples: the number of samples and thus sample means we will

xbarGenerator = function(sampleSize = 30, number_of_samples = 100)
{
  for(i in 1:number_of_samples)
  {
    theSample = sample(population,sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}  

xbars = xbarGenerator(30,1000)
length(xbars)
hist(xbars)

xBarVec2 = c()

xbarGenerator2 = function(sampleSize = 30, number_of_samples = 100, mean = 0, sd = 1)
{
  population = rnorm(10000000,mean, sd)
  for(i in 1:number_of_samples)
  {
    theSample = sample(population,sampleSize)
    xbar = mean(theSample)
    xBarVec2 = c(xBarVec2, xbar)
  }
  return(xBarVec2)
  
}
 
xbars2 = xbarGenerator2(50, 10000, 60, 10)
hist(xbars2)
mean(xbars2)
sd(xbars2)
  
