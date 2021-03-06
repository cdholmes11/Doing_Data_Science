

## Control Parameters
population = 10000000 # population size
simulations = 10000
sample_size = 50
df = 2


## Chi-squared distribution
chi_sqr_dist = rchisq(population, df)
chi_sqr_dist

hist(chi_sqr_dist, col = c("#ff6666"), xlab = NULL, main = paste("Chi-Square Distribution: df = ", df))
mean(chi_sqr_dist)
sd(chi_sqr_dist)


## Data Holders
xbar_holder1 = numeric(simulations) # This will hold all the sample means for the first distribution.

## Simulate and Store
for (i in 1:simulations)
{
  sample1 = sample(chi_sqr_dist,sample_size)
  xbar1 = mean(sample1)
  xbar_holder1[i] = xbar1
}


## Display the distribution of sample means (plot a histogram of the sample means)
hist(xbar_holder1, col = c("#ff9966"), main = paste("Distribution of the sample mean: n = ", sample_size), xlab = "Dist Sample Means", xlim = c(-4,4))


## summary statistics of the distribution of the simulated sample means.
mean(xbar_holder1)
sd(xbar_holder1)


# Beach Comber Two-Sided T-Test

## Parameters

ages = c(25,19,37,29,40,28,31)
n = length(ages)
alpha = .05

## Step 1 - Claim: mean age of the distribution of Comber patrons is <> 21
##  Ho: mu = 21
##  Ha: mu <> 21

ho = 21

## Step 2
critval = qt(alpha, n-1, lower.tail = FALSE)
critval

## Step 3
ttest = t.test(ages, mu = ho, alt = "two.sided", conf= 1 - alpha)
tstat = ttest$statistic

## Step 4
pval = ttest$p.value

## Step 5
ttest

## Step 6
## There is enough evidence to suggest the true mean of the distribution of 
## Comber patrons is not equal to 21 (p-value = 0.01622)
















