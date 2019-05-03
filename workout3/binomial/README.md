# Workout 03

binomial is a package that has a few functions relating to the binomial distribution.

*bin_choose() does "n choose k"
*bin_variable() creates an object of class binvar which holds the amount of trials and the probability of success
*various bin_ summary statistics, which are mean, variance, mode, skewness, and kurtosis
*summary() works on binvar objects to list the summary statistics all at once

*bin_probability() calculates the specific probability of a certain number of successes in a number of trials with a probability of success
*bin_distribution() creates an object with class bindis which holds the binomial distribution for the given trials and probability
*bin_cumulative() creates an object with class bincum which holds the binomial distribution, and the cumulative probability
*both bin_distribution and bin_cumulative can be plotted with plot()


## Installation
***

install this package with "devtools", and then do:
devtools::install_github("stat133-sp19/hw-stat133-MNevins168/tree/master/workout3/binomial")
or 
devtools::install_github("stat133-sp19/hw-stat133-MNevins168/tree/master/workout3/binomial", build_vignettes = TRUE)
depending on if vignettes are desired

***

## Usage
```{r}
library(binomial)

#5 choose 2
bin_choose(5,2)

#creating a generic binomial variable
bv <- bin_variable(5,0.5)
bv
summary(bv)

#finding a specific probability
bin_probability(2,5,0.5)

#getting the data frames
bd <- bin_distribution(5,0.5)
bc <- bin_cumulative(5,0.5)
#plots
plot(bd)
plot(bc)
```
