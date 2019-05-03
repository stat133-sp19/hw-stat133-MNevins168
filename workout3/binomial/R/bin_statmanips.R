# Check_prob checks if prob is a valid input for other functions.
check_prob <- function(prob = 1) {
  if (!is.double(prob) & !is.integer(prob)) {
    #Making sure prob is a number
    stop("Probability must be a number.")
  }
  if (prob > 1 | prob < 0) {
    #Once it is known to be a number, checks if between zero and one.
    stop("Probability has to be between 0 and 1.")
  }
  #if passes criteria, then must be valid.
  return(TRUE)
}


#check_trials checks if trials is a realistic value.
check_trials <- function(trials = 1) {
  if (!is.integer(trials) & !is.double(trials)) {
    #first checks if trials is a number
    stop("Trials must be a number.")
  }
  if (trials < 1 | trials != floor(trials)) {
    #then checks if trials is an integer or a double with equivalent value.
    stop("Invalid trials value.")
  }
  #if passes criteria, then must be valid.
  return(TRUE)
}

#check_success checks if the success are realistic when factoring in the number of trials.
check_success <- function(success = 1, trials = 1) {
  if (!is.integer(success) & !is.double(success)) {
    #check if success is a number
    stop("Success must be an number")
  }
  if (!is.integer(trials) & !is.double(trials)) {
    #check if trials is a number
    stop("Trials must be an number")
  }
  for (i in 1:length(success)) {
    if (success[i] < 0 | trials < 1) {
      #iterates through all values in the success vector to make sure they all are positive.
      stop("All inputs must be positive and trials must be non-zero")
    }
    if (success[i] > trials) {
      #makes sure that every value of success is smaller than trials.
      stop("Trials must be larger than success")
    }
    if (success[i] != floor(success[i])) {
      #then checks if success is an integer or a double with equivalent value.
      stop("Invalid success value, not an integer.")
    }
  }
  #if passes criteria, then must be valid.
  return(TRUE)
}



#' @title Mean of Binomial
#' @description Finds the mean of the binomial distribution.
#' @param trials the number of trials in the distribution
#' @param prob the probability of a success
#' @return The mean of the binomial distribution, using aux_mean
#' @export
bin_mean <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}
# bin_mean(10,0.3)

#' @title Variance of Binomial
#' @description Finds the variance of the binomial distribution.
#' @param trials the number of trials in the distribution
#' @param prob the probability of a success
#' @return The variance of the binomial distribution, using aux_variance
#' @export
bin_variance <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials,prob))
}
# bin_variance(10,0.3)

#' @title Mode of Binomial
#' @description Finds the mode of the binomial distribution.
#' @param trials the number of trials in the distribution
#' @param prob the probability of a success
#' @return The mode of the binomial distribution, using aux_mode
#' @export
bin_mode <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}
# bin_mode(10,0.3)
# bin_mode(3,0.5)

#' @title Skewness of Binomial
#' @description Finds the skewness of the binomial distribution.
#' @param trials the number of trials in the distribution
#' @param prob the probability of a success
#' @return The skewness of the binomial distribution, using aux_skewness
#' @export
bin_skewness <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}
# bin_skewness(10,0.3)

#' @title Kurtosis of Binomial
#' @description Finds the kurtosis of the binomial distribution.
#' @param trials the number of trials in the distribution
#' @param prob the probability of a success
#' @return The kurtosis of the binomial distribution, using aux_kurtosis
#' @export
bin_kurtosis <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}
# bin_kurtosis(10,0.3)


#' @export
# summary <- function(binomialvariable) {
#   UseMethod("summary",binomialvariable)
# }
summary.binvar <- function(binomialvariable) {
  trials <- binomialvariable$trials
  prob <- binomialvariable$prob
  mean <- aux_mean(trials, prob)
  variance <- aux_variance(trials, prob)
  mode <- aux_mode(trials, prob)
  skewness <- aux_skewness(trials, prob)
  kurtosis <- aux_kurtosis(trials, prob)
  binsum <- structure(list(trials = trials, prob = prob, mean = mean, variance = variance, mode = mode, skewness = skewness, kurtosis = kurtosis),class = "summary.binvar")
  return(binsum)
}

#' @export
# print <- function(binsum) {
#   UseMethod("print", binsum)
# }
print.summary.binvar <- function(binsum) {
  words <- cat('"','Summary Binomial','"','\n\n','Parameters','\n','- number of trials: ',binsum$trials,'\n','- prob of success : ',binsum$prob,'\n\n','Measures','\n','- mean        : ', binsum$variance,'\n','- variance    : ', binsum$mode,'\n','- mode        : ', binsum$mode,'\n','- skewness    : ', binsum$skewness,'\n','- kurtosis    : ', binsum$kurtosis,sep = "")
  return(words)
}











aux_mean <- function(trials = 1, prob = 1) {
  #basic mean function.
  return(trials * prob)
}
# aux_mean(10,0.3)

aux_variance <- function(trials = 1, prob = 1) {
  #basic variance function
  return(trials*prob*(1-prob))
}
# aux_variance(10,0.3)

aux_mode <- function(trials = 1, prob = 1) {
  #basic mode function that discerns if there are one or two modes
  mode = (trials*prob + prob)
  if (mode == floor(mode)) {
    return (c(mode,mode-1))
  } else {
    return(floor(mode))
  }
}
# aux_mode(10,0.3)
# aux_mode(3,0.5)

aux_skewness <- function(trials = 1, prob = 1) {
  #basic skewness function
  return((1-2*prob)/(sqrt(trials*prob*(1-prob))))
}
# aux_skewness(10,0.3)

aux_kurtosis <- function(trials = 1, prob = 1) {
  #basic kurtosis function
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}
# aux_kurtosis(10,0.3)

#' @title Binomial Choose Function
#' @description Takes n and k and calculates n choose k, given that n is always larger than k
#' @param n the number of objects to choose from
#' @param k the number of objects to choose
#' @return n choose k
#' @export
#' @examples
#' bin_choose(5,2)
#' bin_choose(5,0)
#' bin_choose(5,1:3)
bin_choose <- function(n = 1, k = 1) {
  for (i in 1:length(k)) {
    if (k[i] > n) {
      stop("k cannot be greater than n")
    }
  }
  return(factorial(n)/((factorial(n-k))*factorial(k)))
}

#' @title Binomial Probability Function
#' @description Takes success, trials, and prob, and checks if they are valid, then uses bin_choose and the Binomial Distribution
#' formula to solve for the probability
#' @param success the number of successes out of all the trials
#' @param trials the number of trials for the Binomial Distribution
#' @param prob the probability for each independent Bernoulli trial
#' @return The robability of having a certain amount of successes in a certain amount of trials with a certain probability
#' @export
#' @examples 
#' bin_probability(2,5,0.5)
bin_probability <- function(success = 1, trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(n = trials, k = success) * (prob^success) * (1-prob)^(trials - success))
}

#' @title Binomial Distribution Function
#' @description For a given number of trials and probability of success, calculates the given distribution of successes.
#' @param trials the number of trials for the Binomial Distribution
#' @param prob the probability for each independent Bernoulli trial
#' @return an object bindis which contains a dataframe named datafr which contains a column of all potential successes values
#' and then their corresponding probabilities.
#' @export
#' @examples 
#' bin_distribution(5,0.5)
bin_distribution <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  probability <- vector(length = length(success))
  for (i in 0:trials) {
    probability[i+1] <- bin_probability(i,trials,prob)
  }
  df <- data.frame(success,probability)
  bindis <- structure(list(datafr = df), class = c("bindis","data.frame"))
  return(bindis)
}

#' @title Binomial Cumulative Function
#' @description For a given number of trials and probability of success, calculates the given distribution of successes and the 
#' cumulative distribution of successes.
#' @param trials the number of trials for the Binomial Distribution
#' @param prob the probability for each independent Bernoulli trial
#' @return an object bincum which contains a dataframe named datafr which contains a column of all potential successes values
#' and then their corresponding probabilities, and a third column with the cumulative probability.
#' @export
#' @examples 
#' bin_cumulative(5,0.5)
bin_cumulative <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  probability <- vector(length = length(success))
  cumulative <- vector(length = length(success))
  for (i in 0:trials) {
    probability[i+1] <- bin_probability(i,trials,prob)
    if (i == 0) {
      cumulative[i+1] <- probability[i+1]
    } else {
      cumulative[i+1] <- cumulative[i] + probability[i+1] 
    }
  }
  df <- data.frame(success,probability,cumulative)
  bincum <- structure(list(datafr = df), class = c("bincum","data.frame"))
  return(bincum)
}

#' @title Binomial Object Creator
#' @description Creates an object which holds the values of trials and prob
#' @param trials the number of trials for the Binomial Distribution
#' @param prob the probability for each independent Bernoulli trial
#' @return an object binvar which holds the trials value in trials and prob value in prob
#' @export
#' @examples 
#' bin_variable(5,0.5)
bin_variable <- function(trials = 1, prob = 1) {
  check_trials(trials)
  check_prob(prob)
  binvar <- structure(list(trials = trials, prob = prob), class = "binvar")
  return(binvar)
}

#' @export
print.binvar <- function(binomialvariable)
{
  words <- cat('"','Binomial Variable','"','\n','\n','Parameters','\n','- number of trials: ',binomialvariable$trials,'\n','- prob of success : ',binomialvariable$prob,sep = "")
  return(words)
}
