
# the possible transitions (new keypads) from keypad = index - 1
transition <- matrix(c(4,6,-1,6,7,-1,7,9,-1,4,8,-1,3,9,0,-1,-1,-1,1,7,0,2,6,-1,1,3,-1,4,2,-1),ncol=3,nrow=10,byrow=TRUE)

# the number of options for each keypad = index - 1
options <- c(2,2,2,2,3,0,3,2,2,2)

# For T = 10, which can be computed exactly

# initialize probs, values, lastvalue, T, tempvales = values
# values = a running total of the sums of the values for a particular state
# probs = the probability for a given state
# lastvalue = the last value from which to transition
probs <- vector(length=1)
probs[1] <- 1
values <- vector(length=1)
values[1] <-  0
lastvalue <- vector(length=1)
lastvalue[1] <-  0
tempvalues <- values
T <- 10

# for each value i from 1 to T, the loop computes "new" vectors for the transition to step i
# then the new vector is added to the end of the temp vectors, and the first index is removed
# at the end of the loop the final output vectors are set to the temp vectors
# thus, at the beginning of the loop both the final output vectors and the temp vectors are set to the state for state i - 1
# Note that this is a Markov chain

for (i in 1:T) {
  numvalues <- length(values)

  for (j in 1:numvalues) {
    
    # set "curr" variables
    currlast <- lastvalue[j]; currvalues <- values[j]; currprobs <- probs[j]; currlength <- options[currlast+1]; curr_num <- length(tempvalues)
    
    #initialize "new" vectors to add to temporary vectors
    newprobs <- vector(length=currlength); newvalues <- vector(length=currlength); newlastvalue <- vector(length=currlength)

    # compute "new" vectors
    for (k in 1:currlength) {
      newprobs[k] <- currprobs/options[currlast+1]
      newvalues[k] <- currvalues + transition[currlast+1,k]
      newlastvalue[k] <- transition[currlast+1,k]
      }

    # add new vectors to temp vectors
    if (j == 1 && curr_num == 1) {
        tempprobs <- newprobs  
        tempvalues <- newvalues
        templastvalue <- newlastvalue
        }
      else {
        tempprobs <- c(tempprobs[2:curr_num],newprobs)  
        tempvalues <- c(tempvalues[2:curr_num],newvalues)
        templastvalue <- c(templastvalue[2:curr_num],newlastvalue)
      }
    }

# set vectors to temp vectors    
probs <- tempprobs
values <- tempvalues
lastvalue <- templastvalue

}

print(paste("T =",as.character(T)))
mod_values <- values %% 10
meanmodvalue <- sum(probs*mod_values)
print(paste("mean mod 10 = ",as.character(meanmodvalue)))
var <- sum(((values - meanmodvalue)^2)*probs)
sd <- sqrt(var)
print(paste("sd mod 10 = ",as.character(sd %% 10)))

temp1 <- probs[which(values %% 7 == 0)]
temp2 <- probs[which(values %% 35 == 0)]
temp3 <- sum(temp2)/sum(temp1)
print(paste("prob of sum is divisible by 5, given divisible by 7",as.character(sum(temp3))))
stop()
T <- 1024
iter <- 2000000
#iter <- 2000
print_inc <- 10000
# with a T = 1024, there will be at least 2^1024 states (over 10^307), too many to compute exactly, as above
# thus, the mean and sd will be estimated with a Monte Carlo simulation
# 2 million iterations were chosen.  

values_mc <- vector(length=iter)
mod_values_mc <- vector(length=iter)

for (i in 1:iter) {
  if (i %% print_inc == 0) {print(as.character(i))}
  currlast <- 0
  currvalues <- 0
  currprob <- 1
  rnd <- runif(T)
  for (j in 1:T) {
    currrnd <- rnd[j]
    currind <- as.integer(options[currlast+1]*currrnd+1)
    newlast <- transition[currlast+1,currind]
    currvalues <- currvalues + newlast
    currlast <- newlast
  }
  
values_mc[i] <- currvalues
mod_values_mc[i] <- currvalues %% 1024

}

print(paste("T =",as.character(T)))
meanvalue_mc <- mean(mod_values_mc)
print(paste("mean mod 1024= ",as.character(meanvalue_mc)))
sd_mc <- sd(mod_values_mc)
print(paste("sd mod 1024= ",as.character(sd_mc)))

temp1 <- subset(values_mc, values_mc %% 29 == 0)
temp2 <- subset(temp1, temp1 %% 23 == 0)
temp3 <- length(temp2)/length(temp1)
print(paste("prob of sum is divisible by 23, given divisible by 29",as.character(sum(temp3))))
