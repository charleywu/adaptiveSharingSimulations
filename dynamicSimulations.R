#Agent-based simulations or psuedo-reciprocity through sharing information
#Imen Bouhlel, Charley Wu, and Robert Goldstone

#load packages
rm(list=ls())
packages <- c('data.table', 'plyr')
lapply(packages, require, character.only=TRUE)


#replication ID for cluster computing
v <- as.integer(commandArgs(TRUE)[1])

#############################################################################################################################################################################################
#Simulation variables
#############################################################################################################################################################################################

agentVec <- c(10) #number of agents and dimensions were fixed for the dynamic simulations
Dimensions<- c(7) 
changeProbabilityVec <- c(0, 0.5, 1) #individual change probability
localInfoRadiusVec <- c(0,1,2) #local information radius
sharingConditions <- c("All", "None", "Free-rider", "Free-giver") #group condition
numValues <- 10 # number of different values that a dimension could take
changeRate <- c(0.25, 0.5, 0.75) #rate of environmental change
decayRate <- c(0.9, 0.8, 0.7, 0.6, 0.5) #temporal discount rate 
turns<-100 #time steps
reps<-10000 # number of repetitions of each sharing condition

#Combination of parameter values
ops <- expand.grid(agentVec, # numAgents
                   changeProbabilityVec, # changeProbability
                   Dimensions, # numDimensions
                   localInfoRadiusVec,# freeLocalInfoRadius
                   changeRate, #likelihood of environmental change
                   decayRate, #decay rate of memory of previous rewards
                   sharingConditions) #Sharing conditions
colnames(ops)<- c('numAgents', 'changeProbability', 'numDimensions', 'freeLocalInfoRadius', "changeRate", "decayRate", "sharingCondition" )

#############################################################################################################################################################################################
#Basic functions for simulations 
#############################################################################################################################################################################################

#calculate score using inverse city-block distance between two vectors
score <-function(x,best){
  difference <- abs(x - best)
  return( (1+1)/(sum(difference) + 1)) #score is inverse distance with laplacian smoothing
}

# mutate a guess by adding or subtracting one from each value, or leaving as it is
mutate <-function(guesses, changeProbability = 0.5){
  changeVec <- rbinom(nrow(guesses), 1, changeProbability) #1 if change, 0 if no change. Drawn from a binomial distribution to see if mutation occurs or not for each agent
  #draw from a binomial distribution with possible values c(-1,0,1) to determine amount of change
  changeAmount <- matrix(rbinom(length(guesses),2,0.5) - 1, nrow = nrow(guesses), ncol= ncol(guesses))
  change <- changeAmount*changeVec #multiple changeAmount by changeVec. This way, values of 0 in change vec result in no change
  guesses <- guesses + change
  return (guesses)
}

#apply rewards to a matrix of agent guesses, where each row is a different agent's guess and ideal is the global maximum
acquireRewards <- function(agents, ideal){
  rewards <- sapply(1:nrow(agents), FUN=function(x) score(agents[x,], ideal)) #apply reward function
  #check for duplicate solutions. Returns a vector corresponding to the total number of agents with the same guess
  duplicates <- sapply(1:nrow(agents), FUN=function(i) sum(apply(agents, 1, function(j) identical(j, agents[i,]))))
  return(rewards/duplicates) #split rewards according to the number of agents with the identical guess
}

#update observations
updateObservations <-function(guesses, rewards, observations, t){
  for (i in 1:nrow(guesses)){ #loop through agents
    observations[[i]] <- rbind(observations[[i]], c(guesses[i,], rewards[i], t)) #add c(guess, score) to the observations of the agent
  }
  return(observations)
}

#local visabilities
localVisibility <- function(agents, freeLocalInfoRadius){
  distances <- as.matrix(dist(agents, method='maximum', upper = T, diag=T)) #calculate all pairwise Chebyshev distances between agents
  visible <- distances <= freeLocalInfoRadius #convert to boolean based on whether the distance is less than or equal to the free local info radius
  return(visible)
}

#acquire free information, and add to observations
freeInfo <- function(visibilities, guesses, rewards, observations, t){
  for (i in 1:nrow(guesses)){ #loop through agents i
    for (j in 1:nrow(guesses)){ #loop through others j
      if (i!=j & visibilities[i,j]){ #not same agent and j is visible to i
        observations[[i]] <- rbind(observations[[i]], c(guesses[j,], rewards[j], t)) #add information from j to the observation's of i
      }}}
  return(observations)
}

#Share information, where shareVec is a binary vector indicating whether not the agent shares information
shareInformation <- function(shareVec, agents, scores, observations, t){
  for (i in 1:nrow(agents)){ #for agent i
    for (j in 1:nrow(agents)){ #for agent j
      if (i!=j & shareVec[i]){ # if agent i ≠ j and agent i is a sharer
        observations[[j]] <- rbind(observations[[j]], c(agents[i,], scores[i], t)) #add information from i to j
      }
    }
  }
  return(observations)
}

#finds index of max, resolving ties randomly
which.max.random <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  which(rank(x, ties.method = "random", na.last = FALSE) == length(x))
}
#standard error
se<-function(x){return(sd(x)/sqrt(length(x)))}


#change the idealValues at each time step
changeIdealValues<- function(idealValues, changeRate){
  if (rbinom(1,1,changeRate)==1){ #if successful, change global optimum
    changeAmount <- rbinom(length(idealValues),2,0.5) - 1
    idealValues <- idealValues + changeAmount
    return (idealValues)
  }else{ #otherwise, stay the same
    return (idealValues)
  }
}

#############################################################################################################################################################################################
#Simulation function
#############################################################################################################################################################################################
#Function to run a single replication using a single set of environmental parameters
runCondition<-function(numAgents, numDimensions, changeProbability, freeLocalInfoRadius, shareVec, sharingCondition, changeRate, decayRate, turns=100){ 
  #1. Intialize main data structures
  #array of agent guesses. Each row of matrix is an agent, each column is a dimension
  agents<-matrix(sample(1:numValues, numAgents*numDimensions,replace=TRUE),nrow=numAgents,ncol=numDimensions) #TODO: Store history of guesses to track evolution of search over time
  #observations: individual observations + free local visibilities + information shared by other agents
  observations <- lapply(1:numAgents, matrix, data= NA, ncol=numDimensions + 2, nrow=0) #each matrix is the total data available to each agent (guess for each dimension, plus the corresponding score, plus the round)
  #scores of each agent
  scoreHistory<-data.frame(score=numeric(), round=numeric(), agent=character(), sharer = numeric())  #scores of each agent. Simplified version of data
  
  #2. Initialize environment and assign rewards based on initial guesses
  idealValues<-sample(1:numValues,numDimensions,replace=TRUE) #generate global maximum
  scores <- acquireRewards(agents, idealValues) #assign rewards based on intial starting values
  observations <- updateObservations(agents, scores, observations, 1)#update observations with initial guesses
  #Add to scoreHistory
  dataEntry <- data.frame(score = scores, round=rep(1, numAgents), agent=seq(1:numAgents), sharer=shareVec)
  scoreHistory <- rbind(scoreHistory, dataEntry)
  
  #3. Start looping through iterations
  for (t in 2:turns){ #for each turn
    #acquire free local visibilities
    visibilities <- localVisibility(agents, freeLocalInfoRadius) #compute visibilities
    observations <- freeInfo(visibilities = visibilities, agents, scores, observations, t) #add free local information
    #Individual search            
    #Apply (reverse?) temporal discounting to observations  
    bestObservations <- t(sapply(1:numAgents, FUN=function(i) {
      decayed <- observations[[i]]
      decayed[,numDimensions+1] <- decayRate ** (t - observations[[i]][,numDimensions+2]) *  observations[[i]][,numDimensions+1]  #reward <-  decayRate^(timeDifferences) * reward
      best <- decayed[which.max.random(decayed[,numDimensions+1]), ,drop=FALSE]
    }))
    #Mutate near best observed reward
    agents <- mutate(bestObservations[,1:numDimensions], changeProbability = changeProbability) 
    #change the idealValues
    idealValues <- changeIdealValues(idealValues, changeRate)
    scores <- acquireRewards(agents, idealValues) #assign rewards based on intial starting values
    observations <- updateObservations(agents, scores, observations, t)#update observations with individual mutations
    #Share information
    observations <- shareInformation(shareVec, agents, scores, observations, t) #add sharing information to observations 
    
    #add to scoreHistory
    dataEntry <- data.frame(score = scores, round=rep(t, numAgents), agent=seq(1:numAgents), sharer=shareVec)
    scoreHistory <- rbind(scoreHistory, dataEntry)
  }
  #return data
  return(scoreHistory)
}

#############################################################################################################################################################################################
#Simulation Loop
#############################################################################################################################################################################################

#Assign condition based on cluster id
cond  <- ops[v,] #condition
#simulation parameters
numAgents <- cond$numAgent 
changeProbability <- cond$changeProbability
numDimensions <- cond$numDimensions
freeLocalInfoRadius <- cond$freeLocalInfoRadius
sharingCondition <- cond$sharingCondition
changeRate <- cond$changeRate
decayRate <- cond$decayRate
#create shareVec based on sharing condition
if (sharingCondition=="All"){
  shareVec <- rep(1,numAgents)
}else if (sharingCondition=="None"){
  shareVec <- rep(0, numAgents)
} else if (sharingCondition=="Free-rider"){
  shareVec <- rep(1, numAgents)
  shareVec[1]<- 0
}else if (sharingCondition == "Free-giver"){
  shareVec <- rep(0, numAgents)
  shareVec[1]<- 1
}


#initialize condition dataframe (condData)
condData <- runCondition(numAgents, numDimensions, changeProbability, freeLocalInfoRadius, shareVec, sharingCondition, changeRate, decayRate, turns)
#sum the scores over all replication
for (rep in 2:reps){
  replication <- runCondition(numAgents, numDimensions, changeProbability, freeLocalInfoRadius, shareVec, sharingCondition, changeRate, decayRate, turns)
  condData$score <- condData$score + replication$score
}

#divide score by number of replications
condData$score <- condData$score/reps

#add environment parameters to data
condData$numAgents <- numAgents
condData$numDimensions <- numDimensions
condData$changeProbability <- changeProbability
condData$freeLocalInfoRadius <-freeLocalInfoRadius
condData$sharingCondition <- sharingCondition
condData$changeRate <- changeRate
condData$decayRate <- decayRate


#add column "number of sharers"
condData$nb_sharers <- ifelse (condData$sharingCondition=="None", 0 ,ifelse (condData$sharingCondition=="Free-giver", 1,ifelse (condData$sharingCondition=="All", condData$numAgents,(condData$numAgents - 1))))
#add column "environment_type" (static or dynamic)
condData$environmenType="Dynamic"


#save data
save(condData, file=paste0("simulationData/dynamic/",v,".Rdata"))
