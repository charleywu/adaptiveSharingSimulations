#Script to analyize static simulation data
#Charley Wu and Imen Bouhlel

rm(list=ls()) #housekeeping

#load packages
packages <- c('zoo', 'RColorBrewer', 'scales', 'data.table', 'plyr', 'ggplot2')
lapply(packages, require, character.only=TRUE)

###################################
#Compile data
###################################

#Simplified data, mean score averaged over round; TAKES LESS TIME
# Data <- data.frame( score = numeric(), round = numeric(), agent = numeric(), sharer = numeric(), numAgents = numeric(), numDimensions = numeric(), changeProbability = numeric(), freeLocalInfoRadius = numeric(), sharingCondition = character(), nb_sharers = numeric(), environmentType = character())
# for (comb in 1:2772){
#   singleData <- get(load(paste0("simulationData/static/",comb,".Rdata")))
#   singleData <- ddply(singleData, ~agent+sharer+numAgents+numDimensions+changeProbability+freeLocalInfoRadius+sharingCondition,summarise,meanScore=mean(score))
#   Data <- rbind(Data, singleData)
# }
# Data$sharingCondition <- factor(Data$sharingCondition, levels = c("All", "Free-rider", "Free-giver", "None"))
# save(Data, file = "simulationData/static/simplified.Rdata") #save simplified version

simplifiedDF <- get(load("simulationData/static/simplified.Rdata")) #load simplified data once the above block of code has been run
simplifiedDF$meanScore <- simplifiedDF$meanScore * 100 #normalize to 100

###################################
#Preparing heatmap analysis data
###################################
#Make sure these match the simulation parameters
agentVec <- seq(4,10)
Dimensions<- seq(5,15)
changeProbabilityVec <- seq(0, 1, 0.5)
localInfoRadiusVec <- seq(0,2)
sharingConditions <- c("All", "None", "Free-rider", "Free-giver")
numValues <- 10 # number of different values that a dimension could take
turns<-100
#Combination of parameter values
heatMapOps <- expand.grid(agentVec, # numAgents
                          changeProbabilityVec, # changeProbability
                          Dimensions, # numDimensions
                          localInfoRadiusVec)# freeLocalInfoRadius
colnames(heatMapOps)<- c('numAgents', 'changeProbability', 'numDimensions', 'freeLocalInfoRadius' )


#heatMapData
heatMapData <- data.frame(numAgents = numeric(), Innovation = numeric(), numDimensions = numeric(), Visibility = numeric(),  relativeBenefit_FreeRiding=numeric(), relativeBenefit_FreeGiving=numeric())

#calculte MarginalBenefit_FreeRiding and MarginalBenefit_FreeGiving (for each agent!) for each parameter values combination and add it to heatMapData
for (row in (1:nrow(heatMapOps))){
  cond  <- heatMapOps[row,] #condition
  singleData <- subset (simplifiedDF, numAgents==cond$numAgents & changeProbability==cond$changeProbability &  numDimensions==cond$numDimensions & freeLocalInfoRadius==cond$freeLocalInfoRadius)
  targetAgent <- subset(singleData, agent==1)
  #Compute marginal benefits
  relativeBenefitsFreeRiding <-  (subset(targetAgent,  sharingCondition=="All")$meanScore - subset(targetAgent, sharingCondition=="Free-rider" )$meanScore) #/ subset(targetAgent, sharingCondition=="Free-rider" )$meanScore #note: positive value means sharing is beneficial
  relativeBenefitsFreeGiving <- (subset(targetAgent, sharingCondition=="Free-giver" )$meanScore -  subset(targetAgent,  sharingCondition=="None")$meanScore) #/ subset(targetAgent,  sharingCondition=="None")$meanScore  #note: positive value means sharing is beneficial
  relativeBenefits <-data.frame( numAgents=cond$numAgents, Innovation=cond$changeProbability,  numDimensions=cond$numDimensions, Visibility=cond$freeLocalInfoRadius, relativeBenefit_FreeRiding=relativeBenefitsFreeRiding, relativeBenefit_FreeGiving=relativeBenefitsFreeGiving)
  
  heatMapData <- rbind(heatMapData, relativeBenefits)
}


#compute average sharing benefit for freeriding and for freegiving
heatMapData$sharingBenefit <- (heatMapData$relativeBenefit_FreeRiding + heatMapData$relativeBenefit_FreeGiving) /2 

###################################
#Heatmaps
###################################

cols <- rev(brewer.pal(11, 'RdBu'))

p1 <- ggplot(heatMapData, aes(x=numDimensions, y = numAgents, fill = sharingBenefit)) +
  geom_tile()+
  scale_fill_distiller(palette = "Spectral", na.value = 'white', name = "Benefit of Sharing")+
  theme_classic() +
  coord_equal() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  scale_y_continuous(breaks = round(seq(5,15, by = 2),1))+
  ylab("Number of Dimensions")+
  xlab("Number of Agents")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Combined benefits of sharing')
p1
ggsave(filename = "plots/aggregatedBenefits.pdf", plot = p1, height =9, width = 8, units = "in")

p2<- ggplot(heatMapData, aes(x=numDimensions, y = numAgents, fill = relativeBenefit_FreeRiding)) +
  geom_tile()+
  #scale_fill_distiller(palette = "Spectral", na.value = 'white', name = "Benefit of Sharing")+
  scale_fill_gradientn(colours = cols,  limits = c(-3.5, 3.5),   name = "Benefits\nof Sharing" )+ 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "midnightblue", midpoint = 0) +
  theme_classic() +
  coord_equal() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  scale_y_continuous(breaks = round(seq(4,10, by = 2),1))+
  scale_x_continuous(breaks = round(seq(5,15, by = 5),1))+
  ylab("Number of Agents")+
  xlab("Number of Dimensions")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Sharing when others share')
p2
ggsave(filename = "plots/benefitOthersShare.pdf", plot = p2, height =6, width = 8, units = "in")

p3<- ggplot(heatMapData, aes(x= numDimensions, y = numAgents, fill = relativeBenefit_FreeGiving)) +
  geom_tile()+
  #scale_fill_distiller(palette = "Spectral", na.value = 'white',name = "Benefit of Sharing")+
  scale_fill_gradientn(colours = cols, limits = c(-12, 12), name = "Benefits\nof Sharing" )+ 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "midnightblue", midpoint = 0) +
  theme_classic() +
  coord_equal() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  scale_y_continuous(breaks = round(seq(4,10, by = 2),1))+
  scale_x_continuous(breaks = round(seq(5,15, by = 5),1))+
  xlab("Number of Dimensions")+
  ylab("Number of Agents")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Sharing when others don\'t share')
p3
ggsave(filename = "plots/benefitOthersDontShare.pdf", plot = p3, height =6, width = 8, units = "in")



###################################
#Learning Curves
###################################

# #Full data; TAKES A LONG TIME!
# #Takes a long time!
# fullData <- data.frame( score = numeric(), round = numeric(), agent = numeric(), sharer = numeric(), numAgents = numeric(), numDimensions = numeric(), changeProbability = numeric(), freeLocalInfoRadius = numeric(), sharingCondition = character(), nb_sharers = numeric(), environmentType = character())
# for (comb in 1:2772){
#   singleData <- get(load(paste0("simulationData/static/",comb,".Rdata")))
#   fullData <- rbind(fullData, singleData)
# }
# fullData$sharingCondition <- factor(fullData$sharingCondition, levels = c("All", "Free-rider", "Free-giver", "None"))
# save(fullData, file = "simulationData/static/fullData.Rdata") #save simplified version

fullData <- get(load("simulationData/static/fullData.Rdata")) #output from the above block of code
fullData$score <- fullData$score * 100 #normalize to 100

#5 trial moving average
fullData$trial5<-round((fullData$round+1)/5)*5
fullData$trial5<-ifelse(fullData$trial5<5,0,fullData$trial5)
dplot5<-ddply(fullData,~trial5+numAgents+numDimensions+changeProbability+freeLocalInfoRadius+sharingCondition+agent,summarise,meanScore=mean(score))


panel1 <- subset(dplot5, numAgents == 10 & numDimensions == 7 & changeProbability==0.5 & freeLocalInfoRadius %in% c(0,1,2))
panel1$Visibility <- panel1$freeLocalInfoRadius
levels(panel1$sharingCondition) <- c("All sharers", "Free-rider", "Free-giver", "No sharers")
p4target<- ggplot(subset(panel1, agent==1), aes(x=trial5, y = meanScore, color = sharingCondition, fill= sharingCondition, shape = sharingCondition)) +
  #geom_smooth(fill=NA, size = 0.7, method = "lm", formula = y ~ poly(x, 20))+
  geom_line(size = 0.7)+
  geom_point(data=subset(panel1, agent==1 & trial5%%10==0), aes(x=trial5, y = meanScore, color = sharingCondition, fill= sharingCondition, shape = sharingCondition), size = 2)+
  theme_classic()+
  scale_color_brewer(palette='Dark2', name ="")+
  scale_fill_brewer(palette= 'Dark2', name = "")+
  scale_shape_discrete(solid = TRUE, name = "")+
  xlab("Trial") +
  ylab("Mean Score") +
  facet_grid(  ~Visibility , labeller = label_both)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
  theme(legend.position="bottom", strip.background=element_blank(), legend.key=element_rect(color=NA), text = element_text(size=16,  family="sans"))
p4target

ggsave(filename = "plots/visibilityTarget.pdf", plot = p4target, height =4, width = 6.5, units = "in")

p4all<- ggplot(panel1, aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition)) +
  geom_smooth(fill=NA, size = 0.7)+
  stat_summary(data=subset(panel1,  round%%10==1), fun.y = mean, geom='point', aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition))+
  theme_classic()+
  scale_color_brewer(palette='Dark2', name ="")+
  scale_fill_brewer(palette= 'Dark2', name = "")+
  scale_shape_discrete(solid = TRUE, name = "")+
  xlab("Trial") +
  ylab("Mean Score") +
  facet_grid(  ~Visibility , labeller = label_both)+
  theme(legend.position="bottom", strip.background=element_blank(), legend.key=element_rect(color=NA), text = element_text(size=16,  family="sans"))
p4all

ggsave(filename = "plots/visibilityAll.pdf", plot = p4all, height =3.5, width = 5.5, units = "in")


panel2 <- subset(fullData, numAgents == 4 & numDimensions == 8 & changeProbability %in% c(0,0.5,1) & freeLocalInfoRadius %in% c(0,1,2))
panel2$Visibility <- panel2$freeLocalInfoRadius
panel2$Innovation <- panel2$changeProbability

p5target<- ggplot(subset(panel2, agent==1), aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition)) +
  geom_smooth(fill=NA, size = 0.7,  method = )+
  geom_point(data=subset(panel2, agent==1 & round%%10==1), aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition))+
  theme_classic()+
  scale_color_brewer(palette='Dark2', name ="")+
  scale_fill_brewer(palette= 'Dark2', name = "")+
  scale_shape_discrete(solid = TRUE, name = "")+
  xlab("Trial") +
  ylab("Mean Score") +
  facet_grid(Innovation ~Visibility , labeller = label_both)+
  theme(legend.position="bottom", strip.background=element_blank(), legend.key=element_rect(color=NA), text = element_text(size=16,  family="sans"))
p5target

ggsave(filename = "plots/innovationTarget.pdf", plot = p5target, height =3.5, width = 5.5, units = "in")


p5all<- ggplot(panel2, aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition)) +
  geom_smooth(fill=NA, size = 0.7,  method = )+
  stat_summary(data=subset(panel2,  round%%10==1), fun.y = mean, geom='point', aes(x=round, y = score, color = sharingCondition, fill= sharingCondition, shape = sharingCondition))+
  theme_classic()+
  scale_color_brewer(palette='Dark2', name ="")+
  scale_fill_brewer(palette= 'Dark2', name = "")+
  scale_shape_discrete(solid = TRUE, name = "")+
  xlab("Trial") +
  ylab("Mean Score") +
  facet_grid(Innovation ~Visibility , labeller = label_both)+
  theme(legend.position="bottom", strip.background=element_blank(), legend.key=element_rect(color=NA), text = element_text(size=16,  family="sans"))
p5all

ggsave(filename = "plots/innovationall.pdf", plot = p5all, height =3.5, width = 5.5, units = "in")


###############################
#Regression
###############################

#regressionData
regressionData <- data.frame(numAgents = numeric(), Innovation = numeric(), numDimensions = numeric(), Visibility = numeric(),  relativeBenefit_FreeRiding=numeric(), relativeBenefit_FreeGiving=numeric(), numberSharers=numeric())

#calculte MarginalBenefit_FreeRiding and MarginalBenefit_FreeGiving (for each agent!) for each parameter values combination and add it to regressionData (two lines)
for (row in (1:nrow(heatMapOps))){
  cond  <- heatMapOps[row,] #condition
  singleData <- subset (Data, numAgents==cond$numAgents & changeProbability==cond$changeProbability &  numDimensions==cond$numDimensions & freeLocalInfoRadius==cond$freeLocalInfoRadius)
  targetAgent <- subset(singleData, agent==1)
  #Compute marginal benefits
  relativeBenefitsFreeRiding <-  (subset(targetAgent,  sharingCondition=="All")$meanScore - subset(targetAgent, sharingCondition=="Free-rider" )$meanScore) #/ subset(targetAgent, sharingCondition=="Free-rider" )$meanScore #note: positive value means sharing is beneficial
  relativeBenefitsFreeGiving <- (subset(targetAgent, sharingCondition=="Free-giver" )$meanScore -  subset(targetAgent,  sharingCondition=="None")$meanScore) #/ subset(targetAgent,  sharingCondition=="None")$meanScore  #note: positive value means sharing is beneficial
  dataEntry_relativeBenefitsFreeRiding <- data.frame( numAgents=cond$numAgents, Innovation=cond$changeProbability,  numDimensions=cond$numDimensions, Visibility=cond$freeLocalInfoRadius, relativeBenefit_FreeRiding=relativeBenefitsFreeRiding, relativeBenefit_FreeGiving=0, numberSharers=cond$numAgents-1)
  dataEntry_relativeBenefitsFreeGiving <- data.frame( numAgents=cond$numAgents, Innovation=cond$changeProbability,  numDimensions=cond$numDimensions, Visibility=cond$freeLocalInfoRadius, relativeBenefit_FreeRiding=0, relativeBenefit_FreeGiving=relativeBenefitsFreeGiving, numberSharers=1)
  dataEntry <- rbind(dataEntry_relativeBenefitsFreeRiding, dataEntry_relativeBenefitsFreeGiving)
  regressionData <- rbind(regressionData, dataEntry)
}

summary(regressionData)
regressionData$sharingBenefit <- (regressionData$relativeBenefit_FreeRiding + regressionData$relativeBenefit_FreeGiving)


attach(heatMapData) # sharingBenefit= (relativeBenefit_FreeRiding + relativeBenefit_FreeGiving) /2 
#work with sharingBenefit, relativeBenefit_FreeGiving or relativeBenefit_FreeRiding as a dependent variable
model1 <- lm(formula = sharingBenefit ~ log(numAgents) + numDimensions + 
               Visibility + Innovation - 1, subset = (Innovation > 0))
summary(model1)


attach(heatMapData) # sharingBenefit= (relativeBenefit_FreeRiding + relativeBenefit_FreeGiving) /2 
#explains well sharingBenefit, relativeBenefit_FreeGiving as a dependent variable, but very bad relativeBenefit_FreeRiding
model2 <- lm(formula = sharingBenefit ~ numDimensions + 
               Visibility + Innovation + 
               Visibility:numAgents + Visibility:numDimensions - 1, subset = (Innovation > 0))
summary(model2)

AIC(model1, model2)
BIC(model1, model2)

attach(regressionData) # sharingBenefit= relativeBenefit_FreeRiding + relativeBenefit_FreeGiving ; where either relativeBenefit_FreeRiding or relativeBenefit_FreeGiving equals 0 
#works only with sharingBenefit as a dependent variable, allows having numberSharers as an explanatory variable
model3 <- lm(formula = sharingBenefit ~ log(numAgents) + numDimensions + 
               Visibility + Innovation + numberSharers - 1, subset = (Innovation >0))
summary(model3)

attach(regressionData) # sharingBenefit= relativeBenefit_FreeRiding + relativeBenefit_FreeGiving ; where either relativeBenefit_FreeRiding or relativeBenefit_FreeGiving equals 0 
#works only with sharingBenefit as a dependent variable, allows having numberSharers as an explanatory variable
model4 <- lm(formula = sharingBenefit ~ numDimensions + 
                Visibility + Innovation + 
               Visibility:numAgents + Visibility:numDimensions + numberSharers - 1, subset = (Innovation > 0))
summary(model4)  
  
AIC(model3, model4)
BIC(model3, model4)  
