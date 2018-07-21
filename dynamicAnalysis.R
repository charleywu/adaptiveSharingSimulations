#Script to analyize dynamic simulation data
#Charley Wu and Imen Bouhlel
rm(list=ls())

#load packages
packages <- c('zoo', 'RColorBrewer', 'scales', 'data.table', 'plyr', 'ggplot2')
lapply(packages, require, character.only=TRUE)

###################################
#Compile data
###################################


#Simplified data, mean score averaged over round; TAKES LESS TIME
# Data <- data.frame( score = numeric(), round = numeric(), agent = numeric(), sharer = numeric(), numAgents = numeric(), numDimensions = numeric(), changeProbability = numeric(), freeLocalInfoRadius = numeric(), sharingCondition = character(), changeRate = numeric(), decayRate = numeric(), nb_sharers = numeric(), environmentType = character())
# for (comb in 1:540){
#   singleData <- get(load(paste0("simulationData/dynamic/",comb,".Rdata")))
#   singleData <- ddply(singleData, ~agent+sharer+numAgents+numDimensions+changeProbability+freeLocalInfoRadius+sharingCondition+changeRate+decayRate,summarise,meanScore=mean(score))
#   Data <- rbind(Data, singleData)
# }
# Data$sharingCondition <- factor(Data$sharingCondition, levels = c("All", "Free-rider", "Free-giver", "None"))
# save(Data, file = "simulationData/dynamic/simplified.Rdata") #save simplified version
simplifiedDF <- get(load("simulationData/dynamic/simplified.Rdata")) #must run above block of code first
simplifiedDF$meanScore <- simplifiedDF$meanScore * 100 #normalize to 100

###################################
#Preparing heatmap analysis data
###################################
#Make sure this matches simulation parameters
agentVec <- c(10) #c(4)
Dimensions<- c(7) #c(14)
changeProbabilityVec <- c(0, 0.5, 1)
localInfoRadiusVec <- c(0,1,2)
sharingConditions <- c("All", "None", "Free-rider", "Free-giver")
numValues <- 10 # number of different values that a dimension could take
changeRate <- c(0.25, 0.5, 0.75)
decayRate <- c(0.9, 0.8, 0.7, 0.6, 0.5)
turns<-100

#Combination of parameter values
heatMapOps <- expand.grid(agentVec, # numAgents
                   changeProbabilityVec, # changeProbability
                   Dimensions, # numDimensions
                   localInfoRadiusVec,# freeLocalInfoRadius
                   changeRate, #likelihood of environmental change
                   decayRate, #decay rate of memory of previous rewards
                   sharingConditions) #Sharing conditions
colnames(heatMapOps)<- c('numAgents', 'changeProbability', 'numDimensions', 'freeLocalInfoRadius', "changeRate", "decayRate", "sharingCondition" )


#heatMapData
heatMapData <- data.frame(numAgents = numeric(), Innovation = numeric(), numDimensions = numeric(), Visibility = numeric(),  EnvironmentChange = numeric(), DiscountRate=numeric(), relativeBenefit_FreeRiding=numeric(), relativeBenefit_FreeGiving=numeric())

#calculte MarginalBenefit_FreeRiding and MarginalBenefit_FreeGiving (for each agent!) for each parameter values combination and add it to heatMapData
for (row in (1:nrow(heatMapOps))){
  cond  <- heatMapOps[row,] #condition
  singleData <- subset (simplifiedDF, numAgents==cond$numAgents & changeProbability==cond$changeProbability &  numDimensions==cond$numDimensions & freeLocalInfoRadius==cond$freeLocalInfoRadius &  changeRate == cond$changeRate & decayRate == cond$decayRate)
  targetAgent <- subset(singleData, agent==1)
  #Compute marginal benefits
  relativeBenefitsFreeRiding <-  (subset(targetAgent,  sharingCondition=="All")$meanScore - subset(targetAgent, sharingCondition=="Free-rider" )$meanScore) #/ subset(targetAgent, sharingCondition=="Free-rider" )$meanScore #note: positive value means sharing is beneficial
  relativeBenefitsFreeGiving <- (subset(targetAgent, sharingCondition=="Free-giver" )$meanScore -  subset(targetAgent,  sharingCondition=="None")$meanScore) #/ subset(targetAgent,  sharingCondition=="None")$meanScore  #note: positive value means sharing is beneficial
  relativeBenefits <-data.frame( numAgents=cond$numAgents, Innovation=cond$changeProbability,  numDimensions=cond$numDimensions, Visibility=cond$freeLocalInfoRadius,   EnvironmentChange =cond$changeRate, DiscountRate = cond$decayRate, relativeBenefit_FreeRiding=relativeBenefitsFreeRiding, relativeBenefit_FreeGiving=relativeBenefitsFreeGiving)
  
  heatMapData <- rbind(heatMapData, relativeBenefits)
}

#compute average across freeriding and freegiving benefits
heatMapData$sharingBenefit <- (heatMapData$relativeBenefit_FreeRiding + heatMapData$relativeBenefit_FreeGiving) /2 

###################################
#Heatmaps
###################################

cols <- rev(brewer.pal(11, 'RdBu'))

p1 <- ggplot(heatMapData, aes(x=EnvironmentChange, y = DiscountRate, fill = sharingBenefit)) +
  geom_tile()+
  scale_fill_distiller(palette = "Spectral", na.value = 'white', name = "Benefit of Sharing")+
  theme_classic() +
  coord_equal() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  #scale_x_continuous(breaks = round(seq(5,15, by = 2),1))+
  xlab("log Memory Window")+
  ylab("Change Rate")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Combined benefits of sharing')
p1
ggsave(filename = "plots/aggregatedBenefits.pdf", plot = p1, height =9, width = 8, units = "in")

p2<- ggplot(heatMapData, aes(x=DiscountRate, y = EnvironmentChange, fill = relativeBenefit_FreeRiding)) +
  geom_tile()+
  #scale_fill_distiller(palette = "Spectral", na.value = 'white', name = "Benefit of Sharing")+
  scale_fill_gradientn(colours = cols, limits = c(-1.9, 1.9),  name = "Benefits\nof Sharing" )+  #  limits = c(-0.035, 0.035),
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "midnightblue", midpoint = 0) +
  theme_classic() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  scale_x_continuous(breaks = round(seq(0.5,1, by = 0.1),1))+
  xlab("Discount Rate")+
  ylab("Environmental Change")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Sharing when others share')
p2
ggsave(filename = "plots/dynamicbenefitOthersShare.pdf", plot = p2, height =5, width = 8, units = "in")

p3<- ggplot(heatMapData, aes(x=DiscountRate, y = EnvironmentChange, fill = relativeBenefit_FreeGiving)) +
  geom_tile()+
  #scale_fill_distiller(palette = "Spectral", na.value = 'white', name = "Benefit of Sharing")+
  scale_fill_gradientn(colours = cols, limits = c(-21, 21),  name = "Benefits\nof Sharing" )+  #  limits = c(-0.035, 0.035),
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "midnightblue", midpoint = 0) +
  theme_classic() +
  facet_grid( Visibility~ Innovation, labeller = label_both)+
  theme(text = element_text(size=14,  family="sans"))+
  scale_x_continuous(breaks = round(seq(0.5,1, by = 0.1),1))+
  xlab("Discount Rate")+
  ylab("Environmental Change")+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  ggtitle('Sharing when others don\'t share')
p3
ggsave(filename = "plots/dynamicbenefitOthersDontShare.pdf", plot = p3, height =5, width = 8, units = "in")


###################################
#Learning Curves
###################################

# #Full data; TAKES A LONG TIME!
# #Takes a long time!
# fullData <- data.frame( score = numeric(), round = numeric(), agent = numeric(), sharer = numeric(), numAgents = numeric(), numDimensions = numeric(), changeProbability = numeric(), freeLocalInfoRadius = numeric(), sharingCondition = character(),  changeRate = numeric(), decayRate=numeric(), nb_sharers = numeric(), environmentType = character())
# for (comb in 1:540){
#   singleData <- get(load(paste0("simulationData/dynamic/",comb,".Rdata")))
#   fullData <- rbind(fullData, singleData)
# }
# fullData$sharingCondition <- factor(fullData$sharingCondition, levels = c("All", "Free-rider", "Free-giver", "None"))
# save(fullData, file = "simulationData/dynamic/fullData.Rdata") #save simplified version
fullData <- get(load("simulationData/dynamic/fullData.Rdata")) #run above block of code first
fullData$score <- fullData$score *100 #normalize to 100

#5 trial  average
fullData$trial5<-round((fullData$round+1)/5)*5 
fullData$trial5<-ifelse(fullData$trial5<5,0,fullData$trial5)
dplot5<-ddply(fullData,~trial5+numAgents+numDimensions+changeProbability+freeLocalInfoRadius+sharingCondition+agent+changeRate+decayRate,summarise,meanScore=mean(score))
summary(dplot5)

panel1 <- subset(dplot5, changeRate==0.25 & decayRate == 0.8 )
panel1$Visibility <- panel1$freeLocalInfoRadius
panel1$Innovation <- panel1$changeProbability
panel1$EnvironmentChange <- panel1$changeRate
panel1$DiscountRate <- panel1$decayRate
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
  facet_grid(Visibility~Innovation , labeller = label_both)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4))+
  theme(legend.position="right", strip.background=element_blank(), legend.key=element_rect(color=NA), text = element_text(size=16,  family="sans"))
p4target

ggsave(filename = "plots/dynamicCurves.pdf", plot = p4target, height =4.5, width =8, units = "in")

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



