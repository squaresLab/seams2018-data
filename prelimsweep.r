library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("GGally", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")


# read in the data
data <- read.csv("~/research/AdaptiveSystemsGeneticProgrammingPlanner/params.csv", quote="\"", comment.char="",sep=",")

# keep only the .9 crossover chance, thats what it really is anyway
dataSub <- subset(data,data$crossoverChance==0.9)

attach(dataSub)

# take the average
aggdata <- aggregate(dataSub,by=list(generations,popSize,crossoverChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=median,na.rm=TRUE)

# now sort to get the best settings
sorted <- aggdata[order(aggdata$runtime),]
sorted <- sorted[order(-sorted$profit),]

# now format it nicely
#data <- rename(data,c("V1"="Generation","V2"="Initialization.Time.ms","V3"="Evaluation.Time.ms","V4"="Average.Individual.Size.Generation","V5"="Average.Individual.Size.Run","V6"="Best.Individual.Size.Generation","V7"="Best.Individual.Size.Run","V8"="Average.Fitness.Generation","V9"="Best.Fitness.Generation","V10"="Best.Fitness.Run"))
datacat <- data
datacat$generations <- as.factor(datacat$generations)

datacat$popSize <- as.factor(datacat$popSize)

datacat$crossoverChance <- as.factor(datacat$crossoverChance)


# append point for prism
aggdata <- rbind(aggdata,c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,281500,2993.499999971642))

aggdata$type <- ifelse(aggdata$killRatio == 1, "PRISM", "SASS")

# setup colors
mycolours <- c("PRISM" = "red", "SASS" = "black")

p <- ggplot(data=aggdata, aes(x=aggdata$runtime/1000,y=aggdata$profit))
p + geom_point(aes(color=aggdata$type)) + theme_bw() + ylab("Profit") + xlab("Runtime (secconds)") +scale_color_manual("Type", values = mycolours) + ggtitle("Parameter Sweep Profit vs Runtime")

p <- ggplot(data=datacat, aes(x=generations,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")

p <- ggplot(data=datacat, aes(x=popSize,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")

p <- ggplot(data=datacat, aes(x=crossoverChance,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")


p <- ggplot(data=data, aes(y=runtime/1000,x=profit))
p + geom_point() + theme_bw() + ylab("runtime")

# lets try out some graphs
# fitness / generation in human readable way
profit = 1/(1-data$Best.Fitness.Run)
p <- ggplot(data=data, aes(x=Generation,y=profit))
p + geom_line() + theme_bw() + ylab("Profit minus Penalty")

# eval time / generation
p <- ggplot(data=data, aes(x=Generation,y=cumsum(data$Evaluation.Time.ms)/1000))
p + geom_line() + theme_bw() + ylab("Cumulative Evaluation Time (seconds)")

