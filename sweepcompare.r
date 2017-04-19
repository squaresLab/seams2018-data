library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

crossoversweep <- read.csv("~/research/seams2017_data/mutationbaby.csv")
adaptcrossoversweep <- read.csv("~/research/seams2017_data/adaptmutationbaby.csv")

attach(crossoversweep)

# take the average
aggdata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,mutationChance,reproductionChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=median,na.rm=TRUE)
# take the sd
sddata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,mutationChance,reproductionChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=sd,na.rm=TRUE)
# merge the two
aggdata$profitsd <- sddata$profit
aggdata$runtimesd <- sddata$runtime
aggdata$sizesd <- sddata$size
# drop agg group cols
aggdata <- aggdata[-c(1:9)]

# now sort to get the best settings
sorted <- aggdata[order(aggdata$runtime),]
sorted <- sorted[order(-sorted$profit),]

starting <- sorted

crossoversweep <- adaptcrossoversweep

attach(crossoversweep)

# take the average
aggdata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,mutationChance,reproductionChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=median,na.rm=TRUE)
# take the sd
sddata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,mutationChance,reproductionChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=sd,na.rm=TRUE)
# merge the two
aggdata$profitsd <- sddata$profit
aggdata$runtimesd <- sddata$runtime
aggdata$sizesd <- sddata$size
# drop agg group cols
aggdata <- aggdata[-c(1:9)]

# now sort to get the best settings
sorted <- aggdata[order(aggdata$runtime),]
sorted <- sorted[order(-sorted$profit),]

adapting <- sorted

starting$type <- "poor"
adapting$type <- "good"

aggdata <- rbind(starting,adapting)

datacat <- sorted
datacat$crossover <- as.factor(datacat$crossover)

# append point for prism
aggdata <- rbind(aggdata,c(1,1,1,1,1,1,1,1,1,20,281500,2993.499999971642,0,0,0,"PRISM"))

#aggdata$type <- ifelse(aggdata$killRatio == 1, "PRISM", "SASS")

# setup colors
mycolours <- c("PRISM" = "red", "poor" = "black","good" = "green")

aggdata$runtime <- as.numeric(aggdata$runtime)

p <- ggplot(data=aggdata, aes(x=aggdata$runtime/1000,y=aggdata$profit))
p + geom_point(aes(color=aggdata$type)) + theme_bw() + ylab("Profit") + xlab("Runtime (seconds)") +scale_color_manual("Type", values = mycolours) + ggtitle("Parameter Sweep Profit vs Runtime")

p <- ggplot(data=datacat, aes(x=crossover,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")
