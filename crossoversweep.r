library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

crossoversweep <- read.csv("~/research/seams2017_data/crossoversweep.csv")

attach(crossoversweep)

# take the average
aggdata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=median,na.rm=TRUE)
# take the sd
sddata <- aggregate(crossoversweep,by=list(generations,popSize,crossoverChance,killRatio,invalidActionPenalty,verbosenessPenalty,minAcceptedImprovement), FUN=sd,na.rm=TRUE)
# merge the two
aggdata$profitsd <- sddata$profit
aggdata$runtimesd <- sddata$runtime
aggdata$sizesd <- sddata$size
# drop agg group cols
aggdata <- aggdata[-c(1:7)]

# now sort to get the best settings
sorted <- aggdata[order(aggdata$runtime),]
sorted <- sorted[order(-sorted$profit),]

datacat <- sorted
datacat$crossover <- as.factor(datacat$crossover)

# append point for prism
aggdata <- rbind(aggdata,c(1,1,1,1,1,1,1,20,220525,2993.499999971642))

aggdata$type <- ifelse(aggdata$killRatio == 1, "PRISM", "SASS")

# setup colors
mycolours <- c("PRISM" = "red", "SASS" = "black")

p <- ggplot(data=aggdata, aes(x=aggdata$runtime/1000,y=aggdata$profit))
p + geom_point(aes(color=aggdata$type)) + theme_bw() + ylab("Profit") + xlab("Runtime (seconds)") +scale_color_manual("Planner", values = mycolours) + ggtitle("Parameter Sweep Profit vs Runtime") + geom_hline(yintercept=2993.499,color="red")  + coord_cartesian(ylim=c(2750, 3000)) 


p <- ggplot(data=datacat, aes(x=crossover,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")

