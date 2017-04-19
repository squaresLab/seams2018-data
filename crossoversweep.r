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

aggdata$type <- ifelse(aggdata$killRatio == 1, "PRISM", "GP")

x <- aggdata$type
x <- factor(x)
aggdata$type <- x
aggdata$type <- factor(x,levels = c("PRISM","GP"))

# setup colors
mycolours <- c("PRISM" = "red", "GP" = "black")

p <- ggplot(data=aggdata, aes(x=aggdata$runtime/1000,y=aggdata$profit,color=aggdata$type),size=2)
p <- p + theme_bw()+ theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.4,"in"))
p + geom_point(size=4) + ylab("Profit") + xlab("Runtime (seconds)") +scale_color_manual("Planner", values = mycolours) + geom_hline(yintercept=2993.499,color="black")  + coord_cartesian(ylim=c(2750, 3000)) + symbols(x=220.525,y=2993.499999971642,circles=.1) + scale_color_grey("Planner")


p <- ggplot(data=datacat, aes(x=crossover,y=profit))
p + geom_boxplot() + theme_bw() + ylab("Profit minus Penalty")

