library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# read in the data
start <- read.table("~/research/AdaptiveSystemsGeneticProgrammingPlanner/out.stat", quote="\"", comment.char="")
adapt <- read.table("~/research/AdaptiveSystemsGeneticProgrammingPlanner/adapt.stat", quote="\"", comment.char="")

start$start <- "Poor"
adapt$start <- "Good"

data <- rbind(start,adapt)

# now format it nicely
data <- rename(data,c("V1"="Generation","V2"="Initialization.Time.ms","V3"="Evaluation.Time.ms","V4"="Average.Individual.Size.Generation","V5"="Average.Individual.Size.Run","V6"="Best.Individual.Size.Generation","V7"="Best.Individual.Size.Run","V8"="Average.Fitness.Generation","V9"="Best.Fitness.Generation","V10"="Best.Fitness.Run"))

# lets try out some graphs
# fitness / generation in human readable way
profit = 1/(1-data$Best.Fitness.Run)
p <- ggplot(data=subset(data,Generation<26), aes(x=Generation,y=V12,color=start))
p + geom_line() + theme_bw() + ylab("Profit minus Penalty") + coord_cartesian(ylim=c(2750, 3000))

# just adapt
p <- ggplot(data=subset(adapt,V1<26), aes(x=V1,y=V12,color=start))
p + geom_line() + theme_bw() + ylab("Profit minus Penalty")

# eval time / generation
p <- ggplot(data=data, aes(x=Generation,y=cumsum(data$Evaluation.Time.ms)/1000))
p + geom_line() + theme_bw() + ylab("Cumulative Evaluation Time (seconds)")

