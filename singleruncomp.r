library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# read in the data
start <- read.csv("~/research/seams2017_data/poorstart4serv.csv", quote="\"", comment.char="")
adapt <- read.csv("~/research/seams2017_data/adapt4serv.csv", quote="\"", comment.char="")
adapt <- read.csv("~/research/seams2017_data/fastadapt4serv.csv", quote="\"", comment.char="")

start <- read.csv("~/research/seams2017_data/poorstart10xreq.csv", quote="\"", comment.char="")
adapt <- read.csv("~/research/seams2017_data/adapt10xreq.csv", quote="\"", comment.char="")
adapt <- read.csv("~/research/seams2017_data/probadapt10xreq.csv", quote="\"", comment.char="")

start <- read.csv("~/research/seams2017_data/poorstart4serv10xreq.csv", quote="\"", comment.char="")
adapt <- read.csv("~/research/seams2017_data/adapt4serv10xreq.csv", quote="\"", comment.char="")

adapt <- read.csv("~/research/seams2017_data/smallstart10x.csv", quote="\"", comment.char="")

# switch to .1 pp, .1 verb pen, .2 mutation
adapt <- read.csv("~/research/seams2017_data/adapt2start4serv10xreq.csv", quote="\"", comment.char="")
adapt <-  read.csv("~/research/seams2017_data/adapt6start10xreq.csv", quote="\"", comment.char="")

short <-  read.csv("~/research/seams2017_data/bothshort.csv", quote="\"", comment.char="")

start <- read.csv("~/research/seams2017_data/econpoor.csv")
adapt <- read.csv("~/research/seams2017_data/econgood.csv")
adapt <- read.csv("~/research/seams2017_data/econshort.csv")

# both scenario
scratch <- read.csv("~/research/seams2017_data/fromscratch/bothscratch.csv")
long <- read.csv("~/research/seams2017_data/fromscratch/bothgood.csv")
short <- read.csv("~/research/seams2017_data/fromscratch/bothshort.csv")
poor <- read.csv("~/research/seams2017_data/fromscratch/bothpoor.csv")

short$start <- "Short"

scratch$start <- "Scratch"

poor$start <- "Poor"

long$start <- "Long"

data <- rbind(poor,long)
data <- rbind(data,scratch)
data <- rbind(data,short)

# now format it nicely
data <- rename(data,c("V1"="Generation","V2"="Initialization.Time.ms","V3"="Evaluation.Time.ms","V4"="Average.Individual.Size.Generation","V5"="Average.Individual.Size.Run","V6"="Best.Individual.Size.Generation","V7"="Best.Individual.Size.Run","V8"="Average.Fitness.Generation","V9"="Best.Fitness.Generation","V10"="Best.Fitness.Run"))

# lets try out some graphs
# fitness / generation in human readable way
p <- ggplot(data=data, aes(x=generation,y=profit,color=start))
p + geom_point() + theme_bw() + ylab("Profit") + xlab("Generation") 
#+ coord_cartesian(ylim=c(2750, 3000)

p <- ggplot(data=subset(data,start=="Good"), aes(x=generation,y=profit,color=start))
p + geom_point() + theme_bw() + ylab("Profit") + xlab("Generation") 

# convert generations to catagorical for making boxplots
datacat <- data
datacat$generation <- as.factor(datacat$generation)

datacat$start = factor(datacat$start,levels=c("Short","Long","Scratch","Poor"))

p <- ggplot(data=datacat, aes(x=generation,y=profit,color=start))
p <- p + ylab("Profit") + xlab("Generation") + scale_color_discrete(name="Starting Plan") +ggtitle("4 Data Centers and 10x Requests") + scale_fill_grey() + theme_classic() 
p <- p + theme(text=element_text(size=20), title=element_text(size=32,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + geom_boxplot() 

p <- ggplot(data=subset(datacat,start=="Good"), aes(x=generation,y=profit,color=start))
p + geom_boxplot() + ylab("Profit") + xlab("Generation") + scale_color_discrete(name="Starting Plan\nQuality") +ggtitle("4 Data Centers and 10x Requests") + scale_fill_grey() + theme_classic()

# just adapt
p <- ggplot(data=data, aes(x=generation,y=profit,color=start))
p + geom_line() + theme_bw() + ylab("Profit") + xlab("Generation")

agglong <- aggregate(long,by=list(long$generation,long$start), FUN=median,na.rm=TRUE)
agglong$cumruntime <- cumsum(agglong$runtime)

aggshort <- aggregate(short,by=list(short$generation,short$start), FUN=median,na.rm=TRUE)
aggshort$cumruntime <- cumsum(aggshort$runtime)

aggscratch <- aggregate(scratch,by=list(scratch$generation,scratch$start), FUN=median,na.rm=TRUE)
aggscratch$cumruntime <- cumsum(aggscratch$runtime)

aggpoor <- aggregate(poor,by=list(poor$generation,poor$start), FUN=median,na.rm=TRUE)
aggpoor$cumruntime <- cumsum(aggpoor$runtime)

aggdata<-rbind(agglong,aggshort)
aggdata <- rbind(aggdata,aggscratch)
aggdata <- rbind(aggdata,aggpoor)

# eval time / generation
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p + geom_line() + theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Profit") + scale_color_discrete(name="Starting Plan\nQuality") + ggtitle("Profit vs Evaluation Time") #+ coord_cartesian(xlim=c(0, 20))

