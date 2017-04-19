library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# read in the data
data <- read.csv("~/research/seams2017_data/tdiffout_mecon.csv")
data <- read.csv("~/research/saso2017_data/tdiff2.csv")

# convert generations to catagorical for making boxplots
#datacat <- subset(data,data$scenario != 'requestsfourserv')
datacat <- data
datacat$generation <- as.factor(datacat$generation)

drops <- c("scenario","plan","generation")
dataagg <- datacat[,!(names(datacat) %in% drops)]

dataagg1 <- aggregate(dataagg,by=list(datacat$generation,datacat$scenario,datacat$plan),FUN=median,na.rm=TRUE)

# sum up the runtime
datasum <- ddply(dataagg1,.(Group.2,Group.3),transform,sumTime = cumsum(runtime))

# rename stupid factors
datacat$scenario <- revalue(datacat$scenario, c("econ"="Increased Costs", "failc"="Failing Data Center","fourserv"="New Data Center","requests"="Request Spike","requestsfourserv"="Request Spike & New Data Center","unreliable"="Network Unreliability"))

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

p <- ggplot(data=datacat, aes(x=generation,y=(distance),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Diversity\n(average pairwise tree edit distance)") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting Plan")
#,legend.position=c(.8,.5)

# normalized diversity
p <- ggplot(data=subset(datacat,!(datacat$scenario %in% c("New Data Center","Request Spike"))), aes(x=generation,y=(distance/averageSize),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Normalized Diversity\n(average pairwise tree edit distance)") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=25), title=element_text(size=25,face="bold"),legend.title=element_text(size=25,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + scale_x_discrete(breaks=c(0,10,20,30,40,50))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting\nPlan")

# structure diff

p <- ggplot(data=datacat, aes(x=generation,y=(structureDistance),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Diversity\n(average pairwise structre tree edit distance)") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting Plan")
#,legend.position=c(.8,.5)

# normalized diversity
p <- ggplot(data=datacat, aes(x=generation,y=(structureDistance/averageSize),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Normalized Diversity\n(average pairwise structre tree edit distance)") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting Plan")

# fitness
#all
p <- ggplot(data=datacat, aes(x=generation,y=(profit),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Fitness") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=25,face="bold"),legend.title=element_text(size=25,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + scale_x_discrete(breaks=0:15)
p +  coord_cartesian(xlim=c(1,15.25)) + facet_wrap(~ scenario,scales="free") + scale_color_manual(values=cbPalette,name="Starting Plan") 
# subset
p <- ggplot(data=subset(datacat,!(datacat$scenario %in% c("New Data Center","Request Spike"))), aes(x=generation,y=(profit),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Profit") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=25), title=element_text(size=25,face="bold"),legend.title=element_text(size=25,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p <- p + scale_x_discrete(breaks=0:15)
p +  coord_cartesian(xlim=c(1,15.25)) + facet_wrap(~ scenario,scales="free") + scale_color_manual(values=cbPalette,name="Starting\nPlan") 

# runtime

# remove data outside of interesting range
datashow <- subset(datasum,!(datasum$sumTime/1000 > 50 & datasum$Group.2 %in% c("econ","failc","fourserv")))
datashow <- subset(datashow,!(datashow$sumTime/1000 > 400 & (!(datashow$Group.2 %in% c("econ","failc","fourserv")))))

datashow <- subset(datasum,!(datasum$profit < 4000 & datasum$Group.2 %in% c("requestsfourserv")))
datashow <- subset(datasum,!(datasum$profit < 2500 & datasum$Group.2 %in% c("unreliable")))

datashow <- subset(datashow,(datashow$Group.2 %in% c("requestsfourserv","unreliable")))

# relabel
# rename stupid factors
datashow$Group.2 <- revalue(datashow$Group.2, c("econ"="Increased Costs", "failc"="Failing Data Center","fourserv"="New Data Center","requests"="Request Spike","requestsfourserv"="Request Spike & New Data Center","unreliable"="Network Unreliability"))


p <- ggplot(data=datashow, aes(y=profit,x=sumTime/1000,color=Group.3))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Profit") + scale_color_discrete(name="Starting Plan\nQuality") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=25), title=element_text(size=25,face="bold"),legend.title=element_text(size=25,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.5,"in"))
p + scale_colour_manual(values=cbPalette,name="Starting\nPlan") + geom_line(lwd=1.5)    + facet_wrap(~Group.2,scales = "free") #+ coord_cartesian(xlim=c(0.5,125))

#,legend.position=c(.8,.5)
# size

p <- ggplot(data=datacat, aes(x=generation,y=(averageSize),color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Average Individual Size") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + facet_wrap(~ scenario) + scale_color_manual(values=cbPalette,name="Starting Plan")

# MISC

p <- p + ylab("Profit") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_boxplot(lwd=2,fatten=0.5,,position=position_dodge(width=.95)) # + coord_cartesian(xlim=c(1,20.75)) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)

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

# econ scenario
scratch <- read.csv("~/research/seams2017_data/hotfix/econscratch.csv")
long <- read.csv("~/research/seams2017_data/hotfix/econlong.csv")
short <- read.csv("~/research/seams2017_data/hotfix/econshort.csv")
poor <- read.csv("~/research/seams2017_data/hotfix/econpoor.csv")

# econ scenario
scratch <- read.csv("~/research/seams2017_data/econb/econscratch.csv")
long <- read.csv("~/research/seams2017_data/econb/econlong.csv")
short <- read.csv("~/research/seams2017_data/econb/econshort.csv")
poor <- read.csv("~/research/seams2017_data/econb/econpoor.csv")

# 4serv scenario
scratch <- read.csv("~/research/seams2017_data/hotfix/4servscratch.csv")
long <- read.csv("~/research/seams2017_data/hotfix/4servlong.csv")
short <- read.csv("~/research/seams2017_data/hotfix/4servshort.csv")
poor <- read.csv("~/research/seams2017_data/hotfix/4servpoor.csv")

# 10x scenario
scratch <- read.csv("~/research/seams2017_data/hotfix/10xscratch.csv")
long <- read.csv("~/research/seams2017_data/hotfix/10xlong.csv")
short <- read.csv("~/research/seams2017_data/hotfix/10xshort.csv")
poor <- read.csv("~/research/seams2017_data/hotfix/10xpoor.csv")

# both scenario
scratch <- read.csv("~/research/seams2017_data/hotfix/bothscratch.csv")
long <- read.csv("~/research/seams2017_data/hotfix/bothlong.csv")
short <- read.csv("~/research/seams2017_data/hotfix/bothshort.csv")
poor <- read.csv("~/research/seams2017_data/hotfix/bothpoor.csv")

# huge scenario
scratch <- read.csv("~/research/seams2017_data/huge/hugescratch.csv")
long <- read.csv("~/research/seams2017_data/huge/hugelong.csv")
short <- read.csv("~/research/seams2017_data/huge/hugeshort.csv")
poor <- read.csv("~/research/seams2017_data/huge/hugepoor.csv")

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
p <- p + ylab("Profit") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_boxplot(lwd=2,fatten=0.5,,position=position_dodge(width=.95))  + coord_cartesian(xlim=c(1,20.75)) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)

p <- ggplot(data=subset(datacat,start=="Good"), aes(x=generation,y=profit,color=start))
p + geom_boxplot() + ylab("Profit") + xlab("Generation") + scale_color_discrete(name="Starting Plan\nQuality") +ggtitle("16 Data Centers and 1 Million Requests") + scale_fill_grey() + theme_classic()

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

aggdata$Group.2 = factor(aggdata$Group.2,levels=c("Short","Long","Scratch","Poor"))

# eval time / generation
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Profit") + scale_color_discrete(name="Starting Plan\nQuality") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + scale_colour_manual(values=cbPalette,name="Starting Plan") + geom_line(lwd=1.5)   + coord_cartesian(xlim=c(0.5,125))

