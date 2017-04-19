library("emoa", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")


`10x4servmultilongfront` <- read.table("~/research/saso2017_data/10x4servmultilongfront.stat", quote="\"", comment.char="")
`10x4servmultishortfront` <- read.table("~/research/saso2017_data/10x4servmultishortfront.stat", quote="\"", comment.char="")
`10x4servmultipoorfront` <- read.table("~/research/saso2017_data/10x4servmultipoorfront.stat", quote="\"", comment.char="")


sass <- read.table("~/research/saso2017_data/10servmultiinit.csv", quote="\"", comment.char="")

scratch <- read.csv("~/research/saso2017_data/fromscratch/scratch.front",sep=" ",header = FALSE)
prism <- read.csv("~/research/OmnetFiles-selected/profitlatency.csv")
poor <- read.table("~/research/saso2017_data/fromscratch/poor.front", quote="\"", comment.char="",sep=" ",header=FALSE)
long <- read.table("~/research/saso2017_data/fromscratch/long.front", quote="\"", comment.char="",sep=" ",header=FALSE)
short <- read.table("~/research/saso2017_data/fromscratch/short.front", quote="\"", comment.char="",sep=" ",header=FALSE)

long <- read.table("~/research/saso2017_data/multilong/multilong/front4.stat", quote="\"", comment.char="",sep=" ",header=FALSE)


errorTable <- data.frame(long=c(),short=c(),poor=c(),scratch=c())
for (i in 0:9) {
  
  scratch <- read.csv(fname("~/research/seams2017_data/multilong/multiscratch/front",i),sep=" ",header = FALSE)
  prism <- read.csv("~/research/OmnetFiles-selected/profitlatency.csv")
  poor <- read.csv(fname("~/research/seams2017_data/multilong/multipoor/front",i),sep=" ",header = FALSE)
  long <- read.table(fname("~/research/seams2017_data/multilong/multilong/front",i), quote="\"", comment.char="",sep=" ",header=FALSE)
  short <- read.csv(fname("~/research/seams2017_data/multilong/multishort/front",i),sep=" ",header = FALSE)
  
  print(paste("Long:",pe(auc(prism),auc(long)),sep=" "))
  print(paste("Poor:",pe(auc(prism),auc(poor)),sep=" "))
  print(paste("Short:",pe(auc(prism),auc(short)),sep=" "))
  print(paste("Scratch:",pe(auc(prism),auc(scratch)),sep=" "))
  
  l <- pe(auc(prism),auc(long))
  sh <- pe(auc(prism),auc(short))
  p <- pe(auc(prism),auc(poor))
  sc <- pe(auc(prism),auc(scratch))
  
  nn <- data.frame(long=l,short=sh,poor=p,scratch=sc)
  
  errorTable <- rbind(errorTable,nn)
  
  print(nrow(unique(long)))
  
}


datacat$start = factor(datacat$start,levels=c("Short","Long","Scratch","Poor"))

p <- ggplot(data=datacat, aes(x=generation,y=profit,color=start))
p <- p + ylab("Profit") + xlab("Generation") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_boxplot(lwd=2,fatten=0.5,,position=position_dodge(width=.95))  + coord_cartesian(xlim=c(1,20.75)) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)


scratch$Tool <- "Scratch"
prism$Tool <- "PRISM"
poor$Tool <- "Poor"
long$Tool <- "Long"
short$Tool <- "Short"


sass <- rbind(scratch,poor)
sass <- rbind(sass,long)
sass <- rbind(sass,short)

sass <- short

sass <- rename(sass,c("V1"="profit","V2"="latency"))



data <- rbind(sass[,c("profit","latency","Tool")],prism)

# colorblind color scheme
cbPalette <- c("#000000", "#3EAA9A", "#C3E270", "#000000")

data$Tool = factor(data$Tool,levels=c("PRISM","Short","Long","Scratch","Poor"))

# rename short to GA Planner
data$Tool <- revalue(data$Tool, c("Short"="GA planner"))

p <- ggplot(data=data, aes(y=profit,x=latency,color=Tool)) + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.85,.35),legend.title=element_text(size=25,face="bold"),legend.text=element_text(size=20),legend.key.size=unit(0.75,"in"))
p + geom_line(lwd=2) + xlab("Latency") + ylab("Profit") + scale_color_manual(values=cbPalette,name="Planner")  #+ coord_cartesian(xlim=c(0, 20))





auc <- function(a) {
  x <- a[,2]
  y <- a[,1]
  id <- order(x)
  AUC <- sum(diff(x[id])*rollmean(y[id],2))
}

pe <- function(e,a){
  r <-(e - a)/a*100
}

distP <- function(a) {
  dist2d(unlist(a),unlist(prism[2,0-3]),unlist(prism[29,0-3]))
}

# distance of point a from the line formed by b c
dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

fname <- function(pre,n) {
  a <- paste(pre,n,sep="")
  a <- paste(a,"stat",sep=".")
}


data <- rename(data,c("V1"="Profit","V2"="Quality","V3"="Latency"))


data <- as.matrix(data)

datafix <- data 
datafix[,1] <- datafix[,1] * -1
datafix[,2] <- datafix[,2] * -1

data <- rename(data,c("V1"="Profit","V2"="Quality","V3"="Lattency"))

data <- as.matrix(data)

datafix2 <- data 
datafix2[,1] <- datafix2[,1] * -1
datafix2[,2] <- datafix2[,2] * -1

data <- `10x4servmultipoorfront`

data <- rename(data,c("V1"="Profit","V2"="Quality","V3"="Lattency"))

data <- as.matrix(data)

datafix3 <- data 
datafix3[,1] <- datafix3[,1] * -1
datafix3[,2] <- datafix3[,2] * -1

hypervolume_indicator(t(datafix2),t(datafix3),c(0,0,10000))

dominated_hypervolume(t(data),c(0,0,10000)) - dominated_hypervolume(t(datafix),c(0,0,10000))

