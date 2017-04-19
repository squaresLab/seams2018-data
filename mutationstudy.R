library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

data <- read.csv("~/research/seams2017_data/console.log")

data <- subset(data,data$plan != 'scratch')

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")



p <- ggplot(data=data, aes(x=scenario,y=dAverageFitness,color=plan))
p <- p + geom_boxplot()
p <- p + ylab("Delta Fitness") + xlab("Scenario") + scale_fill_discrete(name="Starting Plan")  + theme_bw()
p <- p + theme(text=element_text(size=10), title=element_text(size=30,face="bold"),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p  + scale_color_manual(values=cbPalette,name="Starting Plan") # + facet_wrap(~ scenario)

