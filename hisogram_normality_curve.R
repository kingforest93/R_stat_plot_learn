rm(list=ls())
gc()

# load packages
library(ggplot2)

# read data
setwd("D:/Biowork/test")
dt <- read.csv("RIL.csv", header=T, sep=",", na.strings="NA")
rownames(dt) <- dt[,1]
dt <- dt[,-1]

#normality test
i <- 2
n <- length(dt[,i])
w <- (max(dt[,i]) - min(dt[,i]))/15
pnor <- dnorm(dt[,i], mean(dt[,i]), sd=sd(dt[,i]))
t <- shapiro.test(dt[,i])

# draw hisogram
hp <- ggplot(dt, aes(x=dt[,i])) +
	geom_histogram(binwidth=w, fill="orange", color="black") +
	geom_line(aes(y=n*w*pnor), color="blue", size=0.5) +
	annotate("text", x=max(dt[,i])-1, y=n/15+2, label=paste("Normality test\n P-value=",signif(t$p.value, digits=3))) +
	theme(panel.grid=element_blank(), panel.background=element_rect(fill="white", color="black", size=0.5),
		 axis.ticks=element_line(color="black", size=0.3), axis.text=element_text(color="black", size=9),
		 axis.title=element_text(color="black", size=11, face="bold"))
ggsave("histogram_normality.pdf", hp, width=120, height=100, units="mm")
