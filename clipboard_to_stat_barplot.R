rm(list=ls())
gc()

# load packages
library(tidyr)
library(agricolae)
library(ggplot2)

# read data
setwd("C:/Users/KingForest/Desktop")
data <- read.table(file="clipboard", header=T, sep="\t", na.strings="")
data <- gather(data, rep, var, -treat, na.rm=T)

# anova and multiple comparison
tmp <- aov(var ~ treat, data)
shapiro.test(resid(tmp)) # normality test
bartlett.test(var ~ treat, data) # homogeneity test
out <- LSD.test(tmp, 'treat', p.adj = 'bonferroni') # post-hoc test
temp <- out$means[order(out$means$var,decreasing=T),1:3]
out <- cbind(out$groups, temp$std / sqrt(temp$r)) # calculate standard error
colnames(out)[3] <- 'stderr'

# draw bar plot with error bars and significance tags
o.gp <- ggplot(out, aes(x=rownames(out), y=var, ymin=var-stderr, ymax=var+stderr)) +
	geom_bar(stat="identity", fill="lightblue", color="black", size=0.3) +
	geom_errorbar(color="black", width=0.2, size=0.3) +
	geom_text(aes(y=var+stderr+1, label=groups)) +
	labs(x="treatment", y=expression("photosynthesis"^"super")) +
	theme(panel.grid=element_blank(), panel.background=element_rect(fill="white", color="black", size=0.5),
		 axis.ticks=element_line(color="black", size=0.3), axis.text=element_text(color="black", size=9),
		 axis.title=element_text(color="black", size=11, face="bold"))
ggsave("barplot.pdf", o.gp, width=120, height=80, units="mm")

