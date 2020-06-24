rm(list=ls())
gc()

# load packages
library(ggplot2)
library(agricolae)
library(tidyr)

#read data
dat <- read.table("clipboard", header=T)
dat$Sub <- paste(c(rep("-PEG",3),rep("+PEG",3)), dat$Sub, sep="_")
dat <- gather(dat, key="Replication", value="Value", -Sub)

# anova and multiple comparison
tmp <- aov(Value ~ Sub, data=dat)
shapiro.test(resid(tmp)) # normality test
bartlett.test(Value ~ Sub, data=dat) # homogeneity test
out <- LSD.test(tmp, 'Sub', p.adj = 'none') # post-hoc test
out$groups[c('-PEG_0h','-PEG_2h','-PEG_10d','+PEG_0h','+PEG_2h','+PEG_10d'),]

out <- cbind(rownames(out$groups), out$groups, out$means$std / sqrt(out$means$r)) # calculate standard error
colnames(out)[4] <- 'stderr'
colnames(out)[1] <- 'class'
rownames(out) <- 1:nrow(out)

# draw bar plot with error bars and significance tags
o.gp <- ggplot(out, aes(x=class, y=Value)) +
	geom_bar(stat="identity", fill="lightblue", color="black", size=0.3) +
	geom_errorbar(aes(ymin=Value-stderr, ymax=Value+stderr), color="black", width=0.2, size=0.3) +
	geom_text(aes(y=Value+stderr+1.5, label=groups)) +
	scale_y_continuous(limits=c(0,28), breaks=seq(0,28,7)) +
	labs(y=expression("Value"^"super")) +
	theme(panel.grid=element_blank(), panel.background=element_rect(fill="white", color="black", size=0.5),
		 axis.ticks=element_line(color="black", size=0.3), axis.text=element_text(color="black", size=9),
		 axis.title=element_text(color="black", size=11, face="bold"))
ggsave("barplot_with_sig.pdf", o.gp, width=120, height=80, units="mm")