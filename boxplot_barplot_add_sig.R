rm(list=ls())
gc()

# load packages
library(ggplot2)
library(ggsignif)

# draw plot and add significance marks
ggplot(mpg, aes(class, hwy)) +
	geom_boxplot() +
	geom_signif(comparisons = list(c("compact", "midsize"), c("minivan", "suv")),
      	map_signif_level = TRUE, textsize=4) +
	ylim(10, 50) +
	theme(panel.background=element_rect(fill="white"), axis.line=element_line(size=0.3, colour="black"))

# anova and multiple comparison
library(agricolae)
tmp <- aov(hwy ~ class, data=mpg)
shapiro.test(resid(tmp)) # normality test
bartlett.test(hwy ~ class, data=mpg) # homogeneity test
out <- LSD.test(tmp, 'class', p.adj = 'bonferroni') # post-hoc test
temp <- out$means[order(out$means$hwy,decreasing=T),1:3]
out <- cbind(out$groups, temp$std / sqrt(temp$r)) # calculate standard error
colnames(out)[3] <- 'stderr'

# draw bar plot
o.p <- barplot(out$hwy, ylim=c(0,32), col=rainbow(7), xlab="class", ylab="hwy")
axis(1, at=o.p, labels=rownames(out), col="black", col.ticks="black")
arrows(o.p, out$hwy, o.p, out$hwy+out$stderr, angle=90, code=2, length=0.05)
text(o.p, out$hwy+out$stderr, out$groups, pos=3, adj=0.5)

# draw bar plot with error bars and significance tags
o.gp <- ggplot(out, aes(x=rownames(out), y=hwy, ymin=hwy-stderr, ymax=hwy+stderr)) +
	geom_bar(stat="identity", fill="lightblue", color="black", size=0.3) +
	geom_errorbar(color="black", width=0.2, size=0.3) +
	geom_text(aes(y=hwy+stderr+1.5, label=groups)) +
	scale_y_continuous(limits=c(0,35), breaks=seq(0,35,7)) +
	labs(x=expression("class"["sub"]), y=expression("hwy"^"super")) +
	theme(panel.grid=element_blank(), panel.background=element_rect(fill="white", color="black", size=0.5),
		 axis.ticks=element_line(color="black", size=0.3), axis.text=element_text(color="black", size=9),
		 axis.title=element_text(color="black", size=11, face="bold"))
ggsave("barplot_with_sig.pdf", o.gp, width=120, height=80, units="mm")

