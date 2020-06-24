rm(list=ls())
gc()

library(ggplot2)
dat <- data.frame(
	name <- c('A','A1','A2','A3','B','B1','B2','B3'),
	value <- c(20, 5, 7, 8, 10, 3, 2, 5),
	level <- c('1','2','2','2','1','2','2','2'),
	stringsAsFactors = FALSE
)

p <- ggplot(dat, aes(x=level, y=value, fill=name, alpha=level)) +
	geom_col(width=1, color='gray90', size=1, position=position_stack()) +
	geom_text(aes(label=name), size=3, position=position_stack(vjust=0.5)) +
	coord_polar(theta='y') +
	scale_alpha_manual(values=c('1'=1, '2'=0.62), guide=F) +
	scale_x_discrete(breaks=NULL) +
	scale_y_continuous(breaks=NULL) +
	scale_fill_brewer(palette='Dark2', na.translate=F) +
	labs(x=NULL, y=NULL) +
	theme_minimal()

library(sunburstR)
library(d3r)
library(htmltools)

dat <- data.frame(
  level1 = rep(c("a", "b"), each=3),
  level2 = paste0(rep(c("a", "b"), each=3), 1:3),
  size = c(10,5,2,3,8,6),
  stringsAsFactors = FALSE
)

tree <- d3_nest(dat, value_cols = "size")

sb1 <- sunburst(tree, width="100%", height=400)
sb2 <- sunburst(
  tree,
  legend = FALSE,
  width = "100%",
  height = 400
)
sb3 <- sund2b(tree, width="100%")