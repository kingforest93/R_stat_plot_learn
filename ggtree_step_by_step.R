rm(list=ls())
gc()

#loading
library(ggplot2)
library(ggtree)

tree <- rtree(50)
# only topology line
p <- ggtree(tree)
# add axis
p1 <- p + theme_tree2()
# add node point
p2 <- p1 + geom_point2(aes(color=isTip, shape=isTip), size=3)
# add scale
p3 <- p2 + geom_treescale(x=0,y=45,width=0.5,color="black", fontsize=4)
# add inter_node point annotation
p4 <- p3 + geom_nodepoint(color="yellow", alpha=0.38, size=10)
# add tip point annotation
p5 <- p4 + geom_tippoint(color="green", shape=8, size=3)
# add tip lable
p6 <- p5 + geom_tiplab(size=3, color="purple", hjust=-.5)
# add inter_node lable
p7 <- p6 + geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=3)
# annotate a clade
p8 <- p7 + geom_cladelabel(node=59, label="annotated clade", offset=0.5)
# annotate part tips with strip
p9 <- p8 + geom_strip(57, 59, barsize=2, color='red', offset=1)
# highlight a clade
p10 <- p9 + geom_hilight(node=59, fill="green", alpha=.38)
# highlight two adjacent descendant clades
p11 <- p10 + geom_balance(node=54, fill='darkgreen', color='white', alpha=0.38)
# add legends
p12 <- p11 + theme(legend.position="right")

# gzoom to focus and enlarge a clade
library("ape")
library("ggtree")
# simple test
data(chiroptera)
gzoom(chiroptera,grep("Plecotus", chiroptera$tip.label))
# complicated test
groupInfo <- split(chiroptera$tip.label,gsub("_\\w+", "", chiroptera$tip.label))
chiroptera <- groupOTU(chiroptera,groupInfo)
p <- ggtree(chiroptera,aes(color=group)) + geom_tiplab() + xlim(NA, 23)
gzoom(p, grep("Plecotus",chiroptera$tip.label), xmax_adjust=2)

# facet to multiple trees
library(ggtree)
trees <- lapply(c(10, 20, 40), rtree)
class(trees) <- "multiPhylo"
ggtree(trees) + facet_wrap(~.id,scale="free") + geom_tiplab()
# facet to show different types of plots
tr <- rtree(30)
d1 <- data.frame(id=tr$tip.label,val=rnorm(30, sd=3))
p <- ggtree(tr)
p2 <- facet_plot(p,panel="dot", data=d1, geom=geom_point, aes(x=val), color='firebrick')
d2 <- data.frame(id=tr$tip.label,value=abs(rnorm(30, mean=100, sd=50)))
facet_plot(p2, panel='bar', data=d2,geom=geom_segment, aes(x=0, xend=value, y=y, yend=y), size=3,color='steelblue') + theme_tree2()