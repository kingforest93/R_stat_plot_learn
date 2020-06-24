rm(list=ls())
gc()

#load packages
library(venn)

#read data
eggnog <- read.table("eggnog.list", header=F)
nr <- read.table("nr.list", header=F)
uniref <- read.table("uniref.list", header=F)

#dara venn diagram
pdf(file="eggnog_nr_uniref.pdf")
venn(list(eggNOG=eggnog[,1], NCBI_NR=nr[,1], UniRef100=uniref[,1]), ilabels = TRUE, zcolor = "style")
dev.off()
