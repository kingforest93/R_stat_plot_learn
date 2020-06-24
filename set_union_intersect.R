#clean
rm(list=ls())
gc()

#read data
total_genes <- read.csv("jiaPvsjianP.csv", header=T)
withP_genes <- total_genes$withP
withoutP_genes <- total_genes$withoutP

#union, intersection, and difference of two sets
union_genes <- union(withP_genes, withoutP_genes)
inter_genes <- intersect(withP_genes, withoutP_genes)
union_genes <- data.frame(union=union_genes)
inter_genes <- data.frame(inter=inter_genes)
withP_only_genes <- setdiff(withP_genes, withoutP_genes)
withoutP_only_genes <- setdiff(withoutP_genes, withP_genes)
withP_only <- data.frame(withP_only=withP_only_genes)
withoutP_only <- data.frame(withoutP_only=withoutP_only_genes)

#save results
write.table(union_genes, "union_genes.csv", row.names=F, quote=F)
write.table(inter_genes, "inter_genes.csv", row.names=F, quote=F)
write.table(withP_only, "withP_only_genes.csv", row.names=F, quote=F)
write.table(withoutP_only, "withoutP_only_genes.csv", row.names=F, quote=F)