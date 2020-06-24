#load packages
library(dplyr)
library(org.At.tair.db)
columns(org.At.tair.db)
library(clusterProfiler)
library(pathview)

#read gene expression count matrix, often output from DESeq2 or edgeR
DE_results <- read.table(file = "genes.counts.matrix.C1_vs_H1.DESeq2.DE_results",
                         sep = "\t",
                         header = T)
DE_record <- mutate(DE_results, GeneID = rownames(DE_results)) %>%
  filter(abs(log2FoldChange) > 1, padj < 0.05)
DE_list <- as.character(DE_record$GeneID)

#GO enrichment analysis
ego <- enrichGO(gene          = DE_list, 
                keyType       = "TAIR",  
                OrgDb         = org.At.tair.db, 
                ont           = "CC", 
                pAdjustMethod = "BH", 
                pvalueCutoff  = 0.05, 
                qvalueCutoff  = 0.05, 
                readable      = TRUE) 
as.data.frame(ego)

#save results as barplot, doplot, and bubble plot
pdf(file = "ego_barplot.pdf")
barplot(ego, showCategory=20, x = "GeneRatio")
dev.off()
pdf(file = "ego_dotplot.pdf")
dotplot(ego)
dev.off()
pdf(file = "ego_plotGOgraph.pdf")
plotGOgraph(ego)
dev.off()

#KEGG enrichment analysis
download_KEGG("ath", keggType = "KEGG", keyType = "kegg")
ekg <- enrichKEGG(gene = DE_list, organism = "ath", keyType = "kegg",
			pvalueCutoff = 0.05, pAdjustMethod = "BH",
			minGSSize = 10, maxGSSize = 500, qvalueCutoff = 0.2,
			use_internal_data = FALSE)
as.data.frame(ekg)

#save results as barplot, dotplot, and pathway map
barplot(ekg, showCategory=10, x = "GeneRatio")
dotplot(ekg, showCategory=10)
ath04141 <- pathview(gene.data  = DE_list,
                     pathway.id = "ath04141",
                     species    = "ath",
                     limit      = list(gene=max(abs(DE_list)), cpd=1))
