setwd("") 
data=read.table("diffGeneExp.txt",sep="\t",header=T,row.names=1,check.names=F)
Rank <- ceiling(rank(data, ties.method="first")/ceiling(nrow(data)*ncol(data)/10))
dimnames1=list(rownames(data),colnames(data))
rt=matrix(as.numeric(as.matrix(Rank)),nrow=nrow(data),dimnames=dimnames1)

library(pheatmap)
Type=c(rep("N",396),rep("T",84)) 
names(Type)=colnames(rt)
Type=as.data.frame(Type)

pdf("",height=8,width=16)  
pheatmap(rt, 
         annotation=Type, 
         show_colnames=F,
         color = colorRampPalette(c("green2", "black", "red2"))(50),
         cluster_cols =F,
         show_rownames=T,
         fontsize = 10,
         fontsize_row=10,
         fontsize_col=10)
dev.off()