library(igraph)
library(reshape2)

expFile=""     
geneFile=""     
cutoff=0.2                     
out=""          
setwd("")

data=read.table(expFile,header=T,sep="\t",row.names=1,check.names=F)

group=sapply(strsplit(colnames(data),"\\-"),"[",4)
group=sapply(strsplit(group,""),"[",1)
group=gsub("2","1",group)
rt=data[,group==0]


geneRT=read.table(geneFile,header=F,sep="\t",check.names=F)
data=t(rt[as.vector(geneRT[,1]),])
cordata=cor(data)

mydata = cordata
upper = upper.tri(mydata)
mydata[upper] = NA


df = data.frame(gene=rownames(mydata),mydata)
dfmeltdata = melt(df,id="gene")
dfmeltdata = dfmeltdata[!is.na(dfmeltdata$value),]
dfmeltdata = dfmeltdata[dfmeltdata$gene!=dfmeltdata$variable,]
dfmeltdata = dfmeltdata[abs(dfmeltdata$value)>cutoff,]

corweight = dfmeltdata$value
weight = corweight+abs(min(corweight))+5
d = data.frame(p1=dfmeltdata$gene,p2=dfmeltdata$variable,weight=dfmeltdata$value)
g = graph.data.frame(dfmeltdata,directed = FALSE)

E(g)$color = ifelse(corweight>0,rgb(254/255,67/255,101/255,abs(corweight)),rgb(0/255,0/255,255/255,abs(corweight)))
V(g)$size = 8
V(g)$shape = "circle"
V(g)$lable.cex = 1.2
V(g)$color = "white"
E(g)$weight = weight


pdf(out,width=7,height=6)
layout(matrix(c(1,1,1,0,2,0),byrow=T,nc=3),height=c(6,1),width=c(3,4,3))
par(mar=c(1.5,2,2,2))
vertex.frame.color = NA
plot(g,layout=layout_nicely,vertex.label.cex=V(g)$lable.cex,edge.width = E(g)$weight,edge.arrow.size=0,vertex.label.color="black",vertex.frame.color=vertex.frame.color,edge.color=E(g)$color,vertex.label.cex=V(g)$lable.cex,vertex.label.font=2,vertex.size=V(g)$size,edge.curved=0.4)

color_legend = c(rgb(254/255,67/255,101/255,seq(1,0,by=-0.01)),rgb(0/255,0/255,255/255,seq(0,1,by=0.01)))
par(mar=c(2,2,1,2),xpd = T,cex.axis=1.6,las=1)
barplot(rep(1,length(color_legend)),border = NA, space = 0,ylab="",xlab="",xlim=c(1,length(color_legend)),horiz=FALSE,
        axes = F, col=color_legend,main="")
axis(3,at=seq(1,length(color_legend),length=5),c(1,0.5,0,-0.5,-1),tick=FALSE)
dev.off()