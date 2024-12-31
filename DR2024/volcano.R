library(dplyr)
library(ggplot2)
library(ggthemes)
setwd("")
data <- read.table("", header=TRUE)

data$type[data$FDR < 0.05 & 
                data$logFC > 1] = "up" 
data$type[data$FDR < 0.05 &
                data$logFC < -1] = "down"
data$type[data$FDR < 0.05 & 
                abs(data$logFC) <=1] = "non"
data$type[data$FDR >= 0.05] = "non"

p<-ggplot(data,aes(x=logFC,y=-1*log10(FDR),colour=type))+
  xlab("log2(Fold Change)")+
  ylab("-log10(FDR)")+
  geom_point(size=1.5,alpha=1)+ 
  scale_color_manual(values =c("green2","grey","red2"))+
  geom_hline(yintercept=1.30103,linetype=3)+ 
  geom_vline(xintercept=c(-1,1),linetype=3)+
  theme_few()+ 
  theme(legend.title = element_blank())
ggsave("",width=4,height=3)