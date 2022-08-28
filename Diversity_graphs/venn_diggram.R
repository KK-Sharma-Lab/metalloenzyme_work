library(plyr)

EUNI<-read.csv("/media/drkksharma/EUNI/EUNI_Analysis/NEW/Combined_ME.tsv", sep='\t')
HMP<-read.csv("/home/drkksharma/Desktop/Enz_diversity/MGX_combo.csv",sep = '\t')

EUNI<-EUNI[,c(-2)]

EUNI<-ddply(EUNI,"EC_number", numcolwise(sum))

euni<-EUNI[,1]
euni[1]
hmp<-HMP[,1]

look<-matrix(ncol = 4)

common<-Reduce(intersect,list(euni,hmp))
u_euni<-setdiff(euni,hmp)
u_hmp<-setdiff(hmp,euni)

library(ggVennDiagram)
library(ggplot2)
library(RColorBrewer)
library(viridis)

x<-list(
  HMP<-hmp,
  EUNI<-euni
)

ggVennDiagram(x, category.names = c('HMP2','INDIAN'),label_alpha = 0, edge_size = 0.1)+
  ggplot2::scale_fill_gradient(low =  "#C7E9B4", high = "#1D91C0")+ggplot2::theme()


display.brewer.pal(3,"BuPu")
colr<-brewer.pal(3,'BuPu')

library(VennDiagram)
grid.newpage()
png('venn.png', units = 'in', res = 500, height = 5, width = 6)
draw.pairwise.venn(area1 = 296, area2 = 262, cross.area = 217, 
                   fill = colr[-1], scaled = FALSE,
                   category = c("INDIAN","HMP2"),
                   lwd = c(1,1),
                   cex = c(2,2,2), fontfamily = "serif",
                   cat.pos = c(340,20),
                   cat.dist = -0.05, cat.cex = 1.5)
dev.off()

display.brewer.all()

library(plyr)
library(dplyr)

EUNI<-read.csv("/media/drkksharma/EUNI/EUNI_Analysis/NEW/Combined_ME.tsv", sep='\t')
meta_euni<-read.csv("/media/drkksharma/EUNI/EUNI_Analysis/Final/EUNI_meta_data.tsv", sep='\t')
HMP<-read.csv("/home/drkksharma/Desktop/Enz_diversity/MGX_combo.csv",sep = '\t')
meta_hmp<-read.csv("/home/drkksharma/Desktop/MGX_combo_meta.txt", sep = '\t')

EUNI<-EUNI[,c(-2)]
EUNI<-ddply(EUNI,"EC_number", numcolwise(sum))

rownames(EUNI)<-EUNI[,1]
rownames(HMP)<-HMP[,1]

meta_euni = meta_euni[meta_euni$Sample_ID %in% colnames(EUNI),]

meta_hmp = meta_hmp[meta_hmp$External_ID %in% colnames(HMP),]

uc<-subset(meta_euni, Diagnosis=="UC")
uc_euni<-EUNI[,colnames(EUNI) %in% uc$Sample_ID]
uc_euni<-uc_euni[rowSums(uc_euni)!=0,]

he<-subset(meta_euni, Diagnosis=="Healthy")
he_euni<-EUNI[,colnames(EUNI) %in% he$Sample_ID]
he_euni<-he_euni[rowSums(he_euni)!=0,]

uc<-subset(meta_hmp, diagnosis=="UC")
uc_hmp<-HMP[,colnames(HMP) %in% uc$External_ID]
uc_hmp<-uc_hmp[rowSums(uc_hmp)!=0,]

he<-subset(meta_hmp, diagnosis=="nonIBD")
he_hmp<-HMP[,colnames(HMP) %in% he$External_ID]
he_hmp<-he_hmp[rowSums(he_hmp)!=0,]

library(plotly)

x<-list(
  rownames(he_euni),rownames(uc_euni),rownames(he_hmp),rownames(uc_hmp)
)

ggVennDiagram(x,label_alpha =0,category.names = c('Indian Healthy', 'Indian UC','HMP2 Healthy','HMP2 nonIBD'))+
  ggplot2::scale_fill_gradient(low =  "#FFEDA0", high = "#F03B20")+ scale_color_brewer(palette = 'Set1')


display.brewer.pal(3,"YlOrRd")
brewer.pal(3,'YlOrRd')
