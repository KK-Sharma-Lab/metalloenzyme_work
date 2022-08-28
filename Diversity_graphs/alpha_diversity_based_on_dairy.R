library('vegan')
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(stringr)

setwd('/media/drkksharma/EUNI/EUNI_Analysis/Graphs')

data=read.csv('/home/drkksharma/Documents/Enz_diversity/MGX_combo.csv', sep='\t', head=TRUE, row.names=1)
data1=read.csv('/home/drkksharma/Documents/taxa_diversity/MGX_combo_genus_level.csv', sep='\t', head=TRUE, row.names=1)

meta=read.csv('/home/drkksharma/Documents/MGX_combo_meta.txt', sep = '\t')

data <- data[rowMeans(data > 0) >= .1,]
data <- t(data)
data <- data[rowMeans(data > 0) >= .1,]

data1 <- data1[rowMeans(data1 > 0) >= .1,]
data1 <- t(data1)
data1 <- data1[rowMeans(data1 > 0) >= .1,]

dim(meta)
meta = meta[meta$External_ID %in% row.names(data),]
meta<-meta[!(is.na(meta$Dairy) | meta$Dairy==""), ]
data<-data[row.names(data) %in% meta$External_ID,]
data1<-data1[row.names(data1) %in% meta$External_ID,]
# meta <- meta[order(meta$Sample_ID),]

UC<- subset(meta, diagnosis== "UC")
nonibd<- subset(meta, diagnosis== "nonIBD")
data_uc <- data[row.names(data) %in% UC$External_ID,]
data1_uc <- data1[row.names(data1) %in% UC$External_ID,]


UC$eshannon<-diversity(data_uc, "shannon")
UC$einvsimpson<-diversity(data_uc, "invsimpson")
UC$gshannon<-diversity(data1_uc, "shannon")
UC$ginvsimpson<-diversity(data1_uc, "invsimpson")

d.bray <- vegdist(data_uc, na.rm=TRUE)
d.bray[is.na(d.bray)] <- 0

d.bray1 <- vegdist(data1_uc, na.rm=TRUE)
d.bray1[is.na(d.bray1)] <- 0


# colr<- brewer.pal(5, "Set2")
# colr

# png("/home/drkksharma/Desktop/diary_genus.png", width = 10, height = 7, units='in', res=500)
# mod <- vegan::betadisper(as.dist(d.bray), as.factor(UC$Dairy))
# op <- par(family = "serif")
# m<-plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, segments = FALSE,label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
# legend( 0.3,-0.05, legend=c("No Consumption", "3 or more time a day"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
# dev.off()
# 
# png("/home/drkksharma/Desktop/diary_enzyme.png", width = 10, height = 7, units='in', res=500)
# mod <- vegan::betadisper(as.dist(d.bray1), as.factor(UC$Dairy))
# op <- par(family = "serif")
# m<-plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, segments = FALSE,label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
# legend( 0.52,-0.05, legend=c("No Consumption", "3 or more time a day"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
# dev.off()


a<-ggplot(UC, aes(x=Dairy, y=eshannon, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.03)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=9, family = "serif", color="black"),
        axis.text.y = element_text(size=9, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

b<-ggplot(UC, aes(x=Dairy, y=einvsimpson, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 2)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=9, family = "serif", color="black"),
        axis.text.y = element_text(size=9, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

c<-ggplot(UC, aes(x=Dairy, y=gshannon, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.08)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=9, family = "serif", color="black"),
        axis.text.y = element_text(size=9, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

d<-ggplot(UC, aes(x=Dairy, y=ginvsimpson, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.3)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=9, family = "serif", color="black"),
        axis.text.y = element_text(size=9, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

png("/home/drkksharma/Desktop/UC_alpha_diversity_dairy_consumption.png", width = 16, height = 9, units='in', res=500)
grid.arrange(a,b,c,d, ncol=2)
dev.off()

###############################################################################################
data_nonibd <- data[row.names(data) %in% nonibd$External_ID,]
data1_nonibd <- data1[row.names(data1) %in% nonibd$External_ID,]

nonibd$eshannon<-diversity(data_nonibd, "shannon")
nonibd$einvsimpson<-diversity(data_nonibd, "invsimpson")
nonibd$gshannon<-diversity(data1_nonibd, "shannon")
nonibd$ginvsimpson<-diversity(data1_nonibd, "invsimpson")

a<-ggplot(nonibd, aes(x=Dairy, y=eshannon, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.03)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))
  
b<-ggplot(nonibd, aes(x=Dairy, y=einvsimpson, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 2)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))

c<-ggplot(nonibd, aes(x=Dairy, y=gshannon, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.08)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))
  
d<-ggplot(nonibd, aes(x=Dairy, y=ginvsimpson, fill=Dairy))+
  geom_boxplot(width=0.3, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.2)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))


png("/home/drkksharma/Desktop/nonibd_alpha_diversity_dairy_consumption.png", width = 16, height = 9, units='in', res=500)
grid.arrange(a,b,c,d, ncol=2)
dev.off()

###########################################################################################

meta$eshannon<-diversity(data, "shannon")
meta$einvsimpson<-diversity(data, "invsimpson")
meta$gshannon<-diversity(data1, "shannon")
meta$ginvsimpson<-diversity(data1, "invsimpson")


a<-ggplot(meta, aes(x=Dairy, y=eshannon, fill=diagnosis))+
  geom_boxplot(width=0.5, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.03)+
  theme(legend.position = "right",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

b<-ggplot(meta, aes(x=Dairy, y=einvsimpson, fill=diagnosis))+
  geom_boxplot(width=0.5, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Enzyme Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 2)+
  theme(legend.position = "right",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

c<-ggplot(meta, aes(x=Dairy, y=gshannon, fill=diagnosis))+
  geom_boxplot(width=0.5, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Shannon Diversity", y="Shannon's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.08)+
  theme(legend.position = "right",plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"), 
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

d<-ggplot(meta, aes(x=Dairy, y=ginvsimpson, fill=diagnosis))+
  geom_boxplot(width=0.5, )+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Genus Simpson Diversity", y="Simpson's Index", x="Self-reported Dairy intake")+
  #geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 0.3)+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, lineheight=15, color = "black"),
        text=element_text(size=10, family="serif"),
        axis.text.x = element_text(size=8, family = "serif", color="black"),
        axis.text.y = element_text(size=7, family = "serif", color="black"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

png("/home/drkksharma/Desktop/dairy_consumption.png", width = 16, height = 9, units='in', res=500)
grid.arrange(a,b,c,d, ncol=2)
dev.off()
