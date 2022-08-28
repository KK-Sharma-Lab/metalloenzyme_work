library('vegan')

#setwd("/home/pratik/Dissimilarity_graphs/")

genus= read.csv('/home/drkksharma/Documents/taxa_diversity/MGX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta= read.csv('/home/drkksharma/Documents/MGX_combo_meta.txt', sep='\t', comment='', head=T)


genus <- t(genus)
dim(genus)

genus[is.na(genus)] <- 0

genus[1:10,1:2]

meta = meta[meta$External_ID %in% row.names(genus),]

dim(meta)

genus = genus[row.names(genus) %in% meta$External_ID,]
dim(genus)


# get Bray-Curtis distances (default for Vegan)
d.bray <- vegdist(genus, na.rm=TRUE)
d.bray[is.na(d.bray)] <- 0
#d.bray

######################
genus1= read.csv('/home/drkksharma/Documents/taxa_diversity/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta1= read.csv('/home/drkksharma/Documents/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus1 <- t(genus1)
dim(genus1)
dim(meta1)

genus1[is.na(genus1)] <- 0

genus1[1:10,1:2]

meta1 = meta1[meta1$External_ID %in% row.names(genus1),]
dim(meta1)

genus1 = genus1[row.names(genus1) %in% meta1$External_ID,]
dim(genus1)



# get Bray-Curtis distances (default for Vegan)
d.bray1 <- vegdist(genus1, na.rm=TRUE)
d.bray1[is.na(d.bray1)] <- 0



#combining the graphs
png("/home/drkksharma/Desktop/UC_vs_nonIBD.png", width = 8, height = 5, units='in', res=700)
par(mfrow = c(1, 2), mar=c(4.5, 2, 2, 1),  mgp=c(1.2, 0.5, 0))

mod <- vegan::betadisper(as.dist(d.bray), as.factor(meta$diagnosis))
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(0.1, 0), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( 0.4,-0.4, legend=c("NonIBD", "UC"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('PERMANOVA p<0.01', side=1, line=2, cex=0.8)
mtext('MGX', side=3, line=1, cex=1)

mod <- vegan::betadisper(as.dist(d.bray1), as.factor(meta1$diagnosis))
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(0.1, 0), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( -0.1,-0.24, legend=c("NonIBD", "UC"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('PERMANOVA p<0.001', side=1, line=2, cex=0.8)
mtext('MTX', side=3, line=1, cex=1)
dev.off()

d.bray1<-as.matrix(d.bray1)
