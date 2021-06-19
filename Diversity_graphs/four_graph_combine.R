library('vegan')

#setwd("/path/to/working/directory/")

genus= read.csv('/path/to/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta= read.csv('/path/to/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus1= read.csv('/path/to/MGX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta1= read.csv('/path/to/MGX_combo_meta.txt', sep='\t', comment='', head=T)

genus <- t(genus)
dim(genus)
genus[genus == 0] <- NA

genus<-genus[rowSums(is.na(genus)) != ncol(genus),]

genus[is.na(genus)] <- 0

genus[1:10,1:2]

meta = meta[meta$External_ID %in% row.names(genus),]
dim(meta)
#take UC meta subset
UC<- subset(meta, diagnosis== "UC")
dim(UC)

#take UC abundance subset
genus_uc = genus[row.names(genus) %in% UC$External_ID,]
dim(genus_uc)

#take nonibd meta subset
nonibd<- subset(meta, diagnosis== "nonIBD")
dim(nonibd)

#take nonibd abundance subset
genus_nonibd = genus[row.names(genus) %in% nonibd$External_ID,]
dim(genus_nonibd)

# get Bray-Curtis distances (default for Vegan)
d.bray.uc <- vegdist(genus_uc, na.rm=TRUE)
d.bray.uc[is.na(d.bray.uc)] <- 0

d.bray.nonibd <- vegdist(genus_nonibd, na.rm=TRUE)
d.bray.nonibd[is.na(d.bray.nonibd)] <- 0
#==============================================================================#
genus1 <- t(genus1)
dim(genus1)
genus1[genus1 == 0] <- NA

genus1<-genus1[rowSums(is.na(genus1)) != ncol(genus1),]

genus1[is.na(genus1)] <- 0

genus1[1:10,1:2]

meta1 = meta1[meta1$External_ID %in% row.names(genus1),]
dim(meta1)
#take UC meta subset
UC1<- subset(meta1, diagnosis== "UC")
dim(UC1)

#take UC abundance subset
genus_uc1 = genus1[row.names(genus1) %in% UC1$External_ID,]
dim(genus_uc1)

#take nonibd meta subset
nonibd1<- subset(meta1, diagnosis== "nonIBD")
dim(nonibd1)

#take nonibd abundance subset
genus_nonibd1 = genus1[row.names(genus1) %in% nonibd1$External_ID,]
dim(genus_nonibd1)

# get Bray-Curtis distances (default for Vegan)
d.bray.uc1 <- vegdist(genus_uc1, na.rm=TRUE)
d.bray.uc1[is.na(d.bray.uc1)] <- 0

d.bray.nonibd1 <- vegdist(genus_nonibd1, na.rm=TRUE)
d.bray.nonibd1[is.na(d.bray.nonibd1)] <- 0

#==========================================================================================#
#
#combining the graphs
png("sex_merge_graph.png", width = 9, height = 8, units='in', res=900)
par(mfrow = c(2, 2), mar=c(4.5, 4, 3, 1.8), mgp=c(1.7, 0.8, 0))

mod <- vegan::betadisper(as.dist(d.bray.nonibd), as.factor(nonibd$sex))
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main="NONIBD ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2"))
legend( 0.3,-0.3, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('ANOVA p<0.01', side=1, line=3, cex=0.8)
mtext('MTX', side=2, line=2.7, cex=1)

mod <- vegan::betadisper(as.dist(d.bray.uc), as.factor(UC$sex))
plot(mod, ellipse = TRUE, hull= FALSE, main=" UC ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2"))
legend( 0.28,-0.4, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('ANOVA p=0.068', side=1, line=3, cex=0.8)

mod <- vegan::betadisper(as.dist(d.bray.nonibd1), as.factor(nonibd1$sex))
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2"))
legend( 0.3,-0.3, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('ANOVA p<0.001', side=1, line=3, cex=0.8)
mtext('MGX', side=2, line=2.7, cex=1)

mod <- vegan::betadisper(as.dist(d.bray.uc1), as.factor(UC1$sex))
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2"))
legend( 0.25,-0.13, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
mtext('ANOVA p<0.001', side=1, line=3,cex=0.8)
dev.off()


#combined graph of age_grp
png("age_merge_graph.png", width = 9, height = 8, units='in', res=900)
par(mfrow = c(2, 2), mar=c(4.5, 4, 3, 1.8), mgp=c(1.7, 0.8, 0))
mod <- vegan::betadisper(as.dist(d.bray.nonibd), as.factor(nonibd$Age_grp))
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main="NONIBD ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19), cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.35,-0.3, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
mtext('TukeyHSD \n Y-O padj<0.001,   Y-A padj<0.001,   A-O padj=0.076 ', side=1, line=3.5, cex=0.8)
mtext('MTX', side=2, line=2.7, cex=1)

mod <- vegan::betadisper(as.dist(d.bray.uc), as.factor(UC$Age_grp))
plot(mod, ellipse = TRUE, hull= FALSE, main=" UC ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.3,-0.4, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
mtext('TukeyHSD \n Y-O padj=0.95,   Y-A padj=0.23,   A-O padj=0.85 ', side=1, line=3.5, cex=0.8)

mod <- vegan::betadisper(as.dist(d.bray.nonibd1), as.factor(nonibd1$Age_grp))
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.35,-0.2, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
mtext('TukeyHSD \n Y-O padj<0.01,   Y-A padj<0.001,   A-O padj=0.13 ', side=1, line=3.5, cex=0.8)
mtext('MGX', side=2, line=2.7, cex=1)

mod <- vegan::betadisper(as.dist(d.bray.uc1), as.factor(UC1$Age_grp))
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19),cex.lab=0.7, cex.axis=0.7, col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.3,-0.1, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
mtext('TukeyHSD \n Y-O padj<0.05,   Y-A padj=0.40,   A-O padj=0.63 ', side=1, line=3.5, cex=0.8)
dev.off()
