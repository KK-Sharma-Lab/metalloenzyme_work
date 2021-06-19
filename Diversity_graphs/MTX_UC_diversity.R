library('vegan')

#setwd("/path/to/working/directory/")

genus= read.csv('/path/to/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta= read.csv('/path/to/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus <- t(genus)
dim(genus)
genus[genus == 0] <- NA

genus<-genus[rowSums(is.na(genus)) != ncol(genus),]

genus[is.na(genus)] <- 0

genus[1:10,1:2]

meta = meta[meta$External_ID %in% row.names(genus),]
dim(meta)

UC<- subset(meta, diagnosis== "UC")
dim(UC)

genus = genus[row.names(genus) %in% UC$External_ID,]
dim(genus)


# get Euclidean distance
d.euc <- dist(genus)


# get Bray-Curtis distances (default for Vegan)
d.bray <- vegdist(genus, na.rm=TRUE)
d.bray[is.na(d.bray)] <- 0


mod <- vegan::betadisper(as.dist(d.bray), as.factor(UC$sex))
png("Taxonomic Beta diversity of UC metatranscriptomes consisting metalloenzymes sex-wise (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( 0.4,0.01, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
dev.off()

mod <- vegan::betadisper(as.dist(d.bray), as.factor(UC$Age_grp))
png("Taxonomic Beta diversity of UC metatranscriptomes consisting metalloenzymes age-wise (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19), col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.4,0.01, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
dev.off()

# Run PCoA (not PCA)
pc.euc <- cmdscale(d.euc, k=2)

# Bray-Curtis principal coords
pc.bray <- cmdscale(d.bray,k=2)
#pc.bray
#if you want to find outlier
#t<- gsub(" ", "\t", pc.bray)
#t
#write.table(pc.bray,"sample.csv")





png("Taxonomic Beta diversity of UC metatranscriptome consisting metalloenzymes sex-wise (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot for the nmds
op <- par(family = "serif")
plot(pc.euc, type="n")
#Add the points colored by age
points(pc.euc, cex=1.5, pch=20, col=c("red", "blue")[UC$Sex])
# Add a legend
legend("topright", 
  legend = c("Male", "Female"), 
  col=c("red","blue"), 
  pch = c(20,20), 
  bty = "o", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(-0.23,0),
    xpd=TRUE)  
dev.off()


png("Taxonomic Beta diversity of UC metatranscriptome consisting metalloenzymes age-wise (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot for the nmds
op <- par(family = "serif")
plot(pc.euc, type="n")
#Add the points colored by age
points(pc.euc, cex=1.5, pch=20, col=c("red", "blue", "green")[UC$Age_grp])
# Add a legend
legend("topright", 
  legend = c("Child", "Adult", "Old"), 
  col=c("red","blue", "green"), 
  pch = c(20,20,20), 
  bty = "o", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(-0.2,0),
    xpd=TRUE)  


#closing pdf
dev.off()





################################################################################################################################

genus1= read.csv('/path/to/MTX_combo.csv', sep='\t', comment=',', head=T, row.names=1)

meta1= read.csv('/path/to/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus1 <- t(genus1)
dim(genus1)
genus1[genus1 == 0] <- NA

genus1<-genus1[rowSums(is.na(genus1)) != ncol(genus1),]

genus1[is.na(genus1)] <- 0

genus1[1:10,1:2]

meta1 = meta1[meta1$External_ID %in% row.names(genus1),]
dim(meta1)

UC<- subset(meta1, diagnosis== "UC")
dim(UC)

genus1 = genus1[row.names(genus1) %in% UC$External_ID,]
dim(genus1)

# get Euclidean distance
d.euc1 <- dist(genus1)


# get Bray-Curtis distances (default for Vegan)
d.bray1 <- vegdist(genus1, na.rm=TRUE)
d.bray1[is.na(d.bray1)] <- 0
#d.bray


mod1 <- vegan::betadisper(as.dist(d.bray1), as.factor(UC$Sex))
png("Beta diversity of UC metatranscriptomes consisting metalloenzymes sex-wise (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
op <- par(family = "serif")
plot(mod1, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( 0.2,0.01, legend=c("Female", "Male"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
dev.off()

mod1 <- vegan::betadisper(as.dist(d.bray1), as.factor(UC$Age_grp))
png("Beta diversity of UC metatranscriptomes consisting metalloenzymes age-wise (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
op <- par(family = "serif")
plot(mod1, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19,19), col=c("indianred2", "steelblue2", "palegreen3"))
legend( 0.2,0.01, legend=c("Adult", "Old", "Young"), col=c("indianred2", "steelblue2", "palegreen3"), box.lty=1, pch = c(19,19,19) )
dev.off()

# Run PCoA (not PCA)
pc.euc1 <- cmdscale(d.euc1, k=2)

# Bray-Curtis principal coords
pc.bray1 <- cmdscale(d.bray1,k=2)
#pc.bray
#if you want to find outlier
#t<- gsub(" ", "\t", pc.bray)
#t
#write.table(pc.bray,"sample.csv")




png("Beta diversity of UC metagenomes consisting metalloenzymes sex-wise (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot for the nmds
op <- par(family = "serif")
plot(pc.euc1, type="n")
#Add the points colored by age
points(pc.euc1, cex=1.5, pch=20, col=c("red", "blue")[UC$Sex])
# Add a legend
legend("topright", 
  legend = c("Male", "Female"), 
  col=c("red","blue"), 
  pch = c(20,20), 
  bty = "o", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(-0.23,0),
    xpd=TRUE)  
dev.off()


png("Beta diversity of UC metagenomes consisting metalloenzymes age-wise (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot for the nmds
op <- par(family = "serif")
plot(pc.euc1, type="n")
#Add the points colored by age
points(pc.euc1, cex=1.5, pch=20, col=c("red", "blue", "green")[UC$Age_grp])
# Add a legend
legend("topright", 
  legend = c("Child", "Adult", "Old"), 
  col=c("red","blue", "green"), 
  pch = c(20,20,20), 
  bty = "o", 
  pt.cex = 2, 
  cex = 1.2, home/pratik/Dissimilarity_graphs/
  text.col = "black", 
  horiz = F , 
  inset = c(-0.2,0),
    xpd=TRUE)  


#closing pdf
dev.off()


