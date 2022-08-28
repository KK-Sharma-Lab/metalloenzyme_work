library('vegan')

#setwd("/home/pratik/Dissimilarity_graphs/")

genus= read.csv('/home/pratik/Documents/diversity/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta= read.csv('/home/pratik/Documents/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus <- t(genus)
dim(genus)

genus[is.na(genus)] <- 0

genus[1:10,1:2]

meta = meta[meta$External_ID %in% row.names(genus),]

# get Euclidean distance
d.euc <- dist(genus)


# get Bray-Curtis distances (default for Vegan)
d.bray <- vegdist(genus, na.rm=TRUE)
d.bray[is.na(d.bray)] <- 0
#d.bray

# Run PCoA (not PCA)
pc.euc <- cmdscale(d.euc, k=2)


png("/home/pratik/Dissimilarity_graphs/MTX/Taxonomic Beta diversity between UC and nonIBD metatranscriptomes consisting metalloenzymes (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
mod <- vegan::betadisper(as.dist(d.bray), as.factor(meta$diagnosis), bias.adjust=TRUE)
op <- par(family = "serif")
plot(mod, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( 0.4,0.1, legend=c("NonIBD", "UC"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
dev.off()


png("/home/pratik/Dissimilarity_graphs/MTX/Taxonomic Beta diversity between UC and nonIBD metatranscriptomes consisting metalloenzymes (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot
op <- par(family = "serif")
plot(pc.euc, type="n")
#Add the points colored by age
points(pc.euc, cex=1.5, pch=20, col=c("red", "blue")[meta$Diagnosis])
#Add a legend
legend("topright", 
  legend = c("nonIBD", "UC"), 
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

#########################################################################################################################

genus1= read.csv('/home/pratik/Documents/Enz_diversity/MTX_combo.csv', sep='\t', comment=',', head=T, row.names=1)

meta1= read.csv('/home/pratik/Documents/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus1 <- t(genus1)
dim(genus1)

genus1[is.na(genus1)] <- 0

common.ids <- intersect(rownames(meta), rownames(genus1))

genus1[1:10,1:2]

meta1 = meta1[meta1$External_ID %in% row.names(genus1),]

dim(meta)

# get Euclidean distance
d.euc1 <- dist(genus1)


# get Bray-Curtis distances (default for Vegan)
d.bray1 <- vegdist(genus1, na.rm=TRUE)
d.bray1[is.na(d.bray1)] <- 0
#d.bray

# Run PCoA (not PCA)
pc.euc1 <- cmdscale(d.euc1, k=2)


mod1 <- vegan::betadisper(as.dist(d.bray1), as.factor(meta1$diagnosis))

png("/home/pratik/Dissimilarity_graphs/MTX/Beta diversity between UC and nonIBD metagenomes consisting metalloenzymes (Bray-Curtis distance).png", width = 9, height = 8, units='in', res=900)
op <- par(family = "serif")
plot(mod1, ellipse = TRUE, hull= FALSE, main=" ", sub=NULL, xlim = c(-0.3, 0.2), segments = FALSE, label = FALSE,  pch = c(19,19), col=c("indianred2", "steelblue2"))
legend( 0.2,0.3, legend=c("NonIBD", "UC"), col=c("indianred2", "steelblue2"), box.lty=1, pch = c(19,19) )
dev.off()


#par(mfrow = c(1, 1))
png("/home/pratik/Dissimilarity_graphs/MTX/Beta diversity between UC and nonIBD metagenomes consisting metalloenzymes (Euclidean distance).png", width = 8, height = 8, units='in', res=900)
par(mar=c(5.1, 4.1, 4.1, 8.3))
#Create a blank plot for the nmds
op <- par(family = "serif")
plot(pc.euc1, type="n")
#Add the points colored by age
points(pc.euc1, cex=1.5, pch=20, col=c("red", "blue")[meta$Diagnosis])
#Add a legend
legend("topright", 
  legend = c("nonIBD", "UC"), 
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
