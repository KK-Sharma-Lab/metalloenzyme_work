library(vegan)
library(matrixStats)


genus= read.csv('/path/to/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

#in this meta, remove the other sample data only keep the 1 samples from each subject after week 20 (nonIBD)
meta= read.csv('/path/to/MTX_20_week_meta.txt', sep='\t', comment='', head=T)

genus <- t(genus)
genus <- genus[,colMeans(genus > 0) >= .1]
genus[is.na(genus)] <- 0
genus[1:10,1:2]


genus = genus[row.names(genus) %in% meta$External_ID,]

dim(genus)

meta = meta[meta$External_ID %in% row.names(genus),]

D <- vegdist(genus, na.rm=TRUE, method = "bray")

meta$ref_set<- (meta$diagnosis == "nonIBD") & (meta$week_num >= 20)
meta$ref_set

rand <- as.data.frame(as.matrix(meta)) 
write.csv(rand, 'meta.csv')

dim(meta)
dim(ref_set)

meta$activity_index <- colMedians(as.matrix(D), keep.names=TRUE)
meta$activity_index

rand <- as.data.frame(as.matrix(meta)) 
write.csv(rand, 'meta.csv')
 

disease_activity_threshold <- quantile(meta$activity_index[meta$diagnosis=="nonIBD"],na.rm = TRUE, 0.9)
disease_activity_threshold
eubiosis_lower_threshold <- quantile(meta$activity_index[meta$diagnosis=="nonIBD"],na.rm = TRUE, 0.1)/home/drkksharma/Desktop/Beta_diversity_analysis/taxa_diversity
eubiosis_lower_threshold

genus= read.csv('/path/to/MTX_combo_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

#this is normal meta containing all samples
meta= read.csv('/path/to/MTX_combo_meta.txt', sep='\t', comment='', head=T)

genus <- t(genus)
genus <- genus[,colMeans(genus > 0) >= .1]
genus[is.na(genus)] <- 0
genus[1:10,1:2]

genus = genus[row.names(genus) %in% meta$External_ID,]

dim(genus)

meta = meta[meta$External_ID %in% row.names(genus),]

D <- vegdist(genus, na.rm=TRUE, method = "bray")

meta$activity_index <- colMedians(as.matrix(D), keep.names=TRUE)
meta$activity_index

meta$active <- meta$activity_index >= disease_activity_threshold
meta$active
