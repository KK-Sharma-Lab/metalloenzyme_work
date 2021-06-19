library(vegan)

#change the abundance table and meta data file accordingly

genus= read.csv('/path/to/abundance/table', sep='\t', comment=',', head=T, row.names = 1)
meta= read.csv('/path/to/meta/data/file', sep='\t', comment='', head=T)

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
genus_uc <- genus[row.names(genus) %in% UC$External_ID,]
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

dist.bray <- as.data.frame(as.matrix(d.bray))

#anosim test
anosim(d.bray, meta$sex, permutations = 1000)
#permanova test
adonis(d.bray ~ diagnosis, data = meta, permutations = 1000)
#permanova test
adonis(d.bray.nonibd ~ Age_grp, data = nonibd, permutations = 1000)
#permanova test
adonis(d.bray.nonibd ~ Sex, data = nonibd, permutations = 1000)

#permanova test
adonis(d.bray.uc ~ Age_grp, data = UC, permutations = 1000)
#permanova test
adonis(d.bray.uc ~ Sex, data = UC, permutations = 1000)

