genus= read.csv('/home/pratik/Documents/diversity/MGX_UC_genus_level.csv', sep='\t', comment=',', head=T, row.names=1)

meta= read.csv('/home/pratik/Documents/MGX_combo_meta.txt', sep='\t', comment='', head=T)

genus= t(genus)
dim(genus)

library(vegan)
genus = genus[row.names(genus) %in% meta$External_ID,]

meta = meta[meta$External_ID %in% row.names(genus),]
dim(meta)

meta$shannon<-diversity(genus, "shannon")
meta$simpson<-diversity(genus, "simpson")


par(mfrow = c(2, 2))

#Then plot each metric.
hist(meta$shannon, main="Shannon diversity", xlab="", breaks=10)
hist(meta$simpson, main="Simpson diversity", xlab="", breaks=10)

shapiro.test(meta$simpson)
shapiro.test(meta$shannon)

qqnorm(meta$shannon, pch = 1, frame = FALSE)
qqline(meta$shannon, col = "steelblue", lwd = 2)

qqnorm(meta$simpson, pch = 1, frame = FALSE)
qqline(meta$simpson, col = "steelblue", lwd = 2)


t.test(meta$simpson ~ meta$Diagnosis, alternative ="less")

t.test(meta$shannon ~ meta$Diagnosis, alternative ="less")


pairwise.wilcox.test(meta$simpson, meta$Diagnosis, p.adjust.method="fdr")

pairwise.wilcox.test(meta$shannon, meta$Diagnosis, p.adjust.method="fdr")

myColors <- ifelse(levels(meta$Diagnosis)=="UC" , "#FA8072", 
                   ifelse(levels(meta$SDiagnosis)=="Healthy", "#20B2AA",
                          "grey90" ) )


#Return the plot area to 1x1
par(mfrow = c(1, 2))
#Plot
boxplot(shannon ~ Diagnosis, data=meta, col=myColors, ylab="Shannon's diversity")

boxplot(simpson ~ Diagnosis, data=meta, col=myColors, ylab="Simpson's diversity")

