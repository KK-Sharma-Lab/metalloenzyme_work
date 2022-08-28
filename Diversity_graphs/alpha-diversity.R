genus= read.csv('/media/drkksharma/EUNI/EUNI_Analysis/Final/species_level.csv', sep='\t', comment=',', head=T, row.names=1)
 
meta= read.csv('/media/drkksharma/EUNI/EUNI_Analysis/Final/meta.csv', sep='\t', comment='', head=T)

genus= t(genus)
dim(genus)

library(vegan)
genus = genus[row.names(genus) %in% meta$External_ID,]

meta = meta[meta$External_ID %in% row.names(genus),]
dim(meta)

meta$shannon<-diversity(genus, "shannon")
meta$simpson<-diversity(genus, "simpson")

UC1<- subset(meta, Season== "Winter")
UC2<- subset(meta, Season== "Summer")
UC<- rbind(UC1, UC2)

par(mfrow = c(2, 2))

#Then plot each metric.
hist(UC$shannon, main="Shannon diversity", xlab="", breaks=10)
hist(UC$simpson, main="Simpson diversity", xlab="", breaks=10)

shapiro.test(UC$simpson)
shapiro.test(UC$shannon)

qqnorm(UC$shannon, pch = 1, frame = FALSE)
qqline(UC$shannon, col = "steelblue", lwd = 2)

qqnorm(UC$simpson, pch = 1, frame = FALSE)
qqline(UC$simpson, col = "steelblue", lwd = 2)


t.test(UC$simpson ~ UC$Season)

t.test(UC$shannon ~ UC$Season)


pairwise.wilcox.test(meta$simpson, meta$Season, p.adjust.method="fdr")

pairwise.wilcox.test(meta$shannon, meta$Season, p.adjust.method="fdr")

myColors <- ifelse(levels(meta$Season)=="Summer" , "#FA8072", 
                   ifelse(levels(meta$Season)=="Winter", "#20B2AA",
                          "grey90" ) )


#Return the plot area to 1x1
par(mfrow = c(1, 2))
#Plot
boxplot(shannon ~ Season, data=meta, col=myColors, ylab="Shannon's diversity")

boxplot(simpson ~ Season, data=meta, col=myColors, ylab="Simpson's diversity")

