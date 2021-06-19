library(vegan)
genus= read.csv('/home/drkksharma/Desktop/Enz_diversity/MTX_combo.csv', sep='\t', comment=',', head=T, row.names=1)
meta= read.csv('/home/drkksharma/Desktop/MTX_combo_meta.txt', sep='\t', comment='', head=T)
genus <- t(genus)
dim(genus)
genus <- genus[,colMeans(genus > 0) >= .1]
genus = genus[row.names(genus) %in% meta$External_ID,]
dim(genus)
genus
meta = meta[meta$External_ID %in% row.names(genus),]
genus[is.na(genus)] <- 0
dim(meta)
genus[1:10,1:2]
# get Bray-Curtis distances (default for Vegan)
d.bray <- vegdist(genus, na.rm=TRUE)
d.bray[is.na(d.bray)] <- 0
colnames(meta)

#krushal wallis test

kruskal.test(diagnosis ~ Soft.drinks..tea.or.coffee.with.sugar..corn.syrup..maple.syrup..cane.sugar..etc., data = meta)

kruskal.test(diagnosis ~ Diet.soft.drinks..tea.or.coffee.with.sugar..Stevia..Equal..Splenda.etc., data = meta)

kruskal.test(diagnosis ~ Fruit.juice..orange..apple..cranberry..prune.etc.., data = meta)

kruskal.test(diagnosis ~ Water, data = meta)

kruskal.test(diagnosis ~ Alcohol..beer..brandy..spirits..hard.liquor..wine..aperitif..etc.., data = meta)

kruskal.test(diagnosis ~ Yogurt.or.other.foods.containing.active.bacterial.cultures..kefir..sauerkraut., data = meta)

kruskal.test(diagnosis ~ Dairy..milk..cream..ice.cream..cheese..cream.cheese., data = meta)

kruskal.test(diagnosis ~ Probiotic, data = meta)

kruskal.test(diagnosis ~ Fruits..no.juice...Apples..raisins..bananas..oranges..strawberries..blueberries, data = meta)

kruskal.test(diagnosis ~ Vegetables..salad..tomatoes..onions..greens..carrots..peppers..green.beans..etc., data = meta)

kruskal.test(diagnosis ~ Beans..tofu..soy..soy.burgers..lentils..Mexican.beans..lima.beans.etc., data = meta)

kruskal.test(diagnosis ~ Whole.grains..wheat..oats..brown.rice..rye..quinoa..wheat.bread..wheat.pasta., data = meta)

kruskal.test(diagnosis ~ Starch..white.rice..bread..pizza..potatoes..yams..cereals..pancakes..etc.., data = meta)

kruskal.test(diagnosis ~ Eggs, data = meta)

kruskal.test(diagnosis ~ Processed.meat..other.red.or.white.meat.such.as.lunch.meat..ham..salami..bologna, data = meta)

kruskal.test(diagnosis ~ Red.meat..beef..hamburger..pork..lamb., data = meta)

kruskal.test(diagnosis ~ White.meat..chicken..turkey..etc.., data = meta)

kruskal.test(diagnosis ~ Shellfish..shrimp..lobster..scallops..etc.., data = meta)

kruskal.test(diagnosis ~ Fish..fish.nuggets..breaded.fish..fish.cakes..salmon..tuna..etc.., data = meta)

kruskal.test(diagnosis ~ Sweets..pies..jam..chocolate..cake..cookies..etc.., data = meta)

kruskal.test(diagnosis ~ Antibiotics, data = meta)

kruskal.test(diagnosis ~ Chemotherapy, data = meta)

kruskal.test(diagnosis ~ Immunosuppressants..e.g..oral.corticosteroids., data = meta)

#anosim test
#anosim(d.bray, meta$Season, permutations = 1000)
#permanova test
adonis(d.bray ~ diagnosis*Soft.drinks, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Diet.soft.drinks, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Fruit.juice, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Water, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Alcohol, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Yogurt, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Dairy, data = meta, permutations = 1000)

adonis(d.bray ~ Dairy*diagnosis, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Probiotic, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Fruits, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Vegetables, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Beans, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Whole.grains, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Starch, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Eggs, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Processed.meat, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Red.meat, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*White.meat, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Shellfish, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Fish, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Sweets, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Antibiotics, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Chemotherapy, data = meta, permutations = 1000)

adonis(d.bray ~ diagnosis*Immunosuppressants, data = meta, permutations = 1000)


