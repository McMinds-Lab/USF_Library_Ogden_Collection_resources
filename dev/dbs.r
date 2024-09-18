# full Catalogue of Life dataset available at https://download.catalogueoflife.org/col/annual/2024_coldp.zip
# use here just for extinction and environment annotations - both of which are not necessarily complete
cold <- read.table('~/raw_data/taxonomy/1b3fd8d5-323d-4367-8260-ab0e4c022395/NameUsage.tsv', header = TRUE, sep = '\t', comment.char='', quote='')

# pre-filtered GBIF backbone
bb <- read.table('~/raw_data/taxonomy/gbif_backbone_accepted_living.tsv', header = TRUE, sep = '\t', comment.char='', quote='')

# gbif species counts from manually drawn caribbean (see ./ranges.r)
# use to limit to geographically relevant taxa
gbif_carib <- read.table('~/raw_data/taxonomy/0018230-240906103802322.csv', header = TRUE, sep = '\t', comment.char='', quote='')
gbif_carib <- gbif_carib[order(gbif_carib$numberOfOccurrences, decreasing = TRUE),]
gbif_carib <- gbif_carib[gbif_carib$taxonomicStatus == 'ACCEPTED' & gbif_carib$taxonRank == 'SPECIES',]
gbif_carib <- gbif_carib[gbif_carib$numberOfOccurrences > 10,]

# Get full list of valid and living Caribbean species names by cross-checking with pre-filtered backbone
caribspec <- unique(gbif_carib$species)
caribspec <- caribspec[caribspec %in% bb$canonicalName]

# Limit further to marine taxa
caribspec_mar <- caribspec[grep('marine', c('', cold$col.environment)[match(caribspec, cold$col.scientificName, nomatch=0) + 1])]

filter_tax <- function(targets, tax) {
  # Entries in the tax file that are directly listed in the prompt file
  res <- tax[tax$canonicalName %in% targets,]
  
  # Parent taxa of the directly mentioned ones that are not already in the list
  missinghigher <- tax$taxonID[tax$taxonID %in% res$parentNameUsageID]
  missinghigher <- missinghigher[!missinghigher %in% res$taxonID]
  
  # Child taxa of the directly mentioned ones that are not already in the list
  missinglower <- tax$taxonID[tax$parentNameUsageID %in% res$taxonID]
  missinglower <- missinglower[!missinglower %in% res$taxonID]
  
  # Iterate toward the root to find all ancestors
  if(length(missinghigher) > 0) {
    higherres <- tax[tax$taxonID %in% missinghigher,]
    while(TRUE) {
      res <- rbind(res, higherres)
      
      missinghigher <- tax$taxonID[tax$taxonID %in% higherres$parentNameUsageID]
      missinghigher <- missinghigher[!missinghigher %in% res$taxonID]
      
      if(length(missinghigher) > 0) {
        higherres <- tax[tax$taxonID %in% missinghigher,]
      } else {
        break
      }
    }
  }
  
  # Iterate toward the tips to find all descendants
  if(length(missinglower) > 0) {
    lowerres <- tax[tax$taxonID %in% missinglower,]
    while(TRUE) {
      res <- rbind(res, lowerres) 
      
      missinglower <- tax$taxonID[tax$parentNameUsageID %in% lowerres$taxonID]
      missinglower <- missinglower[!missinglower %in% res$taxonID]
      
      if(length(missinglower) > 0) {
        lowerres <- tax[tax$taxonID %in% missinglower,]
      } else {
        break
      }
    }
  }
  
  return(res)
}

bb_carib <- filter_tax(caribspec, bb)
bb_carib_marine <- filter_tax(caribspec_mar, bb_carib)

scleractinians <- filter_tax('Scleractinia', bb_carib_marine)
scler_spec <- scleractinians[scleractinians$taxonRank == 'species',]
sort(scler_spec$canonicalName)
gbif_carib$species[gbif_carib$species %in% scler_spec$canonicalName] # order of abundance

write.table(scler_spec, '~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation/www/example_corals.tsv', sep='\t', quote=FALSE, row.names=FALSE)

fish <- filter_tax('Chordata', bb_carib_marine) # Actinopterygii doesn't exist in GBIF... So I guess whales and pinnipeds and who knows what else will also be in here
fish_spec <- fish[fish$taxonRank == 'species',]
write.table(fish_spec, '~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation/www/example_fish.tsv', sep='\t', quote=FALSE, row.names=FALSE)

algae <- filter_tax(c('Phaeophyceae','Rhodophyta','Chlorophyta'), bb_carib) # Algae don't have annotations for col.environment in Catalogue of Life
algae_spec <- algae[algae$taxonRank == 'species',]

write.table(algae_spec, '~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation/www/example_algae.tsv', sep='\t', quote=FALSE, row.names=FALSE)

