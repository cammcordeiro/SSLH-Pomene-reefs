### Loading files

#### Fish and benthos

# load packages
library(reshape2)
library(vegan)
library(plyr)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(iNEXT)

#
# setwd("~/Google Drive/PUBLICACOES/POMENE-SSLH/Paper Pomene")

# load files
metadata <- read.xlsx("data/SS_Pomene data all 2019nov12.xlsx", sheet="metadata")
fish1 <- read.xlsx("data/SS_Pomene data all 2019nov12.xlsx", sheet="fish data")
macrofauna <- read.xlsx("data/SS_Pomene data bentos.xlsx", sheet=6)
dados1 <- read.xlsx("data/bentos_transects-quadrados_mfg.xlsx") %>% 
  dplyr::rename(locality = site,
         site = local) # bentos_quadrados_coral.csv usei esse

traits1 <-read.xlsx("~/Google Drive/LECAR/dados/Traits_Gaspar.xlsx", sheet=1)
head(traits1)
traits <- traits1[-1,]
head(traits)

head(macrofauna)

##### Data cleaning and setting

#1) Check NA's and standardize species names

# fish data
cols <- c("locality", "site", "collector", "code", "family","genus","taxa_concat")
fish1[cols] <- lapply(fish1[cols], factor)

fish <- droplevels(subset(fish1, ! is.na(transect_number) & ! is.na(taxa_concat) & ! locality == "Sao Sebastiao"))
fish$eventID <- paste(fish$eventDate,fish$locality,fish$site,sep = '_')
fish$unique_eventID <- paste(fish$eventDate,fish$locality,fish$site,fish$collector,fish$transect_number, sep = '_')

fish$locality <- plyr::mapvalues(fish$locality, from="Baixo Silva", to="Baixo Silvia")
fish$locality <- factor(fish$locality, levels = c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa"))


# fish traits
colst <- c("Genus_and_species", "Diets", "IUCN_status", "Schooling","Level-water","Home-Range")
traits[colst] <- lapply(traits[colst], factor)

#fish1 <- droplevels(subset(fish, ! Species == "0" & ! Quantity == "" & !Site == "Vilankulo"))
#large1$Habitat <- mapvalues(large1$Habitat, from=c("seagrass ","seagrass"), to=c("Seagrass","Seagrass"))
#large1$Area <- mapvalues(large1$Area, from=c("Chingunguene","Mazarete MPA1","Mazarete MPA2"), to=c("Chingonguene","Mazarete MPA","Mazarete MPA"))

#large1$Species <- mapvalues(large1$Species, 
 #                           from=c("Black colonial ascidian"), 
  #                          to=c("Ascidian black"))


# Traits
diff <- setdiff(levels(fish$taxa_concat), levels(traits$Genus_and_species))


# macrofauna

macrofauna <- macrofauna %>% 
  mutate(locality = mapvalues(locality, from = c("baixo_africa", "baixo_silva", "baixo_zambia", "pomene"), 
                              to = c("Baixo Africa", "Baixo Silvia", "Baixo Zambia", "Pomene")))

macrofauna$unique_eventID <- paste(macrofauna$locality,macrofauna$site,macrofauna$collector,macrofauna$transect, sep = '_') #macrofauna$eventDate,

macrofauna$unique_quadratID <- paste(macrofauna$locality,macrofauna$site,macrofauna$collector,macrofauna$transect,macrofauna$image, sep = '_') #macrofauna$eventDate,


# 2) Join tables

#setdiff(levels(fish$transect),levels(macrofauna$transect))

#smallj <- small1[,c("Site", "Place","lat","lon","Date","Habitat","transect","species")]
#names(largej) <- c("Site", "Place","lat","lon","Date","Habitat","transect","species") 

#joined1 <- rbind(smallj, largej)
#str(joined1)
#joined <- droplevels(unique(joined1))

## check valid species

# make a list of species names
#aggregate(abundance ~ taxa_concat, fish, mean) %>%
#  write.csv("~/Google Drive/MOZ/2019/data/fish_name.csv", row.names = FALSE)

# check on worms
teste <- merge(fish, traits[colst], by.x = "taxa_concat", by.y = "Genus_and_species", all.x = TRUE)

# subset(teste, is.na(IUCN_status))['taxa_concat'] %>% unique()
# correct observations
#matched[-4] <- lapply(matched[-4], factor)

#large1$Species <- mapvalues(large1$Species, 
#                           from=c("Black colonial ascidian"), 
#                          to=c("Ascidian black"))


#sort(levels(joined$species), decreasing=F)

#small1$Phylum <- matched$Phylum [ match (small1$species, matched$Species) ]
#large1$Phylum <- matched$Phylum [ match (large1$Species, matched$Species) ]
#joined$target <- matched$target [ match (joined$species, matched$Species) ]


# 3) Convert format of table from long to wide for community analysis

# long to wide for community analysis (presence and absence)

# benthos
dados1$site <- mapvalues(dados1$site, from = c("Baixo Silvia 0", "Baixo Silvia sul", "Baixo Silvia norte",
                                               "Pomene norte", "Pomene sul", "Pomene norte norte",
                                               "Zambia0", "Zambia norte", "Zambia meio",
                                               "Africa Sul", "Norte",  "Baixo Silvia dentro", 
                                               "Baixo Silvia 109", "Pomene 110", "Zambia1",
                                               "Zambia111", "Zambia3", "Afria 112",          
                                               "Afria 2", "Afria 3"), 
                         to = c("baixo_silva_dentro", "baixo_silva_sul", "baixo_silva_norte",
                                "ponta_pomene_norte", "ponta_pomene_sul", "ponta_pomene_meio",
                                "baixo_zambia_sul", "baixo_zambia_norte", "baixo_zambia_meio", 
                                "baixo_africa_sul", "baixo_africa_1", "baixo_silva_dentro",
                                "baixo_silva_109", "pomene_110", "zambia_1",
                                "zambia_2", "zambia_3", "baixo_africa_1",
                                "baixo_africa_2", "baixo_africa_3"))


longb <- dados1 %>% 
  distinct() %>%
  mutate(locality = mapvalues(locality, from = "Zambia", to = "Baixo Zambia"),
         unique_eventID = Image) %>% #paste(locality, site, transecto, quadrat, sep = "_")
  aggregate(Cov..per.species ~ spp.Name + MFG + site + locality + transecto + quadrat + unique_eventID, ., mean) %>% 
  mutate(MFG1 = mapvalues(MFG, from = c("Dead coral", "Recruit", "Red algea", "Echinodermata", "Crustose coralline algae (CCA)", "Ascidian "), 
                          to = c("Coral", "Coral", "Macroalgae", "Other invertebrates", "Turf algae", "Ascidian"))) %>% 
  left_join(aggregate(depth_m ~ site, fish, function(x) max(x)-min(x)))


wideb <- longb %>% 
  dcast(unique_eventID + depth_m + transecto + quadrat + locality ~ spp.Name, value.var="Cov..per.species", sum) 


widemfg <- longb %>% 
  dcast(unique_eventID + depth_m + transecto + quadrat + locality ~ MFG1, value.var="Cov..per.species", sum)


# fish
fish$site <- mapvalues(fish$site, from = "baixo_africa_norte", to = "baixo_africa_3") %>% droplevels()

widef <- aggregate(abundance ~ taxa_concat + unique_eventID + site + locality + depth_m, fish, sum) %>% 
  dcast(unique_eventID + site + locality + depth_m~ taxa_concat, value.var = "abundance", sum)


fish_sel <- fish %>% 
  group_by(taxa_concat) %>% 
  dplyr::summarise(peixe = sum(abundance, na.rm = T)) %>% 
  arrange(-peixe) %>% 
  slice(1:30) %>% 
  pull(taxa_concat)


widef_sel <- widef %>% 
  select(unique_eventID:depth_m, fish_sel)

# macro-invertivores
wide_macro <- macrofauna %>% 
  dcast(data=., unique_eventID + locality + site ~ taxon, value.var = "number", sum) %>% 
  mutate(site = mapvalues(site, from = c("baixo_silva_sul_2", "ponta_pomene_norte_norte", "baixo_zambia", 
                                                 "ponta_pomene_110", "baixo_zambia_111", "africa_112", 
                                         "africa_2", "africa_3", "baixo_africa_norte"), 
                              to = c("baixo_silva_dentro", "ponta_pomene_meio", "baixo_zambia_sul",
                                     "pomene_110", "zambia_2", "baixo_africa_1", "baixo_africa_2", 
                                     "baixo_africa_3", "baixo_africa_3")))

# 4) Check for absent species

# checking absent species
# data.spp <- wide[ rowSums(wide[3:ncol(wide)])!= 0, ]
# data.spp1 <- data.spp[ colSums(data.spp[3:ncol(data.spp)])!= 0 ]



###
# Function to calculate the count per group/overlap
# Note: data is a global variable

# testes <- function(x,y,z,w) {
#   a <- subset(fish, locality == x)[["taxa_concat"]] %>% unique() %>% droplevels()
#   a1 <- length(a)
#   b <- subset(fish, locality == y)[["taxa_concat"]] %>% unique() %>% droplevels()
#   b1 <- length(b)
#   c <- subset(fish, locality == z)[["taxa_concat"]] %>% unique() %>% droplevels()
#   c1 <- length(c)
#   d <- subset(fish, locality == w)[["taxa_concat"]] %>% unique() %>% droplevels()
#   d1 <- length(d)
#   
#   ab <- length(intersect(a,b))
#   ac <- length(intersect(a,c))
#   ad <- length(intersect(a,d))
#   bc <- length(intersect(b,c))
#   bd <- length(intersect(b,d))
#   cd <- length(intersect(c,d))
#   
#   #abc <- length(Reduce(intersect, list(a,b,c)))
#   #abd <- length(Reduce(intersect, list(a,b,d)))
#   #bcd <- length(Reduce(intersect, list(b,c,d)))
#   
#   abc <- length(intersect(intersect(a,b),c))
#   abd <- length(intersect(intersect(a,b),d))
#   acd <- length(intersect(intersect(a,c),d))
#   bcd <- length(intersect(intersect(b,c),d))
#   
#   
#   print(bcd)
#   inter <- data.frame(contrast=c(x,y,z,w,paste(x,y),paste(x,z),paste(x,w),paste(y,z),paste(y,w),paste(z,w),paste(x,y,z),paste(x,y,w),paste(x,w,z),paste(y,w,z)), 
#                       species=c(a1,b1,c1,d1,ab,ac,ad,bc,bd,cd,abc,abd,acd,bcd))
#   print(inter)
# }

rm("fish1", "traits1", "traits", cols, colst, diff)

##########
library(lubridate)

fish %>% 
  select(eventDate, locality, site, transect_number, time_in_24h, depth_m) %>% 
  group_by(locality) %>%
  summarise(med = mean(depth_m),
            sd = sd(depth_m))
  

macrofauna %>% 
  select(eventDate, locality, site, period, transect) %>%
  distinct()

longb %>% 
  group_by(site) %>% 
  summarise(n = n_distinct(unique_eventID))
