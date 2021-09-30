# packages
require(tidyverse) # summarises data

### https://stackoverflow.com/questions/23169645/r-3-0-3-rbind-multiple-csv-files

mydir <- c("~/github/SSLH-Pomene-reefs/output/todos/")
files <- list.files(path = mydir, pattern = '\\.csv') # read all spreadsheets from photoQuad (within working folder)
dados <- plyr::adply(paste0(mydir, files), 1, read.csv) # stack all spreadsheets

### select columns
dados1 <- dados %>% 
  select(Image, site, local, transecto, spp.Name, Cov..per.species, spp.ID)

### load cover codes and species
codes <- readxl::read_xlsx("~/Google Drive/PARCERIAS/MOZ/2019 WIORI SS_Pomene/relatorio/KC/photoquad_katia.xlsx") 

codes <- codes %>% 
  mutate(spp.Name = codes %>% 
           pull(Species.name) %>% 
           gsub("\\.", "", ., perl=TRUE)) %>%
  dplyr::rename(MFG = Group.name,
                spp.ID = Species.ID)

### add morpho-functional categories matching species name 
dados1 <- dados1 %>% 
  left_join(codes %>% 
              select(spp.ID, MFG))

dados1 %>% 
  filter(is.na(MFG)) %>% 
  distinct(spp.Name)

setdiff(dados1$spp.Name, codes$spp.Name)
setdiff(codes$spp.Name, dados1$spp.Name)

dados1$Image <- gsub('\\.JPG','', dados1$Image)

dados2 <- data.frame(do.call('rbind', strsplit(as.character(dados1$Image), '_', fixed=TRUE))) %>% 
  select(X4, X6) %>% 
  rename(sitio = X4,
         quadrat = X6) %>% 
  bind_cols(dados1)
dados2 <- cbind(Image, dados1)


# ###
# #dados2 <- separate(dados1, Path, into=c("nada","nada1","nada2","nada3","nada4","nada5","nada6","transecto","nada7"), extra="merge")
# levels(as.factor(dados1$site))
# gsub("\\", "", dados1$site, fixed=TRUE)
# tolower(dados1$site)

### write file
openxlsx::write.xlsx(dados1, "data/bentos_quadrados_mfg.xlsx")
openxlsx::write.xlsx(dados2, "data/bentos_transects-quadrados_mfg.xlsx")


wideb <- dados2 %>% 
  select(-local, -spp.ID, -sitio, -Image, -MFG) %>% 
  pivot_wider(values_from = Cov..per.species, names_from = spp.Name, values_fn = mean, values_fill = 0) %>% 
  left_join(aggregate(depth_m ~ site, fish, function(x) max(x)-min(x))) 
  
openxlsx::write.xlsx(wideb, "data/bentos_wide.xlsx")

  
  # mutate(unique_eventID = paste(local, site, transecto, sep = "_")) %>% 
  aggregate(Cov..per.species ~ spp.Name + MFG + site + transecto + Image, ., mean) %>% 
  mutate(MFG1 = mapvalues(MFG, from = c("Dead coral", "Recruit", "Red algea", "Echinodermata", "Crustose coralline algae (CCA)"), 
                          to = c("Coral", "Coral", "Macroalgae", "Other invertebrates", "Turf algae"))) %>%
         # local = mapvalues(local, from = "Zambia", to = "Baixo Zambia"))  
  reshape2::dcast(Image + site + local ~ spp.Name, value.var="Cov..per.species", sum) %>%
  left_join(aggregate(depth_m ~ site, fish, function(x) max(x)-min(x))) %>% 
  dplyr::rename(locality = local)

#
vegan::adonis(wideb[, 4:81] ~ site + depth_m, wideb, permutations = 9, strata = wideb$transecto)

dados1 %>% 
  group_by(site) %>% 
  dplyr::summarise(n = n_distinct(Image))

# ver numero de figs