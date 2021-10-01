# packages
require(tidyverse) # summarises data

### https://stackoverflow.com/questions/23169645/r-3-0-3-rbind-multiple-csv-files

mydir <- c("~/github/SSLH-Pomene-reefs/output/todos/")
files <- list.files(path = mydir, pattern = '\\.csv') # read all spreadsheets from photoQuad (within working folder)
dados <- plyr::adply(paste0(mydir, files), 1, read.csv) # stack all spreadsheets

### select columns
dadosA <- dados %>% 
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
dadosA <- dadosA %>% 
  left_join(codes %>% 
              select(spp.ID, MFG))

dadosA %>% 
  filter(is.na(MFG)) %>% 
  distinct(spp.Name)

setdiff(dadosA$spp.Name, codes$spp.Name)
setdiff(codes$spp.Name, dadosA$spp.Name)

#
dadosB <- dadosA %>% 
  mutate(Image = gsub('\\.JPG','', Image),
         quadrat = str_sub(Image, - 3, - 1) %>% 
           gsub('\\_','',.))

# ###
# #dados2 <- separate(dados1, Path, into=c("nada","nada1","nada2","nada3","nada4","nada5","nada6","transecto","nada7"), extra="merge")
# levels(as.factor(dados1$site))
# gsub("\\", "", dados1$site, fixed=TRUE)
# tolower(dados1$site)

### write file
openxlsx::write.xlsx(dadosB, "data/bentos_transects-quadrados_mfg.xlsx")
