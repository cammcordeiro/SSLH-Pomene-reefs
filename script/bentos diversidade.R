#######################################################################
# Analises funcionais e diversidade
#######################################################################

# load data and packages
source("script/relatorio data wrangling.R")

#
setdiff(unique(dados1$site), unique(fish$site))
setdiff(unique(fish$site), unique(dados1$site))

#
# wideb1 <- aggregate(Cov..per.species ~ spp.Name + site + local, dados1, mean)
# widef1 <- aggregate(abundance ~ taxa_concat + site + locality, fish, mean)

##
widef2 <- sort(apply(widef[,4:ncol(widef)], 2, sum)) %>% 
  data.frame()
widef2$spp <- row.names(widef2)
names(widef2) <- c("abun", "spp")

sel_spp <- widef2 %>% 
  mutate(prop = abun / sum(abun) * 100) %>% 
  filter(prop > 0.5) %>% 
  distinct(spp) %>% pull()

widef_sel <- widef %>%
  dplyr::select(site, locality, depth_m, all_of(sel_spp))


#
setdiff(
  unique(wideb1$site),
  unique(widef$site)
)

#
# widef_sel <- widef %>%
#   filter(site %in% unique(wideb$site)) %>% 
#   group_by(site) %>%
#   dplyr::select(-site, -unique_eventID, -locality) %>% 
#   summarise_all(.funs=sum) %>% 
#   dplyr::select(site, all_of(sel_spp)) %>% 
#   column_to_rownames('site')

# wideb_sel <- wideb %>%
#   group_by(site) %>%
#   dplyr::select(-site, -unique_eventID, -locality) %>% 
#   summarise_all(.funs=sum) %>% 
#   column_to_rownames('site')
# 


wideb2 <- sort(apply(wideb[,4:81], 2, mean)) %>% 
  data.frame()

wideb2$spp <- row.names(wideb2)
names(wideb2) <- c("abun", "spp")

sel_spp_b <- wideb2 %>% 
  mutate(prop = abun / sum(abun) * 100) %>% 
  filter(prop > 1) %>% 
  distinct(spp) %>% pull()

wideb_sel <- wideb %>%
  dplyr::select(site, locality, depth_m, all_of(sel_spp_b))


###################
### MANTEL TEST ###

distmfg <- vegdist(wideb_sel[,4:11], method = 'bray')
distf <- vegdist(widef_sel[4:ncol(widef_sel)], method = 'bray')
ape::mantel.test(as.matrix(distb), as.matrix(distf), 
                 graph = TRUE, nperm = 4999,
                 xlab = "z-statistic", ylab = "Density",
                 sub = "The vertical line shows the observed z-statistic",
                 main = "Mantel test", las=1)


###########
### RDA ###

rda.densi.subst <- rda(widef_sel ~., wideb_sel)
plot(rda.densi.subst)#, type=c('t'))

vif.cca(rda.densi.subst)
(R2.adj<-RsquareAdj(rda.densi.subst)$adj.r.squared)

anova.cca(rda.densi.subst, step=10000)
anova.cca(rda.densi.subst, step=10000, by="axis")
ordistep(rda.densi.subst, direction="both")

plot(rda.densi.subst, type='n')#, xlim = c(-2, 2.5), ylim = c(-1.5, 1.5), las=1)
points(rda.densi.subst, choices = c(1,2), display = 'sites', pch = 3, cex=0.5, col = widef$local)
points(rda.densi.subst, choices = c(1,2), display = "species", pch= 16, cex = 0.8, col = "red")

envfit(rda.densi.subst, wideb1[, c("Coral", "Macroalgae", "Sponge", "Substrate", "Turf algae")], choices = c(1,2), permutations = 5000) %>% 
  plot (., col="blue")

rda.red <- rda(widef[,5:ncol(widef)] ~., wideb1[,c("Coral", "Macroalgae", "Sponge", "Substrate", "Turf algae")])
plot(rda.red)
anova.cca(rda.red, step=10000)
anova.cca(rda.red, step=10000, by="axis")
ordistep(rda.red, direction="both")
(R2.adj<-RsquareAdj(rda.red)$adj.r.squared)


###############
### CLUSTER ###

#https://www.datanovia.com/en/lessons/comparing-cluster-dendrograms-in-r/
# Compute 2 hierarchical clusterings  
hc1 <- hclust(distb, method = "ward.D2")
hc2 <- hclust(distf, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Create a list to hold dendrograms
library(dendextend)
dend_list <- dendlist(dend1, dend2)

# Align and plot two dendrograms side by side
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()                       # Draw the two dendrograms

# Compute alignment quality. Lower value = good alignment quality
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement()                     # Alignment quality

dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% 
  tanglegram(
    highlight_distinct_edges = FALSE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE # Color common branches 
  )

# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")
# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)


#################
### PERMANOVA ###

## FISH ASSEMBLAGES

# widef_adonis <- widef_sel %>% 
#   rownames_to_column(var = 'site') %>% 
#   right_join(unique(widef[,c('site', 'locality')])) %>% 
#   unique() %>% 
#   dplyr::select(locality, site, depth_m, `Ctenochaetus strigosus`:`Chromis fieldi`)

# test dispersion homogeneity
betadisper(vegdist(widef_sel[, 4:ncol(widef_sel)], method = "bray"), group = widef_sel$locality) %>% 
  anova()

# p>0.05, thus we can proceed with adonis
widef_sel %>% 
  mutate(depth = ifelse(depth_m < 13, "shallow", "deep")) %>% 
  adonis(widef_sel[, 4:39] ~ locality + depth, ., permutations = 4999, strata = widef_sel$site) 


# check species contribution
simper(widef_sel[, 4:39], group = widef_sel$locality) %>% 
   summary()

#
fishes <- mvabund::mvabund(widef_sel[, 4:ncol(widef_sel)])
mvabund::meanvar.plot(fishes, las=1)
plot(fishes ~ widef_sel$locality, cex.axis=0.8, cex=0.8, las=2)
mod1 <- mvabund::manyglm(fishes ~ widef_sel$locality + widef_sel$depth_m, family="negative_binomial")
plot(mod1)
anova(mod1, p.uni="adjusted")
summary(mod1)

# Local, p<0.05 => Lutjanus kasmira, Acanthurus.tennentii, Acanthurus.leucosternon
# Lutjanus.lutjanus

#  
fish %>% 
  filter(taxa_concat %in% c("Lutjanus kasmira", "Lutjanus lutjanus",
                            "Acanthurus leucosternon", "Acanthurus tennentii")) %>% 
  ggplot(aes(y=abundance, x=locality), outlier.shape = "") +
  geom_boxplot(alpha = 0.3, width = 0.4) +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=locality)) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"), 
        strip.text = element_text(size=10, face = "italic"),
        axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12, angle=90, hjust = 1, vjust = 0.5),
        legend.position="") +
  labs(x="", y="") +
  facet_wrap(taxa_concat ~., scales = "free") + 
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) 


###
# wideb_adonis <- wideb_sel %>% 
#   rownames_to_column(var = 'site') %>% 
#   right_join(unique(wideb[,c('site', 'locality')])) %>% 
#   unique() %>% 
#   dplyr::select(locality, site, `Acanthastrea sp`:depth_m)

betadisper(vegdist(wideb[, 4:81], method = "euclidian"), group = wideb$locality) %>% anova()
  TukeyHSD()
adonis(wideb[, 4:81] ~ locality + depth_m, wideb, permutations = 4999, strata = wideb$site)

# asin(sqrt(widemfg[, 4:11]/100))
betadisper(vegdist(asin(sqrt(widemfg[, 4:11]/100)), method = "euclidian"), group = widemfg$locality) %>% 
  TukeyHSD()

betadisper(vegdist(widemfg[, 4:11], method = "euclidian"), group = widemfg$locality) %>% 
  TukeyHSD()
adonis(widemfg[, 4:11] ~ locality + depth_m, widemfg, permutations = 4999, strata = widemfg$site)


#
benthos <- mvabund::mvabund(asin(sqrt(widemfg[, 4:11]/100)))
mvabund::meanvar.plot(benthos, las=1)
mod2 <- mvabund::manylm(benthos ~ wideb$locality + wideb$depth_m)
plot(mod2)
summary(mod2)
anova(mod2, p.uni="adjusted", nBoot = 999)

# locality => Ascidian p<0.05
# depth => Substrate p<0.05

longb %>% 
  filter(MFG1 %in% c("Ascidian ", "Substrate")) %>% 
  ggplot(aes(y=Cov..per.species, x=locality), outlier.shape = "") +
  geom_boxplot(alpha = 0.3, width = 0.4) +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=locality)) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"), 
        strip.text = element_text(size=10, face = "italic"),
        axis.title=element_text(size=12, face="bold"),
        axis.text.x=element_text(size=12, angle=90, hjust = 1, vjust = 0.5),
        legend.position="") +
  labs(x="", y="") +
  facet_wrap(MFG1 ~., scales = "free") + 
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) 



# 
betadisper(vegdist(wide_macro[, 4:29], method = "bray"), group = wide_macro$locality) %>%
  TukeyHSD()
adonis(wide_macro[, 4:29] ~ locality, wide_macro, permutations = 4999, strata = wide_macro$site)


#################
###### NMDS #####


wideb_site <- dados1 %>% 
  mutate(unique_eventID = paste(local, site, transecto, sep = "_")) %>% 
  aggregate(Cov..per.species ~ MFG + site + local + unique_eventID, ., mean) %>% 
  mutate(MFG1 = mapvalues(MFG, from = c("Dead coral", "Recruit", "Red algea", "Echinodermata", "Crustose coralline algae (CCA)"), 
                          to = c("Coral", "Coral", "Macroalgae", "Other invertebrates", "Turf algae")),
         local = mapvalues(local, from = "Zambia", to = "Baixo Zambia")) %>% 
  dcast(unique_eventID + site + local ~ MFG1, value.var="Cov..per.species", sum) %>%
  left_join(aggregate(depth_m ~ site, fish, function(x) max(x)-min(x))) %>% 
  dplyr::rename(locality = local)




######
# BENTHOS
NMDS_b <- metaMDS(widemfg[,4:11], distance = "euclidian", autotransform=F, try = 50)
NMDS_b <- metaMDS(wideb[,4:81], distance = "euclidian", autotransform=F, try = 50)

#
benthos.scores <- as.data.frame(scores(NMDS_b, "species")) %>% 
  tibble::rownames_to_column(., "MFG")

# plot polygons per group
library(ggrepel)

benthos_mds <- data.frame(NMDS_b$points) %>% 
  mutate(site = wideb$locality) %>% 
  group_by(site) %>% 
  dplyr::slice(chull(MDS1, MDS2)) %>% 
  ggplot(., aes(x=MDS1, y=MDS2)) + 
  #geom_point(shape = 21, size = 1, aes(color=site)) +
  geom_polygon(alpha = 0.5, aes(fill = site)) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "") +
  labs(x='MDS 1', y='MDS 2', title = "(A)") +
  geom_point(data = data.frame(NMDS_b$points), aes(x=MDS1, y=MDS2), color = "black", shape = 3, size = 0.5, alpha = 0.3) #+
  # geom_point(data = para.scores, aes(x=NMDS1, y=NMDS2), color = "red", shape = 3) #+
  # geom_label_repel(data = para.scores, aes(x=NMDS1, y=NMDS2, label = MFG), segment.size=0.1,
  #                  size = 2.5, fontface=3, min.segment.length = 0, seed = 13, box.padding = 0.5) 

#### MACROFAUNA

# species scores
NMDS_m <- metaMDS(wide_macro[,4:29], distance = "bray", autotransform=T, try = 50)

macro.scores <- as.data.frame(scores(NMDS_m, "species")) %>% 
  tibble::rownames_to_column(., "spp")

# plot polygons per group
macro_mds <- data.frame(NMDS_m$points) %>% 
  mutate(site = wide_macro$locality) %>% 
  group_by(site) %>% 
  dplyr::slice(chull(MDS1, MDS2)) %>% 
  ggplot(., aes(x=MDS1, y=MDS2)) + 
  #geom_point(shape = 21, size = 1, aes(color=site)) +
  geom_polygon(alpha = 0.5, aes(fill = site)) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "") +
  labs(x='NMDS 1', y='NMDS 2', title = "(B)") +
  geom_point(data = data.frame(NMDS_m$points), aes(x=MDS1, y=MDS2), color = "black", shape = 3, size = 0.5, alpha = 0.3) #+
  #geom_point(data = macro.scores, aes(x=NMDS1, y=NMDS2), color = "red", shape = 3) +
# geom_label_repel(data = macro.scores, aes(x=NMDS1, y=NMDS2, label = spp), segment.size=0.1,
#                  size = 2.5, fontface=3, min.segment.length = 0, seed = 13, box.padding = 0.5) 


#### FISH
NMDS <- metaMDS(widef_sel[,4:40], distance = "bray", autotransform=T, try = 50)

# species scores
fish.scores <- as.data.frame(scores(NMDS, "species")) %>% 
  tibble::rownames_to_column(., "taxa_concat") %>% 
  inner_join(
    unique(teste[,c("taxa_concat", "Diets")]) %>% 
      filter(!is.na(Diets)) %>% 
      rbind(data.frame(taxa_concat = c("Acanthurus sp", "Bodianus sp", "Caranx sp",
                                       "Coris sp", "Gobiidae sp", "Gymnothorax sp",
                                       "Halichoeres sp", "Kyphosus sp", "Macropharyngodon sp",
                                       "Scorpaenopsis sp", "Synodus sp"),
                       Diets = c("HD", "IM", "FC", "IM", "IM", "FC", "IM", "HD", "IM", "FC", "FC")))
  ) 


# plot polygons per group
fish_mds <- data.frame(NMDS$points) %>% 
  mutate(site = as.character(widef$locality)) %>% 
  group_by(site) %>% 
  dplyr::slice(chull(MDS1, MDS2)) %>% 
  ggplot(., aes(x=MDS1, y=MDS2)) + 
  #geom_point(shape = 21, size = 1, aes(color=site)) +
  geom_polygon(alpha = 0.5, aes(fill = site)) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "") +
  labs(x='NMDS 1', y='NMDS 2', title = "(C)") +
  #geom_point(data = fish.scores, aes(x=NMDS1, y=NMDS2), color = "red", shape = 3)# +
  geom_point(data = data.frame(NMDS$points), aes(x=MDS1, y=MDS2), shape = 3, size = 0.5, alpha = 0.3)
  
  # geom_label_repel(data = fish.scores, aes(x=NMDS1, y=NMDS2, label = spp), segment.size=0.1,
  #                  size = 2.5, fontface=3, min.segment.length = 0, seed = 13, box.padding = 0.5) 


#########
library(patchwork)

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legenda_5 <- get_legend(data.frame(NMDS$points) %>% 
                        mutate(site = as.character(widef$locality)) %>% 
                        group_by(site) %>% 
                        dplyr::slice(chull(MDS1, MDS2)) %>% 
                        ggplot(., aes(x=MDS1, y=MDS2)) + 
                        geom_point(shape = 21, size = 1, aes(color=site)) +
                        geom_polygon(alpha = 0.5, aes(fill = site)) +
                        theme_classic() + 
                        theme(legend.title = element_blank(),
                              legend.position = "right") +
                        labs(x='MDS 1', y='MDS 2', title = "(C)"))


## Plot
benthos_mds + macro_mds + fish_mds + legenda_5 + plot_layout(ncol = 2)#, heights = c(2.3, 2.3, 2.3, 0.8))
