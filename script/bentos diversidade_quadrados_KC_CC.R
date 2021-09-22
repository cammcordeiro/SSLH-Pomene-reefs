#######################################################################
# Analises de diversidade
#######################################################################

# load packages
library(vegan)
library(FD)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(openxlsx)

# load data
dados1 <- read.csv("/Users/katia/Documents/Moçambique/Resultados/juntos/bentos_quadrados.csv", header = T)
dados1 <- read.csv("/Users/cesarcordeiro/Google Drive/MOZ/2019 WIORI SS_Pomene/relatorio/KC/bentos_quadrados.csv", header = T) # bentos_quadrados_coral.csv usei esse
dados1 <- read.csv("/Users/katia/Documents/Moçambique/Resultados/juntos/bentos_quadrados_coral.csv", header = T)

head(dados1)
str(dados1)

# ajustar niveis site
dados1$local <- revalue(dados1$local, c("Zambia" = "Baixo Zambia"))
                                      
dados1$spp.Name <- revalue(dados1$spp.Name, c("ascidias nao identificadas" = "unidentified ascidians"))
#                                        

# long to wide
wide <- dcast(dados1, spp.Name + local + site + transecto ~ MFG, value.var="Cov..per.species", sum)
head(wide)

#checar se tem contagem com mais de 100%
wide1 <- subset(wide, ! rowSums(wide[,5:ncol(wide)]) > 1000)
head(wide1)

# absent species / empty samples
wide_all <- wide1[ rowSums(wide1[5:ncol(wide1)])!=0, ]
colSums(wide_all[,5:ncol(wide_all)])


#############################################
#############################################

# # # dominant species
# # histogram
# most freq species (http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html)
sort(apply(wide_all[,5:ncol(wide_all)], 2, sum)) %>% barplot(las=2) # abundance
sort(apply(wide_all[,5:ncol(wide_all)]>0, 2, sum)) %>% barplot(las=2) # frequency

#tentativa de gerar um padrão de cores aleatorio
#library(randomcoloR)
distinctColorPalette(k = 17)
#colors_kc

#############################################
#############################################

#grafico de barra proporcional
ggplot(dados1, aes(x=local, y=Cov..per.species, fill=MFG)) +
  geom_bar(stat="identity", position="fill", width = 0.6) +
  scale_fill_manual(values = c ("#7AA5D3", "#77D7DD", "#DFB24E", "#CDE74E", "#D5E494", "#DE53D6", 
                                "#6DE464", "#D7A38F", "#7C46DB", "#70E4AD", "#E15B79", "#D2E5CC",
                                "#8D82E1", "#D5C9DD", "#DF94D6", "#945B85", "#7EA274")) + # scale_fill_grey() -> for grey scale
  theme(panel.grid.major = element_blank(), # below, defining aesthetics features
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x=element_text(size=12, angle=90, vjust=0.5, hjust=1),
        axis.text.y=element_text(size=12, angle=90, vjust=0.5, hjust=0.5),
        axis.text=element_text(size=12), 
        strip.text = element_text(), #face = "italic"
        axis.title=element_text(size=12,face="bold"),
        legend.position="right") +
  labs(x="", y="Abundance (%)", fill="") +
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) #+

#############################################
#############################################

#porcentagem de cobertura média por espécie por local
species_coverage_meanSD <- cbind(aggregate(Cov..per.species ~ spp.Name + local, dados1, mean), aggregate(Cov..per.species ~ spp.Name + local, dados1, sd)
[,3])
colnames(species_coverage_meanSD) <- c('spp.Name','local','mean_cover','sd')

group_coverage_meanSD <- cbind(aggregate(Cov..per.species ~ MFG + local, dados1, mean), aggregate(Cov..per.species ~ MFG + local, dados1, sd)
                                 [,3])
colnames(group_coverage_meanSD) <- c('group','local','mean_cover','sd')

#Soma da cobertura 
group_coverage_soma <- cbind(aggregate(Cov..per.species ~ MFG + local, dados1, sum))
colnames(group_coverage_soma) <- c('group','local','soma')
write.xlsx(group_coverage_soma, "/Users/katia/Documents/Moçambique/Resultados/juntos/group_coverage_soma.xlsx")




#plotar a % de cobertura média por grupo; um gráfico para cada local
groups_coverage <- 
  dados1 %>% 
  filter(MFG != "Substrate") %>% 
  mutate(MFG = mapvalues(MFG, from = c("Red algea","Crustose coralline algae (CCA)"), to = c("Red algae", "CCA"))) %>% 
  aggregate(Cov..per.species ~ MFG + local + site + transecto, ., mean) %>%
  ggplot(aes(x=MFG, y=Cov..per.species, alpha=0.6), outlier.shape = "") + 
  geom_jitter(width = 0.1, alpha=0.6) + # , aes(color=local)
  geom_boxplot(aes(fill=local, color=local), width = 0.5) +
  theme_classic() + 
  theme(legend.position = "", 
        axis.text.y=element_text(size=12, angle=0, vjust=0.5, hjust=0.5),
        axis.title=element_text(size=12,face="bold"),
        axis.text.x=element_text(size=14, angle=45, vjust=1, hjust=1),
        axis.text=element_text(size=14)) +
  labs(x="", y="Relative cover(%)") +
  facet_grid(vars(local)) +
  scale_x_discrete(limits=c("Turf algae","Plate coral","Sponge",
                            "Macroalgae", "Red algae","Octocoral", "Encrusting coral", 
                            "Massive coral","Branching coral", "Other invertebrates",
                            "Solitary coral", "Ascidian ", "Echinodermata",
                            "CCA", "Recruit", "Dead coral"))

groups_coverage


dados1 %>% 
  filter(MFG != "Substrate") %>% 
  mutate(MFG = mapvalues(MFG, from = c("Red algea","Crustose coralline algae (CCA)"), to = c("Red algae", "CCA"))) %>% 
  aggregate(Cov..per.species ~ MFG, ., range) #function(x) cbind(mean(x), sd(x)) 

#### MELHOR USAR O MODELO ANTERIOR
#plotar a % de cobertura média por local; um gráfico para cada grupo
#CESAR, AQUI FALTA TIRAR O CONTORNO DO NOME DOS GRUPOS E INCLUIR UM CONTORNO EM CADA GREFICO
#SE SOUBER FAZER ISSO E QUISER INCLUIR, EU AGRADECO HAHAHA
groups_coverage_b <- aggregate(Cov..per.species ~ MFG + local + site + transecto, dados1, mean) %>%
  ggplot(aes(x=local, y=Cov..per.species), outlier.shape = "") + 
  geom_boxplot(aes(fill=local, color=local)) +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=local)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=0.5, color="black") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), size=0.5, color="black") +
  theme_classic() + 
  theme(legend.position = "", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  labs(x="", y="") +
  #facet_grid(cols = vars(MFG)) +
  facet_wrap(~ MFG, ncol=4) +
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) #+
  #panel.border = element_rect(colour = "black", size=1)
#ylim(1)
groups_coverage_b 


# # # diversity indexes
richness <- aggregate(spp.Name ~ local + site + transecto, dados1, function(x) length(unique(x)))
richness
richness <- aggregate(spp.Name ~ local + site + transecto, dados1, function(x) length(unique(x))) %>%
ggplot(aes(x=local, y=spp.Name), outlier.shape = "") + #geom_boxplot() +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=local)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=0.5, color="black") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
  theme_classic() + 
  theme(legend.position = "", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  labs(x="", y="Species richness") + #+ facet_grid(tideHeight~locality) 
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) #+
  #ylim(1)

richness


#
wide2 <- dcast(dados1, local + site + transecto ~ spp.Name, value.var="Cov..per.species", sum)
head(wide)

Simpson_quadrat_data <-  data.frame(wide2[,1:3], simpson=diversity(wide2[,4:81], index='simpson'))
Simpson_quadrat_data
Simpson_quadrat <-  data.frame(wide2[,1:3], simpson=diversity(wide2[,4:81], index='simpson')) %>%
ggplot(aes(x=local, y=simpson), outlier.shape = "") + #geom_boxplot() +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=local)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=0.5, color="black") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
  theme_classic() + 
  theme(legend.position = "", axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1)) +
  labs(x="", y="Simpson index") + #+ facet_grid(transect~local)
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) #+
  #ylim(0.67,0.92)

Simpson_quadrat


#
# shannon <- 
# ggplot(subset(diversidade, habitat == 'meso'), aes(x=season, y=Shannon), outlier.shape = "") + #geom_boxplot() +
#  geom_jitter(width = 0.1, alpha=0.5, aes(color=locality)) +
#  stat_summary(fun.y=mean, geom="point", shape=18, size=0.5, color="black") +
#  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
#  theme_classic() + 
#  theme(legend.position = "", axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1)) +
#  labs(x="", y="Shannon index") + facet_grid(tideHeight~locality) 

#
eveness_data <- data.frame(wide2[,1:3], eveness=diversity(wide2[,4:81])/log(specnumber(wide2[,4:81])))
eveness_data
eveness <- data.frame(wide2[,1:3], eveness=diversity(wide2[,4:81])/log(specnumber(wide2[,4:81]))) %>%
ggplot(aes(x=local, y=eveness), outlier.shape = "") + #geom_boxplot() +
  geom_jitter(width = 0.1, alpha=0.5, aes(color=local)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=0.5, color="black") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
  theme_classic() + 
  theme(legend.position = "", axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1)) +
  labs(x="", y="Eveness (J')") + 
  scale_x_discrete(limits=c("Baixo Silvia","Pomene","Baixo Zambia","Baixo Africa")) #+
  #ylim(0.5,1) #+ facet_grid(tideHeight~locality) 

eveness


library(patchwork)
(richness | Simpson_quadrat | eveness)

#############################################
#############################################
## ANOVAS + permanova

library(lmerTest)
# richness
rich <- aggregate(spp.Name ~ local + site + transecto, dados1, function(x) length(unique(x)))
mod_rich <- lmer(spp.Name ~ local + (1|local:site), rich)
anova(mod_rich)
#lmer(spp.Name ~ local + (1|local:site), rich) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_rich) 

# simpson
simp <- data.frame(wide2[,1:3], simpson=diversity(wide2[,4:81], index='simpson'))
mod_simp <- lmer(simpson ~ local + (1|local:site), simp) 
anova(mod_simp)
difflsmeans(mod_simp)
#lmer(simpson ~ local + (1|local:site), simp) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_simp) 


# evenness
even <- data.frame(wide2[,1:3], eveness=diversity(wide2[,4:81])/log(specnumber(wide2[,4:81])))
mod_even <- lmer(eveness ~ local + (1|local:site), even)
anova(mod_even)
difflsmeans(mod_even)
#lmer(eveness ~ local + (1|local:site), even) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_even) 

# permanova
adonis(df[,5:ncol(df)] ~ local, data=df, method="bray", permutations = 4999)


#############################################
#############################################
## rarefação

###
library(iNEXT)

matrizPA <- aggregate(Cov..per.species ~ local + MFG + spp.ID, dados1, sum) %>%
  dcast(MFG ~ local, value.var = "Cov..per.species", sum)
rownames(matrizPA) <- matrizPA[,1]

iNEXT(matrizPA[,-1], q=0, datatype="abundance") %>%
  ggiNEXT(se=TRUE, facet.var="none", color.var="site", grey=FALSE) + 
  theme_classic() + labs(y="Species richness")


#############################################
#############################################
## similaridades
wideS <- dcast(dados1, local + site + transecto ~ MFG, value.var="Cov..per.species", mean)
wideS[is.na(wideS)] <- 0
df1 <- wideS

sort(apply(df1[,6:ncol(df1)], 2, sum))
df1[ rowSums(df1[6:ncol(df1)])!=0, ] %>% dim()

df <- df1 %>% select_if(~ !is.numeric(.) || sum(.) != 0)
which(is.na(df))

#nMDS
example_NMDS = metaMDS(df[,4:ncol(df)], distance = "bray", autotransform=T)
plot (example_NMDS, display = 'sites', type = 'n', las=1) 
points (example_NMDS, display = 'sites', pch=3, cex=0.5, col="red")
ordihull(example_NMDS, draw="polygon", lty = 2, groups = df$local, col = c("red","green","blue","yellow"), label = F, cex=0.6)
text (example_NMDS, display = 'species', cex=0.8, col="black") #cex = goodness (example_NMDS)*100
#ordispider(example_NMDS, groups = df$local, col = c("red","green","blue","yellow"), label = F, cex=0.6)
legend(0.6,0.2, legend = levels(df$local), cex=0.6, bty = "n", col = c("red","green","blue","yellow"), pch = 21, pt.bg = c("red","green","blue","yellow"))


# cluster
widess <- dcast(dados1, local ~ MFG, value.var="Cov..per.species", mean)
widess[is.na(widess)] <- 0
row.names(widess) <- widess[,1]

dissi <- vegan::vegdist(widess[,-1], method="bray")
dendro <- hclust(dissi, method="ward.D")

par(mar = c(5,1,1,7))
plot(as.dendrogram(dendro), horiz=T, xlab="Dissimilarity")



############################################
#############################################
## ANOVAS + permanova

library(lmerTest)
library(sjPlot)
library(glmmTMB)

# richness
rich <- aggregate(spp.Name ~ local + site + transecto, dados1, function(x) length(unique(x)))
mod_rich <- lmer(spp.Name ~ local + (1|local:site), rich)
anova(mod_rich)
#lmer(spp.Name ~ local + (1|local:site), rich) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_rich) 

# simpson
simp <- data.frame(wide2[,1:3], simpson=diversity(wide2[,4:81], index='simpson'))
mod_simp <- lmer(simpson ~ local + (1|local:site), simp) 
anova(mod_simp)
difflsmeans(mod_simp)
#lmer(simpson ~ local + (1|local:site), simp) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_simp) 

# evenness
even <- data.frame(wide2[,1:3], eveness=diversity(wide2[,4:81])/log(specnumber(wide2[,4:81])))
mod_even <- lmer(eveness ~ local + (1|local:site), even)
anova(mod_even)
difflsmeans(mod_even)
#lmer(eveness ~ local + (1|local:site), even) %>% sjPlot::plot_model(type = 're') 
# MuMIn::r.squaredGLMM(mod_even) 

# permanova
adonis(df[,5:ncol(df)] ~ local, data=df, method="bray", permutations = 4999)
