# https://brainchronicle.blogspot.com/2012/12/computing-empirical-pfdr-in-r.html#more
######L I B R A R Y #####
library(tidyverse)
library(zoo)
library(rstatix)
library(ggpubr)
library(knitr)
library(DescTools)
library(ggseg)
library(reshape2)
library(ggsegGlasser)
library(foreach)
library(readxl)

####### L O A D I N G   D A T A ########
############### loading data
KEY<-data.frame(read.csv("E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/Key.csv"))
test<-KEY %>% slice(1:34) %>% mutate(HAMD=na.approx(HAMD))
index_KEY<-which(test$HAMD<17)

HAMD<-KEY %>% slice(1:34
)%>% mutate(HAMD=na.approx(HAMD)
)%>% mutate(MED=MEDICATION
)%>%select(X,HAMD,MED)

# load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GS_TOPO_ALL_SUBJ.Rdata")
# GS_L <- as.data.frame(t(Zrval)
#       )%>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
#       #)%>% mutate(SUBREG=c(rep("INT",11),rep("EXT",15),rep("MENT",12))
#       )%>% mutate(id=c(1:34)
#       )%>% filter(id %in% index_KEY)
# load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/R_GLASSER_FDR_GS_TOPO_ALL_SUBJ.Rdata")
# GS_R <- as.data.frame(t(Zrval)
#       )%>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
#       #)%>% mutate(SUBREG=c(rep("INT",11),rep("EXT",15),rep("MENT",12))
#       )%>% mutate(id=c(1:34)
#       )%>% filter(id %in% index_KEY)

# GS - B A N D P A S S E D  D A T A 
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GS_TOPO_ALL_SUBJ.Rdata")
GS_L <- as.data.frame(t(Zrval))
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/R_GLASSER_FDR_GS_TOPO_ALL_SUBJ.Rdata")
GS_R <- as.data.frame(t(Zrval))
colnames(GS_R) <- paste(colnames(GS_R),"_Ri_",sep="")
GS <- cbind(GS_L,GS_R)
GS <- GS %>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
)%>% mutate(id=c(1:34)
)%>% filter(id %in% index_KEY)

# load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GSR_TOPO_ALL_SUBJ.Rdata")
# GSR <- as.data.frame(t(Zrval)
#       )%>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
#       #)%>% mutate(SUBREG=c(rep("INT",11),rep("EXT",15),rep("MENT",12))
#       )%>% mutate(id=c(1:34)
#       )%>% filter(id %in% index_KEY)

# GSR - B A N D P A S S E D  D A T A 
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GSR_TOPO_ALL_SUBJ.Rdata")
GSR_L <- as.data.frame(t(Zrval))
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/R_GLASSER_FDR_GSR_TOPO_ALL_SUBJ.Rdata")
GSR_R <- as.data.frame(t(Zrval))
colnames(GSR_R) <- paste(colnames(GSR_R),"_Ri_",sep="")
GSR <- cbind(GSR_L,GSR_R)
GSR <- GSR %>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
)%>% mutate(id=c(1:34)
)%>% filter(id %in% index_KEY)



# load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GS_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
# GS_HP <- as.data.frame(t(Zrval)
#       )%>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
#       #)%>% mutate(SUBREG=c(rep("INT",11),rep("EXT",15),rep("MENT",12))
#       )%>% mutate(id=c(1:34)
#       )%>% filter(id %in% index_KEY)

# GS - H I G H P A S S E D  D A T A 
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GS_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
GS_HP_L <- as.data.frame(t(Zrval))
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/R_GLASSER_FDR_GS_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
GS_HP_R <- as.data.frame(t(Zrval))
colnames(GS_HP_R) <- paste(colnames(GS_HP_R),"_Ri_",sep="")
GS_HP <- cbind(GS_HP_L,GS_HP_R)
GS_HP <- GS_HP %>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
)%>% mutate(id=c(1:34)
)%>% filter(id %in% index_KEY)

# load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GSR_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
# GSR_HP <- as.data.frame(t(Zrval)
#       )%>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
#       #)%>% mutate(SUBREG=c(rep("INT",11),rep("EXT",15),rep("MENT",12))
#       )%>% mutate(id=c(1:34)
#       )%>% filter(id %in% index_KEY)

# GSR - H I G H P A S S E D  D A T A

load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/GLASSER_FDR_GSR_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
GSR_HP_L <- as.data.frame(t(Zrval))
load(file = "E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/DATA/R_GLASSER_FDR_GSR_HIGHPASS_TOPO_ALL_SUBJ.Rdata")
GSR_HP_R <- as.data.frame(t(Zrval))
colnames(GSR_HP_R) <- paste(colnames(GSR_HP_R),"_Ri_",sep="")
GSR_HP <- cbind(GSR_HP_L,GSR_HP_R)
GSR_HP <- GSR_HP %>% mutate(GROUP=c(rep("DM",20),rep("CM",14))
)%>% mutate(id=c(1:34)
)%>% filter(id %in% index_KEY)



#######----G S  P R E S E N T - B A N D P A S S E D########
##------Regions that has greater GSCORR in dep than cont 
e=GS[,1:360] #Removing group and id
classLabel=GS$GROUP

## COMPUTING P-VALUE DISTRIBUTION

# Important NOTE : When one tries with toy data,
# alternative looks If first ... than second. Below
# investigates Regions that have greater in DM than CM
p <- apply(e, 2, function(x){wilcox.test(as.numeric(x[1:17]),
                                         as.numeric(x[18:31]),
                                         alternative = "greater")$p.value})

## COMPUTING THE RANDOM P-VALUE DISTRIBUTION
# How many random sampling
R=100
# Shuffling the sample labels and recomputing the p-value each time
p.rand <- foreach(i = 1:dim(e)[2], .combine=rbind) %do% {
  pval <- vector(mode='numeric', length=R)
  for(j in 1:R){
    randomOrder <- sample(1:length(classLabel))
    pval[j] <- wilcox.test(as.numeric(e[randomOrder[1:17],i]),
                           as.numeric(e[randomOrder[18:31],i]),
                           alternative = "greater")$p.value
  }
  pval
}

## COMPUTING Q-VALUES
qval <- foreach(i = 1:length(p), .combine=c) %do% {
  # how many p-value are lower among the random matrix (lower is better)
  length(which(p.rand <= p[i])) / R
}
qval <- qval/length(qval)

# Finds significant regions by with numbers
NAME=colnames(GS)  
LEFT <- NAME[1:180]
RIGHT <- NAME[181:360]
lsig=qval[1:180]
rsig=qval[181:360]
R_RIG <- RIGHT[rsig<=0.05]
L_RIG <- LEFT[lsig<=0.05]

test <- read_excel("C:/Users/kaan/Downloads/Glasser_2016_Table.xlsx")
REGION_INDEX <- test$...2[2:181]
test <- str_extract_all(R_RIG,"\\d+")
R_LOC <- matrix(unlist(test))
test <- str_extract_all(L_RIG,"\\d+")
L_LOC <- matrix(unlist(test))


# Significant regions in right hemis
REGION_INDEX[as.integer(R_LOC)]
# Color Scheme according to region
COLOR_REG=c(1:180)
COLOR_REG[as.integer(R_LOC)]
# Visualization
library(ggseg)
library(ggsegGlasser)
someData = tibble(
  #First right than left hemisphere
  region =c(REGION_INDEX[as.integer(R_LOC)],REGION_INDEX[as.integer(L_LOC)]),
  # Color for regions
  Region = c(COLOR_REG[as.integer(R_LOC)],COLOR_REG[as.integer(L_LOC)]),
  HEMIS = c(rep("R", length(REGION_INDEX[as.integer(R_LOC)])), 
             rep("L", length(REGION_INDEX[as.integer(L_LOC)])))

)

someData$Region <- as.factor(someData$Region)
someData$region <- as.factor(someData$region)


# FIGURE SOLUTION: https://github.com/ggseg/ggseg/issues/50
my_data = data.frame(
  region = someData$region,
  region_to_fill = someData$region,
  HEMIS=someData$HEMIS,
  stringsAsFactors = FALSE
)



GS_BRAIN <-my_data %>% 
  group_by(HEMIS)%>%
  ggplot()+geom_brain(atlas = glasser,
                      position=position_brain(hemi ~ side),
                      mapping=aes(fill=region_to_fill))+
  scale_fill_brain2(palette=glasser$palette) +
  theme_classic()+
  facet_wrap(~HEMIS)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "GS Presented", 
       subtitle = "0.01 - 0.1 Bandpassed")+
  guides(fill=guide_legend(ncol=12))+
  labs(fill="Regions")


# OLD FIGURE 
# GS_BRAIN <- someData %>%
# group_by(HEMIS)%>%
# ggplot()+geom_brain(atlas=glasser,
#                             show.legend=TRUE,
#                             position = position_brain(hemi ~ side),
#                             aes(fill = region))+
#   facet_wrap(~HEMIS) +
#   # scale_fill_viridis_d(option = "viridis",
#   #                       direction = -1,
#   #                      begin = 0,
#   #                      end = 0.5) +
#   # theme_void() +
#   scale_fill_manual(values = glasser$palette)+
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 10))+
#   labs(title = "GS Presented", 
#        subtitle = "0.01 - 0.1 Bandpassed")+
#   scale_fill_discrete(labels=someData$region)+
#   guides(fill=guide_legend(ncol=12))


ggsave("E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/GS_BRAIN.png",
       GS_BRAIN , 
       width = 1920/150, 
       height = 1080/150, 
       dpi = 150)

##------Regions that has less GSCORR in dep than cont 

e=GS[,1:360] #Removing group and id
classLabel=GS$GROUP

## COMPUTING P-VALUE DISTRIBUTION

# Important NOTE : When one tries with toy data,
# alternative looks If first ... than second. Below
# investigates Regions that have greater in DM than CM
p <- apply(e, 2, function(x){wilcox.test(as.numeric(x[1:17]),
                                         as.numeric(x[18:31]),
                                         alternative = "less")$p.value})

## COMPUTING THE RANDOM P-VALUE DISTRIBUTION
# How many random sampling
R=100
# Shuffling the sample labels and recomputing the p-value each time
p.rand <- foreach(i = 1:dim(e)[2], .combine=rbind) %do% {
  pval <- vector(mode='numeric', length=R)
  for(j in 1:R){
    randomOrder <- sample(1:length(classLabel))
    pval[j] <- wilcox.test(as.numeric(e[randomOrder[1:17],i]),
                           as.numeric(e[randomOrder[18:31],i]),
                           alternative = "less")$p.value
  }
  pval
}

## COMPUTING Q-VALUES
qval <- foreach(i = 1:length(p), .combine=c) %do% {
  # how many p-value are lower among the random matrix (lower is better)
  length(which(p.rand <= p[i])) / R
}
qval <- qval/length(qval)

# Finds significant regions by with numbers
NAME=colnames(GS)  
LEFT <- NAME[1:180]
RIGHT <- NAME[181:360]
lsig=qval[1:180]
rsig=qval[181:360]
R_RIG <- RIGHT[rsig<=0.05]
L_RIG <- LEFT[lsig<=0.05]

test <- read_excel("C:/Users/kaan/Downloads/Glasser_2016_Table.xlsx")
REGION_INDEX <- test$...2[2:181]
test <- str_extract_all(R_RIG,"\\d+")
R_LOC <- matrix(unlist(test))
test <- str_extract_all(L_RIG,"\\d+")
L_LOC <- matrix(unlist(test))


# Significant regions in right hemis
REGION_INDEX[as.integer(R_LOC)]
# Color Scheme according to region
COLOR_REG=c(1:180)
COLOR_REG[as.integer(R_LOC)]
# Visualization
library(ggseg)
library(ggsegGlasser)
someData = tibble(
  #First right than left hemisphere
  region =c(REGION_INDEX[as.integer(R_LOC)],REGION_INDEX[as.integer(L_LOC)]),
  # Color for regions
  Region = c(COLOR_REG[as.integer(R_LOC)],COLOR_REG[as.integer(L_LOC)]),
  HEMIS = c(rep("R", length(REGION_INDEX[as.integer(R_LOC)])), 
            rep("L", length(REGION_INDEX[as.integer(L_LOC)])))
  
)

someData$Region <- as.factor(someData$Region)
someData$region <- as.factor(someData$region)


# FIGURE SOLUTION: https://github.com/ggseg/ggseg/issues/50
my_data = data.frame(
  region = someData$region,
  region_to_fill = someData$region,
  HEMIS=someData$HEMIS,
  stringsAsFactors = FALSE
)



GS_BRAIN <-my_data %>% 
  group_by(HEMIS)%>%
  ggplot()+geom_brain(atlas = glasser,
                      position=position_brain(hemi ~ side),
                      mapping=aes(fill=region_to_fill))+
  scale_fill_brain2(palette=glasser$palette) +
  theme_classic()+
  facet_wrap(~HEMIS)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "GS Presented", 
       subtitle = "0.01 - 0.1 Bandpassed")+
  guides(fill=guide_legend(ncol=12))+
  labs(fill="Regions")


# OLD FIGURE 
# GS_BRAIN <- someData %>%
# group_by(HEMIS)%>%
# ggplot()+geom_brain(atlas=glasser,
#                             show.legend=TRUE,
#                             position = position_brain(hemi ~ side),
#                             aes(fill = region))+
#   facet_wrap(~HEMIS) +
#   # scale_fill_viridis_d(option = "viridis",
#   #                       direction = -1,
#   #                      begin = 0,
#   #                      end = 0.5) +
#   # theme_void() +
#   scale_fill_manual(values = glasser$palette)+
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 10))+
#   labs(title = "GS Presented", 
#        subtitle = "0.01 - 0.1 Bandpassed")+
#   scale_fill_discrete(labels=someData$region)+
#   guides(fill=guide_legend(ncol=12))


ggsave("E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/GS_BRAIN_less.png",
       GS_BRAIN , 
       width = 1920/150, 
       height = 1080/150, 
       dpi = 150)




###########----- G S  R E G R E S S E D - B A N D P A S S E D ########

e=GSR[,1:360] #Removing group and id
classLabel=GSR$GROUP

## COMPUTING P-VALUE DISTRIBUTION

p <- apply(e, 2, function(x){wilcox.test(as.numeric(x[1:17]),
                                         as.numeric(x[18:31]),
                                         alternative = "greater")$p.value})

## COMPUTING THE RANDOM P-VALUE DISTRIBUTION
# How many random sampling
R=100
# Shuffling the sample labels and recomputing the p-value each time
p.rand <- foreach(i = 1:dim(e)[2], .combine=rbind) %do% {
  pval <- vector(mode='numeric', length=R)
  for(j in 1:R){
    randomOrder <- sample(1:length(classLabel))
    pval[j] <- wilcox.test(as.numeric(e[randomOrder[1:17],i]),
                           as.numeric(e[randomOrder[18:31],i]),
                           alternative = "greater")$p.value
  }
  pval
}

## COMPUTING Q-VALUES
qval <- foreach(i = 1:length(p), .combine=c) %do% {
  # how many p-value are lower among the random matrix (lower is better)
  length(which(p.rand <= p[i])) / R
}
qval <- qval/length(qval)

# Finds significant regions by with numbers
NAME=colnames(GSR)  
LEFT <- NAME[1:180]
RIGHT <- NAME[181:360]
lsig=qval[1:180]
rsig=qval[181:360]
R_RIG <- RIGHT[rsig<=0.05]
L_RIG <- LEFT[lsig<=0.05]

test <- read_excel("C:/Users/kaan/Downloads/Glasser_2016_Table.xlsx")
REGION_INDEX <- test$...2[2:181]
test <- str_extract_all(R_RIG,"\\d+")
R_LOC <- matrix(unlist(test))
test <- str_extract_all(L_RIG,"\\d+")
L_LOC <- matrix(unlist(test))


# Significant regions in right hemis
REGION_INDEX[as.integer(R_LOC)]
# Color Scheme according to region
COLOR_REG=c(1:180)
COLOR_REG[as.integer(R_LOC)]
# Visualization
library(ggseg)
library(ggsegGlasser)
someData = tibble(
  #First right than left hemisphere
  region =c(REGION_INDEX[as.integer(R_LOC)],REGION_INDEX[as.integer(L_LOC)]),
  # Color for regions
  Region = c(COLOR_REG[as.integer(R_LOC)],COLOR_REG[as.integer(L_LOC)]),
  HEMIS = c(rep("R", length(REGION_INDEX[as.integer(R_LOC)])), 
            rep("L", length(REGION_INDEX[as.integer(L_LOC)])))
  
)

someData$Region <- as.factor(someData$Region)

my_data = data.frame(
  region = someData$region,
  region_to_fill = someData$region,
  HEMIS=someData$HEMIS,
  stringsAsFactors = FALSE
)



GSR_BRAIN <-my_data %>% 
  group_by(HEMIS)%>%
  ggplot()+geom_brain(atlas = glasser,
                      position=position_brain(hemi ~ side),
                      mapping=aes(fill=region_to_fill))+
  scale_fill_brain2(palette=glasser$palette) +
  theme_classic()+
  facet_wrap(~HEMIS)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "GS Regressed", 
       subtitle = "0.01 - 0.1 Bandpassed")+
  guides(fill=guide_legend(ncol=12))+
  labs(fill="Regions")


# GSR_BRAIN<-someData %>%
#   group_by(HEMIS)%>%
#   ggplot()+geom_brain(atlas=glasser,
#                       show.legend=TRUE,
#                       position = position_brain(hemi ~ side),
#                       aes(fill = Region))+
#   facet_wrap(~HEMIS) +
#   scale_fill_viridis_d(option = "viridis",
#                        direction = -1) +
#   theme_void() +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 10))+
#   labs(title = "GS Regressed", 
#        subtitle = "0.01 - 0.1 Bandpassed")+
#   scale_fill_discrete(labels=someData$region)+
#   guides(fill=guide_legend(ncol=12))


ggsave("E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/GSR_BRAIN.png",
       GSR_BRAIN , 
       width = 1920/150, 
       height = 1080/150, 
       dpi = 150)

#######----G S  P R E S E N T - H I G H  P A S S E D########
e=GS_HP[,1:360] #Removing group and id
classLabel=GS_HP$GROUP

## COMPUTING P-VALUE DISTRIBUTION

p <- apply(e, 2, function(x){wilcox.test(as.numeric(x[1:17]),
                                         as.numeric(x[18:31]),
                                         alternative = "greater")$p.value})

## COMPUTING THE RANDOM P-VALUE DISTRIBUTION
# How many random sampling
R=100
# Shuffling the sample labels and recomputing the p-value each time
p.rand <- foreach(i = 1:dim(e)[2], .combine=rbind) %do% {
  pval <- vector(mode='numeric', length=R)
  for(j in 1:R){
    randomOrder <- sample(1:length(classLabel))
    pval[j] <- wilcox.test(as.numeric(e[randomOrder[1:17],i]),
                           as.numeric(e[randomOrder[18:31],i]),
                           alternative = "greater")$p.value
  }
  pval
}

## COMPUTING Q-VALUES
qval <- foreach(i = 1:length(p), .combine=c) %do% {
  # how many p-value are lower among the random matrix (lower is better)
  length(which(p.rand <= p[i])) / R
}
qval <- qval/length(qval)

# Finds significant regions by with numbers
NAME=colnames(GS_HP)  
LEFT <- NAME[1:180]
RIGHT <- NAME[181:360]
lsig=qval[1:180]
rsig=qval[181:360]
R_RIG <- RIGHT[rsig<=0.05]
L_RIG <- LEFT[lsig<=0.05]

test <- read_excel("C:/Users/kaan/Downloads/Glasser_2016_Table.xlsx")
REGION_INDEX <- test$...2[2:181]
test <- str_extract_all(R_RIG,"\\d+")
R_LOC <- matrix(unlist(test))
test <- str_extract_all(L_RIG,"\\d+")
L_LOC <- matrix(unlist(test))


# Significant regions in right hemis
REGION_INDEX[as.integer(R_LOC)]
# Color Scheme according to region
COLOR_REG=c(1:180)
COLOR_REG[as.integer(R_LOC)]
# Visualization
library(ggseg)
library(ggsegGlasser)
someData = tibble(
  #First right than left hemisphere
  region =c(REGION_INDEX[as.integer(R_LOC)],REGION_INDEX[as.integer(L_LOC)]),
  # Color for regions
  Region = c(COLOR_REG[as.integer(R_LOC)],COLOR_REG[as.integer(L_LOC)]),
  HEMIS = c(rep("R", length(REGION_INDEX[as.integer(R_LOC)])), 
            rep("L", length(REGION_INDEX[as.integer(L_LOC)])))
  
)

someData$Region <- as.factor(someData$Region)


my_data = data.frame(
  region = someData$region,
  region_to_fill = someData$region,
  HEMIS=someData$HEMIS,
  stringsAsFactors = FALSE
)



GS_HP_BRAIN <-my_data %>% 
  group_by(HEMIS)%>%
  ggplot()+geom_brain(atlas = glasser,
                      position=position_brain(hemi ~ side),
                      mapping=aes(fill=region_to_fill))+
  scale_fill_brain2(palette=glasser$palette) +
  theme_classic()+
  facet_wrap(~HEMIS)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "GS Presented", 
       subtitle = "Only Highpass")+
  guides(fill=guide_legend(ncol=12))+
  labs(fill="Regions")


# GS_HP_BRAIN<-someData %>%
#   group_by(HEMIS)%>%
#   ggplot()+geom_brain(atlas=glasser,
#                       show.legend=TRUE,
#                       position = position_brain(hemi ~ side),
#                       aes(fill = Region))+
#   facet_wrap(~HEMIS) +
#   scale_fill_viridis_d(option = "viridis",
#                        direction = -1) +
#   theme_void() +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 10))+
#   labs(title = "GS Presented", 
#        subtitle = "Only Highpa")+
#   scale_fill_discrete(labels=someData$region)+
#   guides(fill=guide_legend(ncol=12))

ggsave("E:/RESEARCH/GSCORR_DEP/GSCORR_RESEARCHPART/GS_HP_BRAIN.png",
       GS_HP_BRAIN , 
       width = 1920/150, 
       height = 1080/150, 
       dpi = 150)
