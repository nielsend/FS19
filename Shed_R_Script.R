setwd('~/')

install.packages("pracma")
library(tidyverse)
library(pracma)

### Shedding data
shed_data <- read_csv('~/FS19_ForAUCAnalysis.csv')


sum_shed <- shed_data %>% group_by(Animal) %>%
  summarise(AUC=trapz(Day, Shed),
            AULC=trapz(Day, log(Shed+1)),
            sum=sum(Shed), 
            maxshed=Shed[which.max(Shed)], 
            day_max=Day[which.max(Shed)], 
            pos_samples=sum(Shed > 0), 
            StudyVar=unique(StudyVar), 
            Strain=unique(Strain))

sum_shed %>%
  ggplot(aes(x=Strain, y=AULC), fill=StudyVar) +
  geom_boxplot()+
  geom_jitter(width= 0.22, size=3, aes(
   color=StudyVar, fill=StudyVar
    ))+ ggtitle("Fecal Shedding AULC by Strain") + xlab("Strain") + labs(color="Trial", fill="Trial")




summary(aov(data = sum_shed, formula = AULC~Strain))
aovDat <- aov(data = sum_shed, formula = AULC~Strain)
TukeyHSD(aovDat)


#### SWABS
swab_data <- read_csv('~/RamsSwabs.csv')


sum_swab <- swab_data %>% group_by(Animal) %>%
  summarise(AUC=trapz(Day, Swab),
            AULC=trapz(Day, log(Swab+1)),
            sum=sum(Swab), 
            maxswab=Swab[which.max(Swab)], 
            day_max=Day[which.max(Swab)], 
            pos_samples=sum(Swab > 0), 
            StudyVar=unique(StudyVar), 
            Strain=unique(Strain))


sum_swab %>%
  ggplot(aes(x=Strain, y=AULC), fill=StudyVar) +
  geom_boxplot()+
  geom_jitter(width= 0.22, size=3, aes(
    color=StudyVar, fill=StudyVar
  ))+ ggtitle("Fecal Swab AULC by Strain") + xlab("Strain") + labs(color="Trial", fill="Trial")




summary(aov(data = sum_swab, formula = AULC~Strain))
aovDat <- aov(data = sum_swab, formula = AULC~Strain)
TukeyHSD(aovDat)
