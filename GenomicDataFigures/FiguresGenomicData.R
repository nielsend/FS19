VFDB findings

library(tidyverse)

#Import files
vfdb_NCBIgroup <- read_tsv('~/vfdb_NCBIgroup.csv')
ecoh_NCBIgroup <- read_tsv('~/ecoh_NCBIgroup.csv')



# Sort, Filter, Select
vfdb_NCBIgroup <- vfdb_NCBIgroup %>% 
  filter(`%COVERAGE`>90) %>% filter(`%IDENTITY` > 90)
vfdb_NCBIgroup1 <- vfdb_NCBIgroup %>% select(FILE, GENE)

ggplot(vfdb_NCBIgroup1, aes(FILE, GENE)) + geom_tile(aes(x=FILE, y=GENE))

#Filter which genes MG1655 has
MG1655List <- vfdb_NCBIgroup1 %>% filter(FILE == 'MG1655')

#Filter genes from MG1655 out of original file
NoMG1655Genes <- vfdb_NCBIgroup1 %>% filter(GENE != 'yagV/ecpE') %>%  filter(GENE != 'yagW/ecpD') %>% filter(GENE != 'yagX/ecpC') %>% 
  filter(GENE != 'yagY/ecpB') %>% filter(GENE != 'yagZ/ecpA') %>% filter(GENE != 'ykgK/ecpR') %>% filter(GENE != 'entD') %>% 
  filter(GENE != 'fepA') %>% filter(GENE != 'fes') %>% filter(GENE != 'entF') %>% filter(GENE != 'fepC') %>% filter(GENE != 'fepG') %>%
  filter(GENE != 'fepD') %>% filter(GENE != 'entS') %>% filter(GENE != 'fepB') %>% filter(GENE != 'entC') %>% filter(GENE != 'entE') %>% 
  filter(GENE != 'entB') %>% filter(GENE != 'entA') %>% filter(GENE != 'ompA') %>% filter(GENE != 'espL1') %>% filter(GENE != 'gspM') %>% 
  filter(GENE != 'gspL') %>% filter(GENE != 'gspC') %>% filter(GENE != 'aslA') %>% filter(GENE != 'espL4') %>% filter(GENE != 'espX4') %>% 
  filter(GENE != 'espX5') %>% filter(GENE != 'fimB') %>% filter(GENE != 'fimE') %>% filter(GENE != 'fimA') %>% filter(GENE != 'fimI') %>%
  filter(GENE != 'fimC') %>% filter(GENE != 'fimD') %>% filter(GENE != 'fimF') %>% filter(GENE != 'fimG') %>% filter(GENE != 'fimH') 

mdataLinStrain <- read_csv('~/metadataLineageStrains.csv')
NoMG1655GenesWLin <- merge(NoMG1655Genes, mdataLinStrain, by="FILE")
ggplot(NoMG1655GenesWLin, aes(FILE, GENE, Lineage)) + geom_tile(aes(x=FILE, y=GENE, fill=Lineage)) + 
  facet_grid(~Lineage, scales="free_x") + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + labs(fill= 'LSPA-6 Lineage', x='Strain', y="Gene")




### ecoh database
ecoh_NCBIgroup <- ecoh_NCBIgroup %>% separate(`#FILE`, into=c('FILE'), sep='.fasta')
ggplot(ecoh_NCBIgroup, aes(FILE, GENE, `%COVERAGE`)) + geom_tile(aes(x=FILE, y=GENE, fill=`%COVERAGE`))

### No polymorphisms/difference in ecoh database group for O157 strains seen wzy-O157, wzx-O157, fliC-H7
### No gene presence/absence seen for O157 strains

View(ecoh_NCBIgroup)
