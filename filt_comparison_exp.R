# Required packages
library(readxl)
library(tidyverse)

# Read metadata for boat ramp samples
metadat <- read_excel('WLM615770_261107.xlsx', sheet = 'metadata', skip = 20) 

metadat <- bind_rows(
  metadat_pj <- metadat %>%
  filter(SiteID == 'Point Julia Boat ramp') %>%
  separate(SampleID, sep = "_", into = c("filt_type", "pore_size", "rep")),
metadat_nb <- metadat %>%
  filter(SiteID == 'North Beach') %>%
  separate(SampleID, sep = "_", into = c("filt_type", "loc", "sub_loc", "depth", "pore_size", "date", "rep")) %>%
  select(-loc, -sub_loc, -depth, -date)
)
rm(metadat_nb, metadat_pj)

# Read in reads data at spp. level
aggdat <- read_excel('WLM615770_261107.xlsx', sheet = 'aggregated')
aggdat <- aggdat %>%
  filter(Rank == 'species')

# Merge metadat and aggdat 
aggdat_long <- aggdat %>%
  pivot_longer(cols = matches("^[0-9]+$"),
               names_to = 'UID',
               values_to = 'reads')
aggdat_long <- aggdat_long %>% 
  inner_join(metadat, by = 'UID') 
aggdat_long <- aggdat_long %>%
  mutate(filt_type_size = paste(filt_type, pore_size, sep = '_')) 

# Visualize Fish Group 
ggplot(aggdat_long %>%
         filter(Group == 'Fish'), aes(x = ScientificName, y = reads, 
                                      fill = filt_type_size)) +
  geom_col() +
  facet_wrap(~ UID, ncol = 1) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

# Species richness (Fish)
rich <- aggdat_long %>%
  filter(Group == 'Fish') %>%
  group_by(UID) %>%
  summarize(richness = length(which(reads > 0)),
            filt_type_size = unique(filt_type_size),
            rep = unique(rep))

ggplot(rich, aes(x = filt_type_size, y = richness, col = filt_type_size)) +
  geom_point(size = 3) + 
  theme_minimal() +
  geom_text()

ggplot(aggdat_long %>% filter(Group == 'Fish'), aes(x = ScientificName, y = UID, fill = reads > 0)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))


# Relative read abundance visualization
samps <- aggdat %>%
  select(matches("^[0-9]+$"))

samps_rel <- sweep(samps, 2, colSums(samps), FUN = '/')
samps_rel <- cbind(aggdat %>%
                     select(-matches("^[0-9]+$")), samps_rel)   
samps_rel_long <- samps_rel %>%
  pivot_longer(cols = matches("^[0-9]+$"),
               names_to = 'UID',
               values_to = 'rel_reads')
samps_rel_long <- samps_rel_long %>%
  inner_join(metadat, by = 'UID')

ggplot(samps_rel_long, aes(x = UID, y = rel_reads, fill = Group)) +
  geom_col() +
  facet_wrap(~ filt_type, ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) 
  
  
