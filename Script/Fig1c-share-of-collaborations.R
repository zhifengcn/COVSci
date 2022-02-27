
library(tidyverse)
library(ggplot2)
library(foreach)
library(ggrepel)
library(patchwork)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


income <- read_csv('data/00_origin/income.csv')

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>% 
  left_join(income, by = c('country_shp' = 'country')) %>% 
  mutate(level = ifelse(is.na(level), 'Low', level))


network <- readRDS('data/papers_international_1.rds') %>% 
  filter(Date >= '2000') %>% 
  mutate(undirected = unlist(mclapply(1:nrow(.), function(i){
    paste(sort(c(country_from[i], country_to[i])), collapse = ', ')
  }, mc.cores = 10))) %>% 
  group_by(undirected) %>% 
  mutate(country_from = country_from[1], 
         country_to = country_to[1]) %>%  
  ungroup() %>% 
  mutate(undirected = NULL) %>%
  left_join(countryregion[c(2,7:9)] %>%
              `colnames<-`(c('country_from', 'country_from_region', 'country_from_direction', 'country_from_income')) %>% 
              filter(!duplicated(country_from)), 
            by = 'country_from') %>%
  left_join(countryregion[c(2,7:9)] %>%
              `colnames<-`(c('country_to', 'country_to_region', 'country_to_direction', 'country_to_income')) %>% 
              filter(!duplicated(country_to)), 
            by = 'country_to')


covid <- readRDS('index/papersCOVID.rds')

network <- network %>% 
  filter(!(Bibkey %in% covid$Bibkey))


network <- network %>%
  filter(!is.na(country_from_direction) & !is.na(country_to_direction)) %>%
  mutate(direct = ifelse(country_from_direction != country_to_direction,
                         'NS',
                         ifelse(str_detect(country_from_direction, 'South'),
                                'SS',
                                'NN')))

coll <- network %>% 
  group_by(Year_) %>% 
  summarise(count = n())

coll.uscn <- network %>% 
  filter((country_from == 'United States' & country_to == 'China') | (country_from == 'China' & country_to == 'United States')) %>% 
  group_by(Year_) %>% 
  summarise(count = n()) %>% 
  mutate(rate_uscn = count / coll$count)

coll.us <- network %>% 
  filter(country_from == 'United States' | country_to == 'United States') %>% 
  group_by(Year_) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(count = count - coll.uscn$count, 
         rate_us = count / coll$count)

coll.cn <- network %>% 
  filter(country_from == 'China' | country_to == 'China') %>% 
  group_by(Year_) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(count = count - coll.uscn$count, 
         rate_cn = count / coll$count)

collrate <- bind_cols(coll.us[c(1,3)], 
                      coll.uscn[,3], 
                      coll.cn[,3]) %>% 
  mutate(rate_other = 1 - rate_us - rate_cn - rate_uscn) %>% 
  pivot_longer(cols = 2:5, names_to = 'withwho', values_to = 'rate') %>% 
  mutate(withwho = ifelse(withwho == 'rate_us', 'With US', withwho), 
         withwho = ifelse(withwho == 'rate_cn', 'With China', withwho), 
         withwho = ifelse(withwho == 'rate_uscn', 'Between US and China', withwho), 
         withwho = ifelse(withwho == 'rate_other', 'Other countries', withwho), 
         withwho = factor(withwho, levels = rev(c('With US', 'Between US and China', 'With China', 'Other countries')))) %>%  
  filter(Year_ >= 2010 & Year_ <= 2020)


ggplot(collrate, aes(Year_, rate, fill = withwho)) + 
  geom_bar(width = 0.96, position = "fill", stat = "identity") + 
  scale_fill_manual(values = rev(c('#ABB6C9', '#5F769E', '#C2C5B6', '#DADAD8'))) + 
  coord_flip() + 
  scale_x_discrete('', expand = c(0, 0)) + 
  scale_y_continuous('Share of total collaboration links', expand = c(0, 0), labels = c('0', '25%', '50%', '75%', '100%')) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        text = element_text(size = 15), 
        axis.line = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12), 
        legend.position = c(0.7, 0.18)) + 
  guides(fill = guide_legend(reverse = T))

ggsave('Fig1c-share-of-collaborations.pdf', height = 5, width = 5)



