
library(tidyverse)
library(ggplot2)
library(foreach)
library(doParallel)
library(ggrepel)

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


network2 <- bind_rows(network[c(2, 6, 14)] %>% rename(country = country_from), 
                      network[c(3, 6, 14)] %>% rename(country = country_to))

network.2019 <- network2 %>% 
  filter(Date >= '2019.500' & Date <= '2019.917') %>% 
  group_by(country) %>% 
  summarise(count_2019 = n(), 
            count_in = sum(direct == 'NN') + sum(direct == 'SS'), 
            count_out = sum(direct == 'NS')) %>% 
  mutate(homo_2019 = (count_out - count_in) / (count_out + count_in))

network.2020 <- network2 %>% 
  filter(Date >= '2020.500' & Date <= '2020.917') %>% 
  group_by(country) %>% 
  summarise(count_2020 = n(), 
            count_in = sum(direct == 'NN') + sum(direct == 'SS'), 
            count_out = sum(direct == 'NS')) %>% 
  mutate(homo_2020 = (count_out - count_in) / (count_out + count_in))

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>% 
  mutate(Region = ifelse(Region == 'Arab States', 'Africa', Region))


homo <- inner_join(network.2019[c(1,2,5)], 
                   network.2020[c(1,2,5)], by = 'country') %>% 
  mutate(delta_count = count_2020 > count_2019, 
         delta_homo = homo_2020 > homo_2019, 
         delta = paste0(delta_count, delta_homo)) %>% 
  left_join(countryregion %>% 
              dplyr::select(2,7:8) %>% 
              filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
  filter(!is.na(Region) & !is.na(Direction)) %>% 
  mutate(Direction = ifelse(Direction == 'Global North', 'Developed country', 'Developing country'))
  


ggplot(homo %>% filter(count_2019 > 60 & count_2020 > 60)) + 
  geom_hline(yintercept = -0.5, lwd = 0.5, linetype = 'dashed', col = 'grey') + 
  geom_segment(aes(x = log(count_2019), xend = log(count_2020), 
                   y = homo_2019, yend = homo_2020, 
                   col = Direction), arrow = arrow(length = unit(1, "mm")), size = 0.25) + 
  scale_color_manual(values = c("#E64B35", "#357EBD")) + 
  xlab('ln (Number of international collaboration links)') + 
  ylab('E-I indicator') + 
  facet_wrap(.~delta) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        text = element_text(size = 15), 
        panel.background = element_rect(fill = 'grey95'), 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        legend.background = element_blank(), 
        legend.position = c(0.82, 0.97)) + 
  guides(color = guide_legend(''))

ggsave('fig/SI-Figure12-homophily.pdf', width = 6, height = 6)
