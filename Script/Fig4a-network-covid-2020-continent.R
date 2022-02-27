
library(igraph)
library(GGally)
library(tidyverse)
library(sf)
library(ggplot2)
library(foreach)
library(doParallel)
library(ggnetwork)
library(intergraph)
library(ggraph)


countryregion <- read_csv('data/00_origin/countryRegion.csv')

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology", 
             "COVID-19 related")


network <- readRDS('data/papers_international_1.rds') %>% 
  rename(country = country_from, country_core = country_to) %>% 
  filter(Date >= '2000') %>% 
  left_join(countryregion[c(2,7:8)] %>%
              `colnames<-`(c('country', 'country_region', 'country_direction')) %>% 
              filter(!duplicated(country)), 
            by = 'country') %>%
  left_join(countryregion[c(2,7:8)] %>%
              `colnames<-`(c('country_core', 'country_core_region', 'country_core_direction')) %>% 
              filter(!duplicated(country_core)), 
            by = 'country_core') %>%
  filter(country != country_core)


network <- network %>%
  filter(!is.na(country_direction) & !is.na(country_core_direction)) %>%
  mutate(direct = ifelse(country_direction != country_core_direction,
                         'NS',
                         ifelse(str_detect(country_direction, 'South'),
                                'SS',
                                'NN')))

covid <- readRDS('index/papersCOVID.rds')

network <- network %>% 
  filter((Bibkey %in% covid$Bibkey))

edges <- network %>%
  as.data.frame() %>%
  filter(Date >= '2020.000' & Date <= '2020.917')

edges <- edges %>%
  mutate(undirected = unlist(lapply(1:nrow(.), function(i){
    paste(sort(c(country[i], country_core[i])), collapse = ', ')
  }))) %>%
  group_by(undirected) %>%
  summarise(country = country[1],
            country_core = country_core[1],
            weight = n()) %>%
  mutate(undirected = NULL) %>%
  as.data.frame()

country.list <- unique(c(edges$country, edges$country_core))

nodes <- bind_rows(foreach(i = 1:length(country.list)) %do% {
  one.country <- edges[edges$country == country.list[i] | edges$country_core == country.list[i], ]
  data.frame(country = country.list[i],
             count = sum(one.country$weight),
             entropy = -sum(((one.country$weight) / sum(one.country$weight)) * log((one.country$weight) / sum(one.country$weight))))
})

net <- readRDS('index/net_template_covid.rds')

countries <- edges %>%
  select(country, country_core) %>%
  unlist() %>%
  as.character() %>%
  unique()

net.node <- net %>%
  filter(x == xend & y == yend)

edges <- edges %>%
  left_join(net.node[c('vertex.names', 'x', 'y')], by = c('country_core' = 'vertex.names')) %>%
  left_join(net.node[c('vertex.names', 'xend', 'yend')], by = c('country' = 'vertex.names')) %>%
  rename(count = weight) %>%
  left_join(countryregion[c(2,7:8)] %>%
              filter(!duplicated(country_shp)),
            by = c('country' = 'country_shp')) %>%
  left_join(countryregion[c(2,7:8)] %>%
              filter(!duplicated(country_shp)) %>%
              rename(Region_core = Region,
                     Direction_core = Direction),
            by = c('country_core' = 'country_shp')) %>%
  mutate(linktype = ifelse(Direction != Direction_core,
                           'NS',
                           ifelse(Direction == 'Global South',
                                  'SS', 'NN')))

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>%
  mutate(Region = ifelse(Region == 'Arab States', 'Africa', Region))

nodes <- nodes %>%
  left_join(net.node[c('vertex.names', 'x', 'y', 'xend', 'yend')], by = c('country' = 'vertex.names')) %>%
  mutate(entropy = NULL) %>%
  left_join(countryregion[c(2,7:8)] %>%
              filter(!duplicated(country_shp)),
            by = c('country' = 'country_shp'))


ggplot() +
  geom_edges(aes(x = x, y = y, xend = xend, yend = yend, col = linktype), edges,
             alpha = 0.2, lwd = 0.1) +
  scale_color_manual(values = c("#1E2B94", "#BB00C4", "#BF3F05")) +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  geom_nodes(aes(x = x, y = y, size = count, fill = Region), nodes[nodes$Direction == 'Global North', ],
             pch = 21, alpha = 0.95, col = 'grey50', stroke = 0.12) +
  geom_nodes(aes(x = x, y = y, size = count, fill = Region), nodes[nodes$Direction == 'Global South', ],
             pch = 24, alpha = 0.95, col = 'grey50', stroke = 0.12) +
  geom_nodes(aes(x = x, y = y, size = count, fill = Region), nodes[nodes$country == 'United States', ],
             pch = 21, alpha = 0.95, col = 'grey50', stroke = 0.12) +
  geom_nodes(aes(x = x, y = y, size = count, fill = Region), nodes[nodes$country == 'India', ],
             pch = 24, alpha = 0.95, col = 'grey50', stroke = 0.12) +
  scale_size_continuous(range = c(2, 8), breaks = c(1, 10, 100, 1000, 10000), limits = c(1, 10000)) +
  scale_fill_manual(values = c("#F0027F", "#29B757", "#DFF4F8", "#D5B3DC", "#FF963C", '#C9D748')) +
  guides(color = guide_legend(title = 'Linkage type'),
         fill = guide_legend(title = 'Region'),
         size = guide_legend(title = 'Count')) +
  theme_blank() +
  theme(aspect.ratio = 1,
        legend.box.margin = margin(0,0,0,-10),
        plot.title = element_text(hjust = 0.5, size = 18))

ggsave('fig/Fig4a-network-covid-2020-continent.pdf', width = 8, height = 6)


