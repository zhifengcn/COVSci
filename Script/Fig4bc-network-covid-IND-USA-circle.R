
setwd('~/Copublication/')
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


titles <- c('Oct 2019 - Dec 2019', 'Jan 2020 - Mar 2020', 'Apr 2020 - Jun 2020', 'Jul 2020 - Sep 2020', 'Oct 2020 - Dec 2020')
months <- data.frame(c('All', titles), 
                     low = c('2019.750', '2019.750', '2020.000', '2020.250', '2020.500', '2020.700'), 
                     high = c('2020.917', '2019.917', '2020.167', '2020.417', '2020.667', '2020.917'))

net <- readRDS('index/net_template_covid.rds')

plots <- vector(mode = 'list', length = 5)

for (j in 2:6) {
  
  edges <- network %>% 
    as.data.frame() %>% 
    filter(Date >= months$low[j] & Date <= months$high[j])
  
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
  
  countries <- edges %>% 
    filter(country == 'India' | country_core == 'India') %>% 
    mutate(country_temp = paste0(country, country_core), 
           country_core = 'India', 
           country = gsub('India', '', country_temp), 
           country_temp = NULL) %>% 
    select(country) %>% 
    unlist() %>% 
    as.character() %>% 
    append('India')
  
  edges <- edges %>% 
    filter(country %in% countries & country_core %in% countries)
  
  edges.core <- edges %>% 
    filter(country == 'India' | country_core == 'India') %>% 
    mutate(country_temp = paste0(country, country_core), 
           country_core = 'India', 
           country = gsub('India', '', country_temp), 
           country_temp = NULL)
  
  nodes <- nodes %>% 
    filter(country %in% countries) %>% 
    left_join(edges.core[c('country', 'weight')], by = 'country') %>% 
    mutate(weight = ifelse(is.na(weight), sum(edges.core$weight), weight), 
           count = NULL)
  
  net.node <- net %>% 
    filter(x == xend & y == yend)
  
  edges <- edges %>% 
    filter(country != 'India' & country_core != 'India') %>% 
    mutate(weight = 1) %>% 
    bind_rows(edges.core) %>% 
    left_join(net.node[c('vertex.names', 'x', 'y')], by = c('country_core' = 'vertex.names')) %>% 
    left_join(net.node[c('vertex.names', 'xend', 'yend')], by = c('country' = 'vertex.names')) %>% 
    mutate(country_core = NULL) %>% 
    rename(count = weight) %>%
    left_join(countryregion[c(2,7:8)] %>%
                filter(!duplicated(country_shp)),
              by = c('country' = 'country_shp'))
  
  edges.core <- edges.core %>% 
    left_join(net.node[c('vertex.names', 'x', 'y')], by = c('country_core' = 'vertex.names')) %>% 
    left_join(net.node[c('vertex.names', 'xend', 'yend')], by = c('country' = 'vertex.names')) %>% 
    mutate(country_core = NULL) %>% 
    rename(count = weight) %>%
    left_join(countryregion[c(2,7:8)] %>%
                filter(!duplicated(country_shp)),
              by = c('country' = 'country_shp'))
  
  nodes <- nodes %>% 
    left_join(net.node[c('vertex.names', 'x', 'y', 'xend', 'yend')], by = c('country' = 'vertex.names')) %>% 
    mutate(entropy = NULL) %>% 
    rename(count = weight) %>%
    left_join(countryregion[c(2,7:8)] %>%
                filter(!duplicated(country_shp)),
              by = c('country' = 'country_shp'))
  
  net.month <- bind_rows(edges, nodes)
  
  share <- nodes %>% 
    filter(country != 'India') %>% 
    group_by(Direction) %>% 
    summarise(countries = n(), 
              links = sum(count)) %>% 
    mutate(share_count = countries / sum(countries), 
           ymax_count = cumsum(share_count), 
           ymin_count = c(0, head(ymax_count, n=-1)), 
           share_links = links / sum(links), 
           ymax_links = cumsum(share_links), 
           ymin_links = c(0, head(ymax_links, n=-1)))
  
 plots[[j-1]] <- ggplot(share) +
    # geom_vline(xintercept = 4.8, col = 'grey', lwd = 0.4) +
    geom_linerange(aes(x = 4.75, ymin = 0, ymax = min(ymax_count)),
                   col = 'grey', lwd = 0.4) +
    geom_rect(aes(ymax = ymax_count, ymin = ymin_count, 
                  xmax = 5.04, xmin = 4.8, fill = Direction), 
              alpha = 0.35) +
    geom_rect(aes(ymax = ymax_links, ymin = ymin_links, 
                  xmax = 4.75, xmin = 4.62, fill = Direction), 
              alpha = 0.8) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("white", "#4D8D74")) + 
    xlim(c(1, 5.04)) +
    theme_void() +
    theme(legend.position = "none")
  
}

plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]] | plots[[5]]

ggsave('circle_IND.pdf', width = 6.5, height = 2)




# Legend ------------------------------------------------------------------

ggplot(share) +
  geom_rect(aes(ymax = ymax_count, ymin = ymin_count, 
                xmax = 5.02, xmin = 4.8, fill = Direction), 
            alpha = 0.35) +
  geom_rect(aes(ymax = ymax_links, ymin = ymin_links, 
                xmax = 4.75, xmin = 4.62, fill = Direction), 
            alpha = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("white", "#4D8D74")) + 
  xlim(c(4, 5.02)) +
  theme_void() +
  theme(legend.position = "none")



