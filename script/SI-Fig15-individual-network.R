
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

sample <- c('China', 'South Africa', 'Iran', 'Indonesia', 'Italy', 'United Kingdom', 'Canada')
color <- c('#4D8D74', '#4D8D74', '#4D8D74', '#4D8D74', '#B13625', '#B13625', '#B13625')

pplots <- vector('list', length(sample))

for (k in 1:length(sample)) {
  
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
      filter(country == sample[k] | country_core == sample[k]) %>% 
      mutate(country_temp = paste0(country, country_core), 
             country_core = sample[k], 
             country = gsub(sample[k], '', country_temp), 
             country_temp = NULL) %>% 
      select(country) %>% 
      unlist() %>% 
      as.character() %>% 
      append(sample[k])
    
    edges <- edges %>% 
      filter(country %in% countries & country_core %in% countries)
    
    edges.core <- edges %>% 
      filter(country == sample[k] | country_core == sample[k]) %>% 
      mutate(country_temp = paste0(country, country_core), 
             country_core = sample[k], 
             country = gsub(sample[k], '', country_temp), 
             country_temp = NULL)
    
    nodes <- nodes %>% 
      filter(country %in% countries) %>% 
      left_join(edges.core[c('country', 'weight')], by = 'country') %>% 
      mutate(weight = ifelse(is.na(weight), sum(edges.core$weight), weight), 
             count = NULL)
    
    net.node <- net %>% 
      filter(x == xend & y == yend)
    
    edges <- edges %>% 
      filter(country != sample[k] & country_core != sample[k]) %>% 
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
    
    plots[[j-1]] <-
      ggplot(mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(data = edges,
                 size = 0.05, color = "grey", alpha = 0.2, show.legend = F) +
      geom_edges(aes(size = count, col = Direction), edges.core, 
                 alpha = 0.3, show.legend = F) +
      scale_size_continuous(range = c(0.06, 0.4), limits = c(1, 150)) + 
      ggnewscale::new_scale(new_aes = 'size') +
      geom_nodes(aes(size = count, col = Direction, shape = Direction), 
                 nodes[nodes$country != sample[k], ] %>% mutate(count = ifelse(count >= 200, 200, count)), 
                 alpha = 0.7, show.legend = F) + 
      geom_nodes(, nodes[nodes$country == sample[k], ], 
                 pch = 23, col = '#9632B8', stroke = 0.2, fill = 'white', size = 1) + 
      scale_size_continuous(range = c(0.5, 2.5), limits = c(1, 200)) + 
      scale_color_manual(values = c("#B13625", "#4D8D74")) + 
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      ggtitle(titles[j-1]) + 
      guides(color = guide_legend(title = 'Region'), 
             size = guide_legend(title = 'Count')) +
      theme_blank() + 
      theme(aspect.ratio = 1, 
            legend.box.margin = margin(0,0,0,-10), 
            plot.title = element_text(hjust = 0.5, size = 9))
  }
  
  title <- ggplot() + 
    geom_label(aes(0, 0, label = label), 
               data.frame(label = sample[k]), col = color[k], 
               size = 4) + 
    theme_blank() + 
    theme(aspect.ratio = 1)
  
  pplots[[k]] <- title | plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]] | plots[[5]]
}

ggsave('fig/SI-Figure15-individual-countries.pdf', 
       pplots[[1]] / pplots[[2]] / pplots[[3]]  / pplots[[4]] / pplots[[5]] / pplots[[6]] / pplots[[7]], 
       width = 9, height = 12)

