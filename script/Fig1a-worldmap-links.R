
library(tidyverse)
library(ggplot2)
library(foreach)
library(sf)
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


world <- st_read('data/00_origin/gadm36/gadm36_adm0.shp')

world.center <- st_centroid(world) %>% 
  mutate(lonlat = as.character(geometry), 
         lon = gsub('c[(](.*?),\\s.+', '\\1', lonlat), 
         lat = gsub('.*?,\\s(.*?)[)]', '\\1', lonlat), 
         lon = as.numeric(sprintf('%.03f', as.numeric(lon))), 
         lat = as.numeric(sprintf('%.03f', as.numeric(lat))), 
         lonlat = NULL)

country <- read_csv('data/00_origin/countryshp.csv') %>% 
  left_join(world.center, by = c('country_shp' = 'NAME_0')) %>% 
  mutate(stringdist = NULL, 
         geometry = NULL) %>% 
  as.data.frame()


edges <- network %>% 
  filter(Date >= '2020.000' & Date < '2021.000') %>% 
  mutate(undirected = unlist(lapply(1:nrow(.), function(i){
    paste(sort(c(country_from[i], country_to[i])), collapse = ', ')
  }))) %>% 
  group_by(undirected) %>% 
  summarise(country_from = country_from[1], 
            country_to = country_to[1], 
            weight = n()) %>%  
  mutate(undirected = NULL) %>% 
  as.data.frame()

country.list <- unique(c(edges$country_from, edges$country_to))

nodes <- bind_rows(foreach(i = 1:length(country.list)) %do% {
  one.country <- edges[edges$country_from == country.list[i] | edges$country_to == country.list[i], ]
  data.frame(country = country.list[i],
             weight = sum(one.country$weight))
})


edges <- edges %>% 
  left_join(country %>% 
              select(country_shp, lon, lat) %>% 
              filter(!duplicated(country_shp)), 
            by = c('country_from' = 'country_shp')) %>% 
  mutate(lon_from = lon, lat_from = lat, 
         lon = NULL, lat = NULL) %>% 
  left_join(country %>% 
              select(country_shp, lon, lat) %>% 
              filter(!duplicated(country_shp)), 
            by = c('country_to' = 'country_shp')) %>% 
  mutate(lon_to = lon, lat_to = lat, 
         lon = NULL, lat = NULL)


nodes <- nodes %>% 
  left_join(country %>% 
              select(country_shp, lon, lat) %>% 
              filter(!duplicated(country_shp)), 
            by = c('country' = 'country_shp'))


ggplot(nodes) + 
  geom_polygon(aes(x = long, y = lat, group = group),
               data = map_data('world'),
               fill = "black", color = "grey50",
               size = 0.1) +
  geom_curve(aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,     # draw edges as arcs
                 color = log10(weight), size = weight),
             data = edges[edges$weight >= 100, ], curvature = 0.2,
             alpha = 0.3) + 
  scale_color_distiller(palette = 'YlOrRd', direction = -1,
                        limits = c(2, 5),
                        breaks = seq(2, 5),
                        labels = c(expression(paste('10'^{2})), expression(paste('10'^{3})),
                                   expression(paste('10'^{4})), expression(paste('10'^{5})))) +
  scale_size_continuous(guide = FALSE, range = c(0.01, 4)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = weight),           # draw nodes
             shape = 21, fill = 'white', alpha = 1, 
             color = 'white', stroke = 0.5) +
  geom_text(aes(x = lon, y = lat, label = country),
            data = nodes[nodes$country %in% c('United States', 'China'), ],
            hjust = 0, nudge_x = -12, nudge_y = 6,
            size = 4, color = "white", fontface = "bold") +
  coord_sf(xlim = c(-180, 190), ylim = c(-60, 90)) + 
  scale_x_continuous('', expand = c(0, 0)) + 
  scale_y_continuous('', expand = c(0, 0)) + 
  theme_bw() + 
  theme(aspect.ratio = 150/360, 
        text = element_text(size = 15), 
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "#1C3A50"),
        panel.grid.major = element_line(colour = NA, size = 0.2), 
        legend.position = c(0.15, 0.2), 
        legend.key = element_rect(fill = 'transparent', color = 'transparent'), 
        legend.key.height = unit(0.1, "npc"), 
        legend.key.width = unit(0.15, "npc"), 
        legend.text = element_text(size = 13, color = 'white'), 
        legend.title = element_text(size = 15, vjust = 1.2, color = 'white'), 
        legend.margin = margin(t = 2, b = 3, r = 6, l = 6),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
        legend.direction = 'horizontal') + 
  guides(color = guide_colorbar(title = 'Num. of collaboration links', 
                                title.position = 'top', 
                                barwidth = 8, 
                                barheight = 1))

ggsave('Fig1a-worldmap-links.pdf', height = 6, width = 12)

