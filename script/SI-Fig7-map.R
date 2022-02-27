
library(foreach)
library(doParallel)
library(tidyverse)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)
library(scales)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


indexes <- readRDS('index/indexes_normal_alllinks_5domains_paperbased.rds') %>% 
  filter(papers_coll >= 2)

indexes <- indexes %>% filter(
  (month > '2017.667') & (month < '2021.000')
)

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>% 
  mutate(Region = ifelse(Region == 'Arab States', 'Africa', Region))

indexes <- indexes %>% 
  left_join(countryregion %>% 
              dplyr::select(2,7:8) %>% 
              filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
  filter(!is.na(Region) & !is.na(Direction))

indexes <- indexes %>% 
  mutate(post_covid = ifelse(month >= 2020.167, 1, 0), 
         post_2020 = ifelse(month >= 2020, 1, 0)) %>% 
  mutate(post_2020_covid = post_covid * post_2020) %>% 
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>% 
  mutate(papers_coll_ratio = papers_coll / papers, 
         papers_coll_ratio_std = as.numeric(scale(papers_coll_ratio)), 
         papers_coll_log = log(papers_coll)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  mutate(majordomain = ifelse(domain %in% domains[1:2], 'Social Science', 'Natural Science')) %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup() %>% 
  group_by(majordomain, month_date) %>%
  mutate(majordomain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')


impact.country <- bind_rows(foreach(i = 1:length(unique(indexes$country))) %do% {

  reg <- tryCatch(felm(papers_coll_log ~ post_2020_covid + time| 
                    domain_month_date | 0 | month_date,
                  data = subset(indexes, country == unique(indexes$country)[i]),
                  na.action = "na.omit"), 
             silent = T, 
             warning = function(e) {
               error_text <- trimws(paste0("WARNING: ", e))
             })
  
  if (class(reg) == 'try-error') {
    return(NULL)
  }
  
  if (class(reg) == 'character') {
    if (str_detect(reg, 'WARNING')) {
      return(NULL)
    }
  }
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
            data.frame(cse = ifelse(is_null(reg[['cse']]), NA, reg[['cse']])), 
            data.frame(cpval = ifelse(is_null(reg[['cpval']]), NA, reg[['cpval']]))) %>% 
    mutate(significance = '', 
           significance = ifelse(!is.na(cpval) & cpval < 0.1, '*', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.05, '**', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.01, '***', significance), 
           ci_high = dependant + 1.96 * cse, 
           ci_low = dependant - 1.96 * cse, 
           # dependant = sprintf('%.03f', dependant * 100),
           # ci_high = sprintf('%.03f', ci_high * 100),
           # ci_low = sprintf('%.03f', ci_low * 100)) %>%
           dependant = as.numeric(sprintf('%.03f', dependant * 100)),
           ci_high = as.numeric(sprintf('%.03f', ci_high * 100)),
           ci_low = as.numeric(sprintf('%.03f', ci_low * 100))) %>%
    slice(1) %>% 
    select(1:2,5:6,3:4) %>% 
    mutate(country = unique(indexes$country)[i])
}) %>% 
  # left_join(indexes %>%
  #             filter(month >= '2020.500') %>%
  #             group_by(country) %>%
  #             summarise(papers_coll_sum = sum(papers_coll)),
  #           by = 'country') %>%
  # mutate(dependant = sprintf('%.02f', dependant),
  #        ci_high = sprintf('%.02f', ci_high),
  #        ci_low = sprintf('%.02f', ci_low), 
  #        ci95 = paste0('[', ci_low, ',', ci_high, ']'),
  #        dependant = paste0(dependant, significance)) %>%
  # arrange(country) %>% 
  # select(country, dependant, papers_coll_sum, ci95)
  mutate(dependant = as.numeric(dependant)) %>% 
  mutate(dependant = ifelse(dependant > 100, 100, dependant), 
         dependant = ifelse(dependant < -100, -100, dependant))


world <- st_read('data/00_origin/gadm36/gadm36_adm0.shp')
sf::sf_use_s2(F)
world <- world %>%
  left_join(impact.country,
            by = c('NAME_0' = 'country')) %>% 
  st_centroid() %>% 
  as.data.frame() %>% 
  mutate(geometry = as.character(geometry), 
         lon = as.numeric(gsub('c[(](.*?),.+', '\\1', geometry)), 
         lat = as.numeric(gsub('.*?,\\s(.*?)[)]', '\\1', geometry)))

worldmap <- map_data('world') %>% 
  mutate(region = ifelse(region == 'UK', 'United Kingdom', region), 
         region = ifelse(region == 'USA', 'United States', region), 
         region = ifelse(region == 'North Macedonia', 'Macedonia', region), 
         region = ifelse(region == 'Palestine', 'Palestina', region), 
         region = ifelse(region == 'Antigua' | region == 'Barbuda', 'Antigua and Barbuda', region), 
         region = ifelse(region == 'Ivory Coast', "Côte d'Ivoire", region), 
         region = ifelse(region == 'Trinidad' | region == 'Tobago', 'Trinidad and Tobago', region), 
         region = ifelse(region == 'Curacao', 'Curaçao', region), 
         region = ifelse(region == 'Saint Kitts' | region == 'Nevis', 'Saint Kitts and Nevis', region), 
         region = ifelse(region == 'Saint Vincent' | region == 'Grenadines', 'Saint Vincent and the Grenadines', region), 
         region = ifelse(region == 'Vatican', 'Vatican City', region), 
         region = ifelse(region == 'Virgin Islands', 'British Virgin Islands', region), 
         region = ifelse(region == 'Svalbard' | region == 'Jan May', 'Svalbard and Jan Maye', region), 
         region = ifelse(region == 'Sao Tome and Principe', 'São Tomé and Príncipe', region), 
         region = ifelse(region == 'Bonaire' | region == 'Sint Eustatius' | region == 'Saba', 'Bonaire, Sint Eustatius and Saba', region)) %>% 
  left_join(impact.country, 
            by = c('region' = 'country'))


rect <- st_polygon(list(cbind(c(rep(-180, 151), rep(190, 151), -180), 
                              c(seq(-60, 90), seq(90, -60), -60))))
rect <- data.frame(temp = 1) %>%
  mutate(geom = st_sfc(rect, crs = '+proj=longlat +datum=WGS84')) %>%
  st_as_sf()


p1 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = worldmap,
               fill = "grey80", color = "grey50", size = 0.1) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = dependant),
               data = worldmap[!is.na(worldmap$dependant), ],
               color = "grey50", size = 0.1) +
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '***', ], 
             size = 1, pch = 25, col = '#2559CD', fill = '#2559CD', stroke = 0.2, show.legend = T) + 
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '**', ], 
             size = 1.1, pch = 25, col = '#2559CD', fill = 'white', stroke = 0.2, show.legend = T) + 
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '*', ], 
             size = 1.2, pch = 21, col = '#2559CD', fill = 'white', stroke = 0.2, show.legend = T) + 
  scale_color_manual(values = c('#3C5488', 'grey50', 'white')) + 
  scale_fill_gradientn("Effect size (%)",
                       values = rescale(c(min(impact.country$dependant), 
                                          min(impact.country$dependant)/3, 
                                          0, 
                                          max(impact.country$dependant)/3, 
                                          max(impact.country$dependant))),
                       limits = c(min(impact.country$dependant), max(impact.country$dependant)),
                       breaks = c(min(impact.country$dependant), -50, 0, 50, max(impact.country$dependant)),
                       labels = c(min(impact.country$dependant), -50, 0, 50, max(impact.country$dependant)), 
                       colours = colorRampPalette(c("#762a83", "#9970ab", "#c2a5cf",
                                                    "#e7d4e8", "#f7f7f7", "#d9f0d3",
                                                    "#a6dba0", "#5aae61", "#1b7837"))(200)) +
  coord_sf(xlim = c(-180, 190), ylim = c(-60, 90)) + 
  scale_y_continuous('', expand = c(0, 0)) + 
  scale_x_continuous('', expand = c(0, 0)) + 
  theme_bw() + 
  theme(aspect.ratio = 150/360, 
        text = element_text(size = 11), 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = c(0.14, 0.14), 
        legend.key = element_rect(fill = 'transparent', color = 'transparent'), 
        legend.key.height = unit(0.015, "npc"), 
        legend.key.width = unit(0.04, "npc"), 
        legend.text = element_text(size = 7.5, color = 'black'), 
        legend.title = element_text(size = 9, vjust = 2, hjust = 0.5, color = 'black', margin = margin(t = 4)), 
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
        legend.direction = 'horizontal') + 
  guides(fill = guide_colorbar(title = 'Effect size in number (%)', 
                               title.position = 'bottom'))


# Ratio
impact.country <- bind_rows(foreach(i = 1:length(unique(indexes$country))) %do% {
  
  reg <- tryCatch(felm(papers_coll_ratio ~ post_2020_covid + time| 
                         domain_month_date | 0 | month_date,
                       data = subset(indexes, country == unique(indexes$country)[i]),
                       na.action = "na.omit"), 
                  silent = T, 
                  warning = function(e) {
                    error_text <- trimws(paste0("WARNING: ", e))
                  })
  
  if (class(reg) == 'try-error') {
    return(NULL)
  }
  
  if (class(reg) == 'character') {
    if (str_detect(reg, 'WARNING')) {
      return(NULL)
    }
  }
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
            data.frame(cse = ifelse(is_null(reg[['cse']]), NA, reg[['cse']])), 
            data.frame(cpval = ifelse(is_null(reg[['cpval']]), NA, reg[['cpval']]))) %>% 
    mutate(significance = '', 
           significance = ifelse(!is.na(cpval) & cpval < 0.1, '*', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.05, '**', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.01, '***', significance), 
           ci_high = dependant + 1.96 * cse, 
           ci_low = dependant - 1.96 * cse, 
           dependant = as.numeric(sprintf('%.03f', dependant * 100)), 
           ci_high = as.numeric(sprintf('%.03f', ci_high * 100)), 
           ci_low = as.numeric(sprintf('%.03f', ci_low * 100))) %>% 
    slice(1) %>% 
    select(1:2,5:6,3:4) %>% 
    mutate(country = unique(indexes$country)[i])
}) %>% 
  # left_join(indexes %>% 
  #             filter(month >= '2020.500') %>% 
  #             group_by(country) %>% 
  #             summarise(papers_coll_sum = sum(papers_coll)), 
  #           by = 'country') %>% 
  # filter(papers_coll_sum >= 6*10) %>%
  # mutate(cse = as.numeric(sprintf('%.03f', cse))) %>% 
  # mutate(dependant = ifelse(nchar(dependant) == 6, dependant, paste0('  ', dependant)), 
  #        ci_low = ifelse(nchar(ci_low) == 6, ci_low, paste0('  ', ci_low)), 
  #        ci_high = ifelse(nchar(ci_high) == 6, ci_high, paste0('  ', ci_high)), 
  #        ci95 = paste0('[', ci_low, ',', ci_high, ']')) %>% 
  # mutate(dependant = paste0(dependant, significance)) %>% 
  # arrange(country) 
  mutate(dependant = as.numeric(dependant)) %>% 
  mutate(dependant = ifelse(dependant > 20, 20, dependant), 
         dependant = ifelse(dependant < -20, -20, dependant))


world <- st_read('data/00_origin/gadm36/gadm36_adm0.shp')
sf::sf_use_s2(F)
world <- world %>%
  left_join(impact.country,
            by = c('NAME_0' = 'country')) %>% 
  st_centroid() %>% 
  as.data.frame() %>% 
  mutate(geometry = as.character(geometry), 
         lon = as.numeric(gsub('c[(](.*?),.+', '\\1', geometry)), 
         lat = as.numeric(gsub('.*?,\\s(.*?)[)]', '\\1', geometry)))

worldmap <- map_data('world') %>% 
  mutate(region = ifelse(region == 'UK', 'United Kingdom', region), 
         region = ifelse(region == 'USA', 'United States', region), 
         region = ifelse(region == 'North Macedonia', 'Macedonia', region), 
         region = ifelse(region == 'Palestine', 'Palestina', region), 
         region = ifelse(region == 'Antigua' | region == 'Barbuda', 'Antigua and Barbuda', region), 
         region = ifelse(region == 'Ivory Coast', "Côte d'Ivoire", region), 
         region = ifelse(region == 'Trinidad' | region == 'Tobago', 'Trinidad and Tobago', region), 
         region = ifelse(region == 'Curacao', 'Curaçao', region), 
         region = ifelse(region == 'Saint Kitts' | region == 'Nevis', 'Saint Kitts and Nevis', region), 
         region = ifelse(region == 'Saint Vincent' | region == 'Grenadines', 'Saint Vincent and the Grenadines', region), 
         region = ifelse(region == 'Vatican', 'Vatican City', region), 
         region = ifelse(region == 'Virgin Islands', 'British Virgin Islands', region), 
         region = ifelse(region == 'Svalbard' | region == 'Jan May', 'Svalbard and Jan Maye', region), 
         region = ifelse(region == 'Sao Tome and Principe', 'São Tomé and Príncipe', region), 
         region = ifelse(region == 'Bonaire' | region == 'Sint Eustatius' | region == 'Saba', 'Bonaire, Sint Eustatius and Saba', region)) %>% 
  left_join(impact.country, 
            by = c('region' = 'country'))


rect <- st_polygon(list(cbind(c(rep(-180, 151), rep(190, 151), -180), 
                              c(seq(-60, 90), seq(90, -60), -60))))
rect <- data.frame(temp = 1) %>%
  mutate(geom = st_sfc(rect, crs = '+proj=longlat +datum=WGS84')) %>%
  st_as_sf()


p2 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = worldmap,
               fill = "grey80", color = "grey50", size = 0.1) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = dependant),
               data = worldmap[!is.na(worldmap$dependant), ],
               color = "grey50", size = 0.1) +
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '***', ], 
             size = 1, pch = 25, col = '#2559CD', fill = '#2559CD', stroke = 0.2, show.legend = T) + 
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '**', ], 
             size = 1.1, pch = 25, col = '#2559CD', fill = 'white', stroke = 0.2, show.legend = T) + 
  geom_point(aes(lon, lat), 
             world[!is.na(world$significance) & world$significance == '*', ], 
             size = 1.2, pch = 21, col = '#2559CD', fill = 'white', stroke = 0.2, show.legend = T) + 
  scale_color_manual(values = c('#3C5488', 'grey50', 'white')) + 
  scale_fill_gradientn("Coefficient",
                       values = rescale(c(min(impact.country$dependant), 
                                          min(impact.country$dependant)/3, 
                                          0, 
                                          max(impact.country$dependant)/3, 
                                          max(impact.country$dependant))),
                       limits = c(-20, 20),
                       breaks = c(-20, -10, 0, 10, max(impact.country$dependant)),
                       labels = c(-20, -10, 0, 10, max(impact.country$dependant)), 
                       colours = colorRampPalette(c("#762a83", "#9970ab", "#c2a5cf",
                                                    "#e7d4e8", "#f7f7f7", "#d9f0d3",
                                                    "#a6dba0", "#5aae61", "#1b7837"))(200)) +
  coord_sf(xlim = c(-180, 190), ylim = c(-60, 90)) + 
  scale_y_continuous('', expand = c(0, 0)) + 
  scale_x_continuous('', expand = c(0, 0)) + 
  theme_bw() + 
  theme(aspect.ratio = 150/360, 
        text = element_text(size = 11), 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = c(0.14, 0.14), 
        legend.key = element_rect(fill = 'transparent', color = 'transparent'), 
        legend.key.height = unit(0.015, "npc"), 
        legend.key.width = unit(0.04, "npc"), 
        legend.text = element_text(size = 7.5, color = 'black'), 
        legend.title = element_text(size = 9, vjust = 2, hjust = 0.5, color = 'black', margin = margin(t = 4)), 
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
        legend.direction = 'horizontal') + 
  guides(fill = guide_colorbar(title = 'Effect size in proportion\n(percentage point)', 
                               title.position = 'bottom'))

p1 / p2

ggplot2::ggsave("fig/SI-Figure7-map.pdf", width = 7.5, height = 7)



