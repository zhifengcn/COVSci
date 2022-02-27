
library(foreach)
library(doParallel)
library(tidyverse)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)
library(ggh4x)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")

papers <- readRDS('data/papers_uniquecountry_2000-2021_1_inter.rds')

coll.count <- papers %>% 
    as.data.frame() %>% 
    filter(Date >= '2020.000' & Date < '2021.000') %>% 
    filter(!duplicated(Bibkey)) %>% 
    group_by(members) %>% 
    summarise(count = n()) %>% 
    mutate(count = ifelse(members == 15, sum(.[.$members >= 15, ]$count), count)) %>% 
    filter(members <= 15)

impact.linkage <-
  bind_rows(foreach(group = c('Global South', 'Global North', 'overall')) %do% {
    bind_rows(foreach(members = c(as.character(2:14), 'gteq15')) %do% {
      
      indexes <- readRDS(paste0('index/indexes_normal_alllinks_5domains_', members, '.rds')) %>% 
        filter(weightedegree >= 2)
      
      indexes <- indexes %>% filter(
        (month > '2017.667') & (month < 2021.000)
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
        mutate(weightedegree_log = log(weightedegree)) %>%
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
      
      if (group != 'overall') {
        indexes <- indexes %>%
          filter(Direction == group)
      }
      
      indexes <- indexes %>%
        filter(month >= '2020.500' | month <= '2020.083')
      
      reg <- felm(weightedegree_log ~ post_2020_covid + time | 
                    country_month_date + domain_month_date | 0 | country,
                  data = indexes,
                  na.action = "na.omit")
      
      bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
                data.frame(cse = reg[['cse']]), 
                data.frame(cpval = reg[['cpval']])) %>% 
        mutate(significance = '', 
               significance = ifelse(cpval < 0.1, '*', significance), 
               significance = ifelse(cpval < 0.05, '**', significance), 
               significance = ifelse(cpval < 0.01, '***', significance), 
               ci_high = dependant + 1.96 * cse, 
               ci_low = dependant - 1.96 * cse, 
               dependant = as.numeric(sprintf('%.03f', dependant)), 
               ci_high = as.numeric(sprintf('%.03f', ci_high)), 
               ci_low = as.numeric(sprintf('%.03f', ci_low))) %>% 
        slice(1) %>% 
        select(1,5:6,2:4) %>% 
        mutate(members = members, 
               group = group) %>% 
        mutate(dependant = dependant * 100, 
               cse = cse * 100, 
               ci_high = ci_high * 100, 
               ci_low = ci_low * 100)
    })
  })


ggplot(impact.linkage %>% 
         filter(group != 'overall') %>% 
         mutate(members = ifelse(members == 'gteq15', '15', members), 
                members = as.integer(members)), 
       aes(members, dependant)) + 
  
  # bottom
  geom_segment(aes(x = members, xend = members, y = -70, yend = -70 + log10(count)*8), 
               coll.count, stat = 'identity', size = 15, col = 'grey80') + 

  # Economic status
  geom_bar(aes(fill = group, group = group), width = 0.8, stat = 'identity', position = 'dodge2') + 
  geom_hline(yintercept = 0, col = 'grey') +
  geom_linerange(y = -13.1, xmin = 1.3, xmax = 15.7, col = '#7030A0', alpha = 0.3, lwd = 0.05, linetype = 'dotted') +
  geom_linerange(aes(ymin = dependant - cse, ymax = dependant + cse, col = group, group = group), 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.8)) + 
  geom_linerange(aes(ymin = ci_low, ymax = ci_high, col = group, group = group), 
                 lwd = 0.4, stat = 'identity', position = position_dodge(width = 0.8)) +
  geom_point(aes(col = group, group = group), fill = 'white', size = 1.6, pch = 21, 
             stat = 'identity', position = position_dodge(width = 0.8)) + 
  # Overall effect
  geom_ribbon(aes(members, dependant, ymin = dependant - cse, ymax = dependant + cse), 
              col = NA, fill = 'grey', alpha = 0.25, 
              impact.linkage %>% 
                filter(group == 'overall') %>% 
                mutate(members = ifelse(members == 'gteq15', '15', members),
                       members = as.integer(members))) + 
  geom_ribbon(aes(members, dependant, ymin = ci_low, ymax = ci_high), 
              col = NA, fill = 'grey', alpha = 0.15, 
              impact.linkage %>% 
                filter(group == 'overall') %>% 
                mutate(members = ifelse(members == 'gteq15', '15', members),
                       members = as.integer(members))) + 
  geom_line(aes(members, dependant), col = 'white', 
            impact.linkage %>%
              filter(group == 'overall') %>% 
              mutate(members = ifelse(members == 'gteq15', '15', members),
                     members = as.integer(members))) + 
  geom_point(aes(members, dependant), col = 'white', 
             impact.linkage %>%
               filter(group == 'overall') %>% 
               mutate(members = ifelse(members == 'gteq15', '15', members),
                      members = as.integer(members))) + 

  scale_color_manual(values = c("#E64B35", "#357EBD")) + 
  scale_fill_manual(values = c("#FADBD6", "#D6E5F1")) + 
  scale_y_continuous(
    "Effect size on number of international collaboration links (%)", 
    breaks = c(-60, -40, -20, 0, 20), 
    limits = c(-70, 30), expand = c(0.01, 0.01), 
    sec.axis = dup_axis(~ . , name = "Number of papers", guide = "axis_truncated", 
                        breaks = c(-70, -62, -54, -46, -38, -30), 
                        labels = c(0, expression(paste('10'^{1})), expression(paste('10'^{2})),
                                   expression(paste('10'^{3})), expression(paste('10'^{4})), 
                                   expression(paste('10'^{5}))))) + 
  scale_x_continuous('Number of participating country', 
                     breaks = 2:15, labels = c(2:14, '>=15'), expand = c(0.02, 0.02)) + 
  theme_classic() + 
  theme(aspect.ratio = 0.5, 
        text = element_text(size = 14), 
        axis.line = element_line(size = 0.2), 
        axis.title.y.right = element_text(margin = margin(l = 10), hjust = 1),
        legend.position = c(0.5, 0.9), 
        legend.background = element_blank()) + 
  guides(col = guide_legend(title = '', direction = 'horizontal'), 
         fill = guide_legend(title = '', direction = 'horizontal'))

ggsave('fig/SI-Figure9-teamsize.pdf', width = 8, height = 4)    
    