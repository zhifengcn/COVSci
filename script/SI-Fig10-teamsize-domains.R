
library(foreach)
library(doParallel)
library(tidyverse)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")

impact.linkage <- bind_rows(foreach(d = domains) %do% {
  bind_rows(foreach(group = c('overall')) %do% {
    bind_rows(foreach(members = c(as.character(2:14), 'gteq15')) %do% {
      
      indexes <- readRDS(paste0('index/indexes_normal_alllinks_5domains_', members, '.rds')) %>% 
        filter(weightedegree >= 2) %>% 
        filter(domain == d)
      
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
      
      if (nrow(indexes) < 200) {
        return(NULL)
      }
      
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
               group = group, 
               domain = d) %>% 
        mutate(dependant = dependant * 100, 
               cse = cse * 100, 
               ci_high = ci_high * 100, 
               ci_low = ci_low * 100)
    })
  })
}) %>% 
  mutate(domain = factor(domain, levels = domains))
    
  
ggplot(impact.linkage %>% 
         mutate(members = ifelse(members == 'gteq15', '15', members),
                members = as.integer(members)) %>% 
         filter(domain != 'Arts & Humanities'), 
       aes(members, dependant)) + 
  geom_hline(yintercept = 0, col = 'grey') + 
  geom_linerange(aes(members, dependant, ymin = dependant - cse, ymax = dependant + cse, col = domain), 
                 lwd = 1.2, show.legend = F) + 
  geom_errorbar(aes(members, dependant, ymin = ci_low, ymax = ci_high, col = domain), 
                lwd = 0.3, width = 0.1, show.legend = F) + 
  geom_point(aes(members, dependant, col = domain), 
             pch = 21, fill = 'white', size = 1.6, show.legend = F) + 
  facet_wrap(.~domain) + 
  scale_y_continuous('Effect size on number of international collaboration links (%)') + 
  scale_x_continuous('Number of participating country', 
                     breaks = 2:15, labels = c(2:14, '>=15'), expand = c(0.02, 0.02)) + 
  scale_color_manual(values = c("#E64B35", "#4DBBD5", "#9632B8", "#357EBD")) + 
  theme_classic() + 
  theme(aspect.ratio = 0.6, 
        text = element_text(size = 12), 
        axis.line = element_line(size = 0.2), 
        axis.title.y.right = element_text(margin = margin(l = 10), hjust = 1),
        legend.position = c(0.5, 0.9), 
        legend.background = element_blank(), 
        strip.background = element_blank(), 
        panel.spacing = unit(1, "lines")) + 
  guides(col = guide_legend(title = '', direction = 'horizontal'), 
         fill = guide_legend(title = '', direction = 'horizontal'))

ggsave('fig/SI-Figure10-teamsize-domains.pdf', width = 7.5, height = 5)
