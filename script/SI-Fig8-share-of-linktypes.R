
library(foreach)
library(tidyverse)
library(doParallel)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")

indexes.bind <- readRDS(paste0('index/indexes_normal_alllinks_5domains_NN.rds')) %>% 
  select(1,5,11:12) %>% 
  filter(weightedegree >= 2) %>% 
  rename(weightedegree_NN = weightedegree) %>% 
  full_join(readRDS(paste0('index/indexes_normal_alllinks_5domains_SS.rds')) %>% 
              select(1,5,11:12) %>% 
              filter(weightedegree >= 2) %>% 
              rename(weightedegree_SS = weightedegree), 
            by = c('country', 'month', 'domain')) %>% 
  full_join(readRDS(paste0('index/indexes_normal_alllinks_5domains_NS.rds')) %>% 
              select(1,5,11:12) %>% 
              filter(weightedegree >= 2) %>% 
              rename(weightedegree_NS = weightedegree), 
            by = c('country', 'month', 'domain')) %>% 
  mutate(weightedegree_NN = ifelse(is.na(weightedegree_NN), 0, weightedegree_NN), 
         weightedegree_SS = ifelse(is.na(weightedegree_SS), 0, weightedegree_SS), 
         weightedegree_NS = ifelse(is.na(weightedegree_NS), 0, weightedegree_NS)) %>% 
  mutate(weightedegree_NN_share = unlist(mclapply(1:nrow(.), function(i){
    weightedegree_NN[i] / (sum(weightedegree_NN[i] + weightedegree_SS[i] + weightedegree_NS[i]))
  }, mc.cores = 10)), 
  weightedegree_SS_share = unlist(mclapply(1:nrow(.), function(i){
    weightedegree_SS[i] / (sum(weightedegree_NN[i] + weightedegree_SS[i] + weightedegree_NS[i]))
  }, mc.cores = 10)), 
  weightedegree_NS_share = unlist(mclapply(1:nrow(.), function(i){
    weightedegree_NS[i] / (sum(weightedegree_NN[i] + weightedegree_SS[i] + weightedegree_NS[i]))
  }, mc.cores = 10)))



impact.linkage.share <- bind_rows(foreach(dir = c('NN', 'SS', 'NS')) %do% {
  
  indexes <- indexes.bind %>% filter(
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
  
  if (dir == 'NN') {
    fmla <- as.formula('weightedegree_NN_share ~ post_2020_covid + time | country_month_date + domain_month_date | 0 | country')
  } else if (dir == 'SS') {
    fmla <- as.formula('weightedegree_SS_share ~ post_2020_covid + time | country_month_date + domain_month_date | 0 | country')
  } else if (dir == 'NS') {
    fmla <- as.formula('weightedegree_NS_share ~ post_2020_covid + time | country_month_date + domain_month_date | 0 | country')
  }
  
  reg <- felm(fmla,
              data = indexes,
              na.action = "na.omit")
  
  stargazer(reg,type="text")
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])),
            data.frame(cse = reg[['cse']]),
            data.frame(cpval = reg[['cpval']])) %>%
    mutate(significance = '',
           significance = ifelse(cpval < 0.1, '*', significance),
           significance = ifelse(cpval < 0.05, '**', significance),
           significance = ifelse(cpval < 0.01, '***', significance),
           ci_high = dependant + 1.96 * cse,
           ci_low = dependant - 1.96 * cse,
           dependant = as.numeric(sprintf('%.04f', dependant)),
           ci_high = as.numeric(sprintf('%.04f', ci_high)),
           ci_low = as.numeric(sprintf('%.04f', ci_low))) %>%
    slice(1) %>%
    select(1:2,5:6,3:4) %>%
    mutate(direct = dir)
})

impact.linkage.share

impact.linkage.share <- impact.linkage.share %>% 
  mutate(dependant = dependant * 100, 
         cse = cse * 100, 
         ci_low = ci_low * 100, 
         ci_high = ci_high * 100) %>% 
  mutate(direct = factor(direct, c('SS', 'NN', 'NS')))

ggplot(impact.linkage.share, aes(direct, dependant)) + 
  geom_bar(fill = '#357EBD', stat = 'identity', 
           position = 'dodge2', width = 0.8, alpha = 0.4, show.legend = F) + 
  geom_linerange(aes(ymin = dependant - cse, ymax = dependant + cse), col = '#357EBD', 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) + 
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), col = '#357EBD', 
                 lwd = 0.4, stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) +
  geom_point(col = '#357EBD', fill = 'white', size = 1.6, pch = 21, 
             stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) + 
  geom_hline(yintercept = 0, col = 'grey') + 
  scale_fill_manual(values = c("#357EBD", "#0FADC7")) + 
  scale_color_manual(values = c("#357EBD", "#0FADC7")) + 
  scale_x_discrete(labels = c('Between developing\ncountries', 'Between\ndeveloping countries', 
                              'Between developing and\ndeveloping countries')) +
  xlab('') + 
  ylab('Effect size on share of international\ncollaboration links (percentage point)') + 
  theme_classic() + 
  theme(aspect.ratio = 0.7,  
        text = element_text(size = 15), 
        axis.line = element_line(size = 0.4), 
        axis.text = element_text(size = 11), 
        axis.ticks = element_line(size = 0.4), 
        axis.title.x = element_text(margin = margin(t = 5)), 
        axis.title.y.right = element_text(margin = margin(l = 5)), 
        legend.position = c(0.25, 0.8), 
        legend.background = element_blank()) + 
  guides(fill = guide_legend(''), 
         alpha = guide_legend(''), 
         col = guide_none())

ggsave('fig/SI-Figure8-did-share-of-linktype.pdf', width = 6, height = 5)
