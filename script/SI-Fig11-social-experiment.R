
library(tidyverse)
library(foreach)


domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")

papers <- readRDS('papers_uniquecountry_2000-2021_1_inter_social.rds') %>% 
  filter(!duplicated(Bibkey)) %>% 
  filter((Date > '2017.667') & (Date < '2021.000')) %>% 
  mutate(Social = unlist(mclapply(1:nrow(.), function(i){
    ifelse(str_detect(Research.Domains[i], paste(domains[1:2], collapse = '|')), T, F)
  }, mc.cores = 10)), 
  Natural = unlist(mclapply(1:nrow(.), function(i){
    ifelse(str_detect(Research.Domains[i], paste(domains[3:5], collapse = '|')), T, F)
  }, mc.cores = 10))) %>% 
  group_by(Social, Natural, social) %>% 
  summarise(count = n())
  
text = bind_rows(
  data.frame(majordomain = 'Natural Science', 
             count = sum(papers[papers$Natural == T, ]$count), 
             count_social = sum(papers[papers$Natural == T & papers$social == 'social', ]$count)), 
  data.frame(majordomain = 'Social Science', 
             count = sum(papers[papers$Social == T, ]$count), 
             count_social = sum(papers[papers$Social == T & papers$social == 'social', ]$count))
) %>% 
  mutate(social_ratio = count_social / count)



indexes <- readRDS('index/indexes_normal_alllinks_5domains_social.rds') %>% 
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

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')


indexes.majordomain <- indexes %>% 
  group_by(country, month, post_2020_covid, time, majordomain) %>% 
  summarise(weightedegree = sum(weightedegree)) %>% 
  ungroup() %>% 
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()

impact.majordomains <- bind_rows(foreach(i = 1:length(unique(indexes.majordomain$majordomain))) %do% {
  
  reg <- felm(weightedegree_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes.majordomain, majordomain == unique(majordomain)[i]), 
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
           dependant = as.numeric(sprintf('%.04f', dependant)), 
           ci_high = as.numeric(sprintf('%.04f', ci_high)), 
           ci_low = as.numeric(sprintf('%.04f', ci_low))) %>% 
    slice(1) %>% 
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(majordomain = unique(indexes.majordomain$majordomain)[i])
}) %>% 
  mutate(dependant = dependant * 100, 
         cse = cse * 100, 
         ci_low = ci_low * 100, 
         ci_high = ci_high * 100)
impact.majordomains


ggplot(impact.majordomains, aes(majordomain, dependant)) + 
  geom_bar(aes(fill = majordomain), 
           stat = 'identity', position = 'dodge2', width = 0.9, alpha = 0.4, show.legend = F) + 
  geom_linerange(aes(ymin = dependant - cse, ymax = dependant + cse, col = majordomain), 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) + 
  geom_linerange(aes(ymin = ci_low, ymax = ci_high, col = majordomain), 
                 lwd = 0.4, stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) +
  geom_point(aes(col = majordomain), fill = 'white', size = 1.6, pch = 21, 
             stat = 'identity', position = position_dodge(width = 0.9), show.legend = F) + 
  geom_hline(yintercept = 0, col = 'grey') + 
  scale_fill_manual(values = c("#357EBD", "#0FADC7")) + 
  scale_color_manual(values = c("#357EBD", "#0FADC7")) + 
  xlab('') + 
  ylab('Effect size on international collaborations\ninvolving social experiments (%)') + 
  theme_classic() + 
  theme(aspect.ratio = 0.7,  
        text = element_text(size = 14.5), 
        axis.line = element_line(size = 0.4), 
        axis.text = element_text(size = 15), 
        axis.ticks = element_line(size = 0.4), 
        axis.title.x = element_text(margin = margin(t = 5)), 
        axis.title.y.right = element_text(margin = margin(l = 5)), 
        legend.position = c(0.25, 0.8), 
        legend.background = element_blank()) + 
  guides(fill = guide_legend(''), 
         alpha = guide_legend(''), 
         col = guide_none())

ggsave('fig/SI-Figure11-social-experiment.pdf', width = 6, height = 5)

