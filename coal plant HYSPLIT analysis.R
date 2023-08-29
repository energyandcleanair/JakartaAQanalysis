require(tidyverse)
require(magrittr)
require(creahelpers)
require(rcrea)
require(sf)

#data_dir='~/../Downloads'
data_dir='data'

source('read_PM_data.R')

readRDS(file.path(data_dir, 'plants.RDS')) -> plants
readRDS(file.path(data_dir, 'receptors.RDS')) -> receptors

read_csv('https://api.energyandcleanair.org/stations?city_name=Jakarta&format=csv') %>% filter(source=='airnow') -> st

st %<>% use_series(coordinates) %>% str_split(',') %>% lapply(force_numeric) %>% 
  lapply(t) %>% lapply(data.frame) %>% bind_rows() %>% set_names(c('lon', 'lat')) %>% 
  bind_cols(st %>% select(id, name), .) %>% to_sf_points()

st$receptor_id <- receptors$id[st_nearest_feature(st, receptors)]



readRDS(file.path(data_dir, 'normalised_contributions_gdas1_10m_plant_receptors.RDS')) %>% rename(conc=starts_with('contribution_')) -> contrib
tz(contrib$date_reception) <- "UTC"
contrib$date_reception %<>% with_tz(tz(pm$date))

plants %<>% mutate(plant_short = plants %>% gsub(' power station', '', ., ignore.case=T) %>% gsub(' |-', '_', .))

contrib %<>% left_join(plants %>% select(plant_id=plants, plant_short) %>% st_drop_geometry()) %>% ungroup

emis_bycluster <- read_csv(file.path("G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/", 'HYSPLIT emissions by pollutant.csv'))
emis_bycluster$plants[grep('Suralaya', emis_bycluster$plants)] <- 'Suralaya'

emis_bycluster %<>% mutate(pm25_potential_t=pm25_potential_t*case_when(pollutant=='SOx'~.2,
                                                                      pollutant=='NOx'~.3,
                                                                      T~1))

emis_bycluster %<>% filter(pollutant != 'total') %>% group_by(across(-c(emissions_t, pm25_potential_t, pollutant))) %>% 
  summarise(across(c(emissions_t, pm25_potential_t), sum)) %>% mutate(pollutant='total') %>% 
  bind_rows(emis_bycluster %>% filter(pollutant!='total'))

plants %<>% select(-emissions_t) %>% left_join(emis_bycluster %>% select(plants, pollutant, emissions_t, pm25_potential_t) %>% filter(pollutant=='total'))

pm %>% 
  filter(date>=min(contrib$date_reception)) %>% 
  left_join(st %>% select(Site=name, receptor_id)) %>% 
  select(date, receptor_id, conc) %>% mutate(plant_short='ambient') %>% 
  bind_rows(contrib %>% select(receptor_id, plant_short, date=date_reception, conc)) ->
  contrib_w_ambient

contrib_w_ambient %>% filter(!is.na(conc)) %>% 
  distinct(across(-conc), .keep_all=T) %>% 
  spread(plant_short, conc) -> contrib_wide


require(nnls)
plants_to_include <- plants$plant_short
as.matrix(contrib_wide[,c('ambient', plants_to_include)]) %>% subset(complete.cases(.)) -> nnls_input
nnls(nnls_input[,-1],nnls_input[,1])$x -> plants$emissions_t_best_fit
plants$emissions_t_best_fit[plants$plant_short=='Muara_Karang'] <- 0

contrib_w_ambient %>% filter(grepl('Menteng', receptor_id)) %>% 
  inner_join(plants %>% select(plant_short, pollutant, emissions_t, pm25_potential_t, emissions_t_best_fit) %>% st_drop_geometry()) %>% 
  mutate(emissions_t_best_fit = pmin(emissions_t_best_fit, pm25_potential_t)) %>% 
  mutate(conc = conc * emissions_t_best_fit) %>% 
  group_by(plant_short) %>% arrange(date) %>% mutate(conc = zoo::rollapply(conc, 24, mean, fill=NA, align='center')) %>% ungroup ->
  contrib_best

contrib_best %>% 
  filter(date>='2023-07-01') %>% 
  group_by(plant_short=plant_short %>% gsub('_', ' ', .)) %>% 
  summarise(across(conc, list(mean=~mean(.x, na.rm=T), max=~max(.x, na.rm=T)))) %>% 
  filter(conc_mean>0) %>% 
  arrange(conc_mean) ->
  contrib_agg

contrib_agg %>% 
  rename(Average=conc_mean, '24-hour maximum'=conc_max) %>% 
  pivot_longer(is.numeric) %>% 
  group_by(name) %>% 
  mutate(fillvalue=value/max(value)) %>% 
  ggplot(aes(plant_short, value, fill=fillvalue)) +
  facet_wrap(~trans(name), scales='free_x') +
  geom_col() + 
  scale_x_discrete(limits=contrib_agg$plant_short) +
  coord_flip() +
  theme_crea() + 
  x_at_zero() +
  scale_fill_distiller(palette='Reds', direction=1, guide='none') +
  labs(title=trans("Estimated contribution of different coal power plants to Jakarta's PM2.5 pollution", wrap_chars = 80),
       y='µg/m3', x='',
       subtitle=trans("in July–August 2023")) -> p
quicksave(paste0('outputs/Estimated contribution of different coal power plants to Jakartas PM25 pollution, ',lang,'.png'), plot=p, footer_height=.03)

contrib_best %>% 
  group_by(receptor_id, date) %>% 
  summarise(across(conc, sum)) %>% 
  mutate(plant_short='all_coal_plants') %>% 
  bind_rows(contrib_w_ambient %>% filter(plant_short=='ambient', grepl('Menteng', receptor_id))) -> plotdata

plotdata %<>% group_by(plant_short) %>% arrange(date) %>% mutate(conc = zoo::rollapply(conc, 24, mean, fill=NA, align='center')) %>% ungroup

plotdata %>% mutate(plant_short=ifelse(plant_short=='all_coal_plants', 'coal power', plant_short)) %>% 
  ggplot(aes(date, conc, col=trans(plant_short))) + geom_line(linewidth=1) + 
  labs(col=trans('source'), title=trans('Jakarta ambient PM2.5 levels and estimated coal power plant contribution'),
       subtitle=trans('24-hour running mean'), x='', y='µg/m3') +
  theme_crea() + scale_color_crea_d('dramatic') +
  scale_x_datetime(labels=function(x) paste(trans(format.Date(x, '%b')), format.Date(x, '%Y'))) + x_at_zero() -> p
quicksave(paste0('outputs/Jakarta ambient PM2.5 levels and estimated coal power plant contribution, ',lang,'.png'), plot=p)

plotdata %>% select(plant_short, date, conc) %>% spread(plant_short, conc) %>% ggplot(aes(ambient,all_coal_plants))+geom_point()+geom_abline()
plotdata %>% select(plant_short, date, conc) %>% spread(plant_short, conc) %>% mutate(share=all_coal_plants/ambient) %>% 
  filter(date>='2023-07-01') %>% summary

plotdata %>% select(plant_short, date, conc) %>% spread(plant_short, conc) %>% lm(data=., ambient~all_coal_plants) %>% summary


contrib %>% 
  rename(conc_1t=conc) %>% 
  mutate(pm25_potential_t=pm25_potential_t*case_when(pollutant=='SOx'~.2,
                                                     pollutant=='NOx'~.3,
                                                     T~1)) %>% 
  mutate(conc=conc_1t * pm25_potential_t) %>% 
  filter(plant_short %in% plants_to_include) %>% 
  group_by(receptor_id, date_reception, pollutant) %>% 
  summarise(across(conc, sum)) %>% 
  mutate(plant_short=paste0('all_coal_plants_', pollutant)) %>% 
  bind_rows(contrib %>% filter(!grepl('all_coal', plant_short))) ->
  contrib_w_totals

contrib_w_totals %<>% filter(plant_short!='all_coal_plants_total', grepl('all_coal_plants', plant_short)) %>% 
  group_by(receptor_id, date_reception) %>% 
  summarise(across(conc, sum)) %>% 
  mutate(plant_short='all_coal_plants_total', pollutant='total') %>% 
  bind_rows(contrib_w_totals %>% filter(plant_short!='all_coal_plants_total'))


contrib_w_totals %<>% 
  rename(conc_1t=conc) %>% 
  mutate(conc=conc_1t * pm25_potential_t) %>% 
  filter(plant_short %in% plants_to_include, pollutant %in% c('SOx', 'NOx')) %>% 
  group_by(receptor_id, date_reception) %>% 
  summarise(across(conc, sum)) %>% 
  mutate(plant_short='all_coal_plants_acid_gases', pollutant='acid gases') %>% 
  bind_rows(contrib_w_totals %>% filter(!grepl('acid_gases', plant_short)))

pm %>% 
  filter(date>=min(contrib_w_totals$date_reception)) %>% 
  left_join(st %>% select(Site=name, receptor_id)) %>% 
  select(date, receptor_id, conc) %>% mutate(plant_short='ambient', pollutant='total') %>% 
  bind_rows(contrib_w_totals %>% select(receptor_id, plant_short, pollutant, date=date_reception, conc)) ->
  contrib_w_ambient


contrib_w_ambient %>% 
  filter(plant_short %in% c('ambient', 'all_coal_plants_total'),
         grepl('Menteng', receptor_id)) %>% 
  ggplot(aes(date, conc, col=plant_short)) + geom_line()


contrib_w_ambient %>% filter(!is.na(conc), pollutant=='total' | grepl('all_coal', plant_short)) %>% 
  distinct(across(-conc), .keep_all=T) %>% 
  select(-pollutant) %>% spread(plant_short, conc) -> contrib_wide

contrib_wide %>% 
  filter(grepl('Menteng', receptor_id)) %>% 
  ggplot(aes(ambient, all_coal_plants_total)) + geom_point() + geom_abline()


contrib_wide %>% lm(data=., formula=ambient~Suralaya+Cirebon+Cilacap+Labuan+DSS_Serang+FAJAR) -> m
m %>% summary

contrib_wide %>% lm(data=., formula=as.formula(paste('ambient~', paste(plants_to_include, collapse='+')))) %>% summary
                    
contrib_wide %>% lm(data=., formula=ambient~all_coal_plants_NOx*as.factor(hour(date))) -> m
summary(m)

contrib_wide$all_coal_plants_total %>% summary

predict(m, contrib_wide) -> pred
predict(m, contrib_wide %>% mutate(all_coal_plants_SOx=0, all_coal_plants_NOx=0, all_coal_plants_PM=0)) -> pred_nocoal
contrib_wide$coal_contribution_best_fit <- (pred-pred_nocoal) %>% pmax(0)
(pred-pred_nocoal) %>% summary

contrib_wide %>% pivot_longer(c(-date, -receptor_id)) %>% 
  filter(name %in% c('ambient', 'coal_contribution_best_fit'), grepl('Menteng', receptor_id)) %>% 
  ggplot(aes(date, value, col=name)) + geom_line()

contrib_wide %>% filter(grepl('Menteng', receptor_id)) %>% 
  ggplot(aes(ambient,coal_contribution_best_fit))+geom_point()+geom_abline()


