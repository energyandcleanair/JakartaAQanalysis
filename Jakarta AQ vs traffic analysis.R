require(tidyverse)
require(magrittr)
require(creahelpers)
require(rcrea)

source('read_PM_data.R')

lang='ID'

list.files(path='data', '_Region_Mobility_Report.csv$', full.names = T) %>% 
  lapply(read_csv) %>% bind_rows() ->
  mob

macet <- readRDS('data/macet.RDS')
rcrea::transport.tomtom_congestion(cities=tibble(city="Jakarta", country="ID")) -> macet
saveRDS(macet, 'data/macet.RDS')


pm %>% group_by(date) %>%
  summarise(across(conc, mean)) %>%
  mutate(conc_24h=zoo::rollapplyr(conc, 24, mean, fill=NA),
         year=year(date), plotdate=date %>% 'year<-'(2022)) ->
  pm_plot

pm_plot %>% filter(year(date)>=2018) %>% 
  ggplot(aes(plotdate, conc_24h, col=as.factor(year))) + geom_line() +
  labs(title=trans('Jakarta PM2.5 concentrations by year'),
       y=trans('µg/m3'), x='', col=trans('year'),
       subtitle=trans('24-hour rolling mean')) +
  x_at_zero() + 
  scale_x_datetime(expand=expansion(), labels=function(x) trans(format.Date(x, '%b'))) +
  scale_color_brewer(palette='Reds') +
    theme_crea() -> p
quicksave(paste0('outputs/Jakarta PM2.5 concentrations, 24h mean, ',lang,'.png'), plot=p)  

pm_plot %>% write_csv('outputs/Jakarta PM2.5 concentrations, 24h mean.csv')


mob %<>% filter(sub_region_1=='Jakarta')

mob %>% pivot_longer(contains('change_from_baseline')) %>% 
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~name)

pm %>% 
  group_by(date=date(date), Site) %>% 
  summarise(across(conc, ~mean(.x, na.rm=T))) %>% 
  full_join(mob) -> 
  alldata

pm %>% 
  group_by(date=date(date-6*3600), Site) %>% 
  summarise(across(c(conc_morning_to_morning=conc), ~mean(.x, na.rm=T))) %>% 
  full_join(alldata) -> 
  alldata

macet %>% mutate(date=ymd(date)) %>% select(date, congestion=value) %>% 
  full_join(alldata, .) -> alldata

alldata %>% write_csv('outputs/alldata.csv')

alldata %>% 
  pivot_longer(matches('change_from_baseline|congestion')) %>% 
  ggplot(aes(value, conc, col=Site)) + geom_point() + geom_smooth(method='lm') + facet_wrap(~name)

indys <- names(alldata) %>% grep('change_from_baseline', ., value=T)
indys <- names(alldata) %>% grep('(workplaces).*change_from_baseline', ., value=T)
indys <- names(alldata) %>% grep('congestion', ., value=T)

alldata %>% filter(Site=='Jakarta South') %>% 
  lm(data = ., formula = as.formula(paste('conc_morning_to_morning ~ as.factor(month(date)) + as.factor(wday(date)) * ', paste(indys, collapse='+')))) %>% summary

indys <- names(alldata) %>% grep('(workplaces|transit|parks|recreation|parks).*change_from_baseline', ., value=T)

alldata %>% filter(Site=='Jakarta Central') %>% 
  lm(data = ., formula = as.formula(paste('conc_morning_to_morning ~ as.factor(month(date)) + ', paste(indys, collapse='+')))) %>% summary


alldata %>% 
  pivot_longer(matches('change_from_baseline|congestion|conc$|conc_morning')) %>% 
  mutate(value = case_when(grepl('change_from_base', name)~value+100, T~value),
         conc_basis = case_when(grepl('morning', name)~'morning to morning', grepl('conc', name)~'midnight to midnight', T~'none'),
         name = case_when(grepl('^conc', name)~paste0('PM2.5, ', Site, ' (µg/m3)'),
                          name=='congestion'~'congestion (TomTom traffic index)',
                          T~paste0('Visits to ', 
                                   name %>% gsub('_percent.*', '', .) %>% gsub('_', ' ', .) %>% gsub('recreation', 'recreation venues', .),
                                   ',\npercent of baseline'))) %>% 
  filter(date<='2023-08-01', !is.na(value)) ->
  plotdata

plotdata %>% filter(grepl('congestion|PM2.5', name), !grepl('midnight', conc_basis)) %>% 
  group_by(name, weekday=wday(date, week_start=1, label=T,abbr=T)) %>% 
  summarise(across(value, mean, na.rm=T)) -> weekday_plot

weekday_plot %>% 
  ggplot(aes(trans(weekday), value, fill=grepl('PM', name))) + geom_col() + facet_wrap(~trans(name)) +
  x_at_zero() +
  theme_crea() +
  scale_fill_crea_d('dramatic', guide='none', col.index=c(4,3)) +
  labs(title=trans('Jakarta average PM2.5 and congestion levels by weekday'),
       subtitle=trans('Data for January 2021 to July 2023'), y='', x='',
       caption=trans('Daily PM2.5 concentrations are calculated from 06:00am until 06:00am the following day to capture the impact of evening traffic on nighttime pollution.')) -> 
  p
quicksave(paste0('outputs/Jakarta average PM2.5 and congestion levels by weekday, ',lang,'.png'), plot=p, footer_height=.03)

weekday_plot %>% group_by(name) %>% summarise(Sat_to_Sun=value[weekday=='Sun']/value[weekday=='Sat']-1,
                                              Fri_to_Mon=value[weekday=='Mon']/value[weekday=='Fri']-1)

plotdata %>% filter(grepl('congestion|PM2.5', name), !grepl('morning', conc_basis), year(date)>=2021) %>% 
  filter(month(date) %in% 1:7) %>% 
  group_by(name, year=as.factor(year(date))) %>% 
  summarise(across(value, ~mean(.x, na.rm=T))) ->
  yearly_congestion_plot

yearly_congestion_plot %>% 
  ggplot(aes(year, value, fill=grepl('PM', name))) + geom_col() + facet_wrap(~trans(name), scales='free_y') +
  x_at_zero() +
  theme_crea() +
  scale_fill_crea_d('dramatic', guide='none', col.index=c(4,3)) +
  labs(title=trans('Jakarta average PM2.5 and congestion levels by year'),
       subtitle=trans('January–July averages'),
       y='', x='') -> p
quicksave(paste0('outputs/Jakarta average PM2.5 and congestion levels by year, ',lang,'.png'), plot=p, footer_height=.03)

yearly_congestion_plot %>% group_by(name) %>% summarise(change=value[year==2023]/value[year==2021]-1)

plotdata %>% filter(grepl('workplace|retail|parks|PM2.5', name), !grepl('morning', conc_basis), year(date) %in% 2020:2022) %>% 
  group_by(name, year=as.factor(year(date))) %>% 
  summarise(across(value, ~mean(.x, na.rm=T))) ->
  yearly_mobility_plot

yearly_mobility_plot %>% 
  ggplot(aes(year, value, fill=ifelse(grepl('PM', name), 'PM', name))) + geom_col() + 
  facet_wrap(~trans(name, wrap_chars=40), scales='free_y') +
  x_at_zero() +
  theme_crea() +
  scale_fill_crea_d('dramatic', guide='none', col.index=c(3, 1, 2, 4)) +
  labs(title=trans('Jakarta average PM2.5 and mobility by year'),
       #subtitle='January–July averages',
       caption=trans('Visits to different venues based on Google Mobility Report'),
       y='', x='') -> p
quicksave(paste0('outputs/Jakarta average PM2.5 and mobility by year, ',lang,'.png'), plot=p)

yearly_mobility_plot %>% group_by(name) %>% summarise(change=value[year==2022]/value[year==2020]-1)
