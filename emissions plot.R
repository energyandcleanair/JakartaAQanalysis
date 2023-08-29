read_csv('data/emissions_100_200km.csv') -> emis_all

theme_crea() -> ct
ct$panel.grid.major.y

emis_all %>% group_by(distance_km, poll) %>% 
  mutate(share=value_tonne/sum(value_tonne)) %>% 
  group_by(sector_long) %>% 
  filter(distance_km==200, poll %in% c('SO2', 'NOx', 'PM')) %>% 
  mutate(sector_plot = ifelse(max(share)>.05, sector_long, 'Others')) %>% 
  group_by(distance_km, sector_plot, poll) %>% 
  summarise(across(c(value_tonne, share), sum)) %>% 
  ungroup() %>% 
  mutate(label_placement = value_tonne + ifelse(value_tonne>max(value_tonne)*.75, -250e3, 250e3),
         poll = factor(poll, c('PM', 'SO2', 'NOx'))) ->
  emisplot

emisplot %>% group_by(sector_plot) %>% summarise(across(value_tonne, sum)) %>% arrange(value_tonne) -> sector_ranking

emisplot %>% 
  ggplot(aes(trans(sector_plot), value_tonne/1e3)) + facet_wrap(~poll) +
  geom_col(aes(fill=sector_plot)) + coord_flip() +
  geom_label(aes(y=label_placement/1000, label=scales::percent(share, accuracy=1))) +
  theme_crea() +
  theme(panel.grid.major.x=element_line(linewidth = .1), panel.grid.major.y = element_blank(),
        panel.border = element_rect(linewidth = .1)) +
  scale_fill_crea_d('dramatic', guide='none') +
  scale_x_discrete(limits=trans(sector_ranking$sector_plot)) +
  x_at_zero(headroom=.3) +
  labs(title=trans('Air pollutant emissions in Jakarta and surroundings'),
       subtitle=trans('total emissions by sector within 200km of Jakarta'),
       y=trans('kt/year'), x='') -> p
quicksave(paste0('outputs/Air pollutant emissions in Jakarta and surroundings, ',lang,'.png'), plot=p, footer_height=.03)
