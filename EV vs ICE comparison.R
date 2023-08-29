output_dir = 'outputs'

read.table('clipboard', header=T, sep='\t') -> comp

custom_limits <- tibble(name=c('PM', 'SO2', 'NOx', 'CO2'),
                        value=c(.25, .25, NA, NA),
                        Vehicle='EV')

comp %>% pivot_longer(is.numeric) %>% 
  ggplot(aes(trans(Vehicle), value)) + 
  facet_wrap(~factor(name, levels=custom_limits$name), scales='free', nrow=1) +
  geom_col(aes(fill=Vehicle)) + 
  geom_point(data=custom_limits, shape=NA) +
  scale_fill_crea_d('dramatic', guide='none') + theme_crea() + x_at_zero() +
  labs(y='g/km', x='', title=trans('Air pollutant and CO2 emissions from gasoline and electric cars in Java-Bali grid'),
       subtitle=trans('per kilometer driven, at current average emission intensity of power generation')) -> p
quicksave(file.path(output_dir, paste0('ICE vs EV emission comparison, ',lang,'.png')), plot=p, footer_height=.03)
