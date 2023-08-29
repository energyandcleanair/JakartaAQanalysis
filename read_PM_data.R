list.files(path='data', '^Jakarta.*_PM2\\.5_.*\\.csv$', full.names = T) %>% 
  lapply(read_csv) %>% bind_rows() ->
  pm

names(pm) %<>% make.names

pm %<>% rename(date=Date..LT., conc=Raw.Conc.) %>% 
  mutate(conc=ifelse(conc<0 | conc==985, NA, conc),
         date=ymd_hm(date))

tz(pm$date) <- "Asia/Jakarta"
