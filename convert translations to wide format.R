read.table('clipboard', sep='\t', header=T) -> t1

t1 %>% pivot_longer(-c(Language, Fig)) %>% #na.omit %>% 
  pivot_wider(names_from=Language) %>% 
  filter(EN!="") -> lt

lt %<>% mutate(is_list=grepl(',', EN) & !grepl('Source:|per kilometer', EN))

split_list <- function(x) {
  x %<>% gsub(', percent of baseline', 'XXX1', .)
  x %<>% gsub(', persen baseline', 'XXX2', .)
  x %<>% strsplit(', ') %>% unlist
  x %<>% gsub('XXX1', ', percent of baseline', .)
  x %<>% gsub('XXX2', ', persen baseline', .)
}

lt %>% group_by(Fig, name, is_list) %>% 
  group_modify(function(df, group) {
    if(!group$is_list) return(df)
    
    tibble(EN=df$EN %>% split_list,
           ID=df$ID %>% split_list)
  }) ->
  lt_split

lt_split %>% ungroup %>% 
  select(EN, ID) %>% write.table('clipboard', sep='\t', row.names=F)
