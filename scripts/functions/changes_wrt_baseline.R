#' CHANGES to BASELINE
#' the data to be passed in has all the simulations in long form. We want:
#' - compute growth rates for all in "sim" and all dimensions
#' - compute changes relative to BaU
changes.wrt.baseline <- function(x){
  # debug: x <- d
  # 1) compute growth rates
  x %<>%
    arrange(t) %>% 
    group_by_at(vars(-t, -value)) %>%  # group by every dimension except for time, and value
    mutate(value_g = 100*(value-lag(value))/lag(value)) %>% 
    ungroup()
  
  # add baseline as row
  names_by <- setdiff(names(x), c("sim","value", "value_g"))
  x <- left_join(x,
                 x %>% filter(grepl("^bau$", tolower(sim))) %>% select(-sim, -value_g),
                 by = names_by,
                 suffix = c("", "_bau"))
  
  #' percentage change w.r.t. baseline
  x %<>%
    mutate(change_per = 100*(value - value_bau)/value_bau) %>% 
    mutate(change_lev = value - value_bau)
  
  #' output
  return(x)
}
