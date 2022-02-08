#' years selection:
#' - line charts use a sequence of years
#' - bar charts use a set of years.
#' If they are specified need to transform them in
year.fix <- function(y,chart){
  # debug: chart = "bau_%g_line"
  # debug: y = "2014-2030"
  if(!grepl('_line$|_bar$', chart))stop('year.fix: Invalid chart name, needs to end with _line or _bar.')
  
  if(grepl('_line$', chart)){ # if it is a line chart
    if(is.na(y)){
      tmp = years_all # take default t from gdx if not specified
    } else {
      tmp_y <- str_split(y, "-") %>% unlist
      tmp = tmp_y[1]:tmp_y[2] # create the right years sequence is specified in the form 2015-2019
    }
  } else if(
    grepl('_bar$', chart)){ # if it is a bar chart
    if(is.na(y)){
      tmp = years_5 # take default 5 years if not specified
    } else {
      tmp <- str_extract_all(y, '\\d{4}') %>% unlist %>% as.integer  # extract all years
    }
  }
  return(tmp)
}