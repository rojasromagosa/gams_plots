#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description  ----

#' this file can process variables of two dimensions


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process Excel input file  ----

d_5dim %<>%
  mutate(rescale = as.character(rescale)) %>% 
  mutate(rescale = if_else(is.na(rescale), "inScale", rescale))



folder_name <- "5dim"
dir.create(file.path(chart_dir, folder_name), showWarnings = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in all data  ----

read_in_all_data5 <- function(var_name,
                              var_rescale){
  
  # ~~~~~~~~~~~~
  # debug start: 
  # var_name <- "ued"
  # var_rescale <- "inScale"
  # debug end
  # ~~~~~~~~~~~~
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1) First we read in the data, using the custom function
  d <- map_dfr(input_files,
               function(x) import.data(gdx_name = x,
                                       var_name = var_name,
                                       inScale = var_rescale))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2) Format data
  #' if there is a variable named ra, then rename it r
  try(d %<>% rename(r = ra), silent = T)
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3) Compute growth rates, and changes wrt baseline
  d %<>% changes.wrt.baseline
  
  
}

d <- pmap_dfr(list(
  var_name = d_5dim$variable_name,
  var_rescale = d_5dim$rescale
),
read_in_all_data5
)
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save data  ----  
  
#' For now the function just saves the data in csv format

plot.var.5dim <- function(var_tmp){
  
  #debug: var_tmp = "ued"
  
  # create a folder where to store the plots 
  dir.create(file.path(chart_dir, folder_name, var_tmp), showWarnings = F)
  
  d_tmp <- d %>% filter(var==var_tmp)
  
  #' save data
    write.csv(d_tmp,
              file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, ".csv") ),
              row.names = F)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot all variables  ----

#' now we can run the function for all of the 
#' variable X dimension pairs
map( d_5dim$variable_name,
      plot.var.5dim)


  
  
  
  
  
  
  
  
  