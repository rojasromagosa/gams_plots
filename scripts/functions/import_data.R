#' IMPORT DATA
#' Use a general function to import data.
#' There are two options: form="full" or long format. The first one is nice because it keeps the values of the dimensions,
#' but it gets a bit messy with more than two dimensions, as need to flatten the array...
#' It is seems better to import long and then match the names


#' this function can be used to import data
#' it works for all variables that require the variable to be multiplied by variable0, and then divided by the scalar
import.data <- function(gdx_name, var_name, inScale){
  # ~~~~~~~~~~~~
  # debug start: 
  # gdx_name <- input_files[1]
  # var_name <- "rgdpmp"
  # inScale <- var_rescale
  # debug end
  # ~~~~~~~~~~~~
  
  #' write a function that reads in the data and matches the dimensions names:
  #' We can use this function to read in the variable, and also to read in variable0
  custom.rgdx <- function(x){
    #' debug: x = "rgdpmp"
    #' 1) read in file
    d_tmp <- rgdx( file.path(input_dir, gdx_name),
                   list(name = x))
    #' 2) extract long table with data
    dom_id <- paste0(d_tmp$domains,"_id") # suffix used to drop columns below
    d <- d_tmp$val %>%
      as_tibble( .name_repair = ~ c(dom_id , "value"))
    
    #' 3) get labels
    d_lab <-   as_tibble(cbind(
      expand.grid( sapply(d_tmp$uels, function(x) x %>% as_factor %>% as.numeric  )), # numeric indexes combinations
      expand.grid( d_tmp$uels, stringsAsFactors = F) # real names combinations
    ),
    .name_repair = ~ c( dom_id, d_tmp$domains))
    
    #' 4) merge labels to data
    d %<>% left_join( .,
                      d_lab, 
                      by = dom_id ,
                      all = T) %>% select( !ends_with("_id")) # drop indexes
    return(d)
  }
  
  
  #' import data
  d <- custom.rgdx(x = var_name)
  
  #' multiply by "variable0" if "variable0" actually exists (what if we need to multiply by something different?)
  if(paste0(var_name, "0") %in% d_meta$name){
    d_tmp0 <- custom.rgdx(x = paste0(var_name, "0")) # read in data
    n = intersect( names(d_tmp0),names(d))
    d %<>%
      left_join(.,
                d_tmp0,
                by = n[n != "value"],
                suffix = c("", "_0")) %>% 
      mutate(value = value * value_0) %>% 
      select(-value_0)
  }
  
  #' if we need to re-sclale the values
  if(inScale!="NULL"){ # this is ugly, find better solution
    scale <- rgdx( file.path(input_dir, gdx_name),
                   list(name = inScale, form = "full"))$val
    d %<>%
      mutate(value = value/scale)
  }
  
  #' need to change year into numeric (it is stored as character in the gdx file)
  d %<>% mutate(t = as.integer(t))
  
  
  #' add names of gdx and variable
  d %<>% mutate(var=var_name, sim= gsub("\\.gdx", "", gdx_name))
  
  #' MANAGE files have no r dimension. We add it "manually" so that the data can be
  #' processed in the dame way as the ENVISAGE data.
  #' NOTE: will have to change this and read in a parameter "r" from the gdx file
  if(! "r" %in% names(d)){
    d %<>% mutate(r="USA")
  }
  
  #' end of processing data
  return(d)
}
