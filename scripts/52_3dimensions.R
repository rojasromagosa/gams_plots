
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description  ----

#' this file can process variables of two dimensions


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process Excel input file  ----

#' First we need to fill in some values at the variable level
d_3dim %<>%
  fill(variable_name, variable_label, .direction = "down")

d_3dim %<>% 
  group_by(variable_name) %>% 
  fill(rescale, export_var_data, compute_shares, .direction = "down")

#' Deal with chart parameters
d_3dim %<>%
  mutate(rescale = if_else(is.na(rescale), "inScale", rescale), # if empty use inScale
         export_var_data = if_else(is.na(export_var_data), 0, export_var_data),
         compute_shares = if_else(is.na(compute_shares), 0, compute_shares),
         theme = if_else(is.na(theme), "my_theme3", theme),
         export_chart_data = if_else( is.na(export_chart_data), 0, export_chart_data))# if empty dont save

#' Process selected years for each chart
d_3dim %<>% rowwise %>% mutate(year_span = year.fix(year_span, chart_type) %>% list)

#' first add some information about the variables listed in the excel file
d_3dim <- merge(
  d_3dim,
  d_meta %>% select(name, domnames_v),
  by.x = "variable_name",
  by.y = "name",
  all.x = T
)


#' For some variable we also need to aggregate the data.
#' For instance, variables with dimensions "r-a-t" need to also be processed
#' by aggregating "a" to "aga", which is aggregate ag
#' Therefore, once we get a list of variables, which check in the  "d_meta" data frame
#' whether the variable has dimension "r-a-t". If yes we add one row to the list of variables
#' but with dimension "r-aga-t". In the function below, there is an ifelse condition which does the aggregation when necessary,
#' before shares and changes are computed.
#' This process can be in theory expanded to have different levels of aggregation, or for different variables
d_3dim <- bind_rows(
  # list of variables
  d_3dim,  # list of variables with "new" dimension
  d_3dim %>% 
    filter(domnames_v=="a-t") %>% 
    mutate(domnames_v="aga-t")
)

#' For a given variable, if we have the same plot with different years or styles, we need to create different names
d_3dim %<>%
  group_by(variable_name, chart_type) %>%
  mutate(chart_name = row_number() -1) %>% 
  mutate(chart_name = paste0(variable_name, "_", gsub("\\%", "", chart_type), if_else(chart_name>0, as.character(chart_name), ""))) %>% 
  mutate(chart_name = if_else(grepl("aga", domnames_v), paste0("agg_", chart_name), chart_name)) %>% 
  ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables with 3 dimensions  ----

#' check everything is correct:
#' - the variables have correct names
#' - the varaibles have indeed dimension 3
if(any(is.na(d_3dim$domnames_v))){stop("some variables could not be found. Look for typos in the Excel file")}
 


folder_name <- "3dim"
dir.create(file.path(chart_dir, folder_name), showWarnings = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in data  ----


#' Data frame with variables to read in
#' Note that we need to read in some variables twice, if the third dimension can be aggregated 
d_3dim_v <- d_3dim %>% 
              select(variable_name, variable_label, rescale, compute_shares, export_var_data, domnames_v) %>%
              unique()


read_in_all_data <- function(var_name,
                             var_rescale,
                             compute_shares,
                             var_dimensions){
  
  # ~~~~~~~~~~~~
  # debug start: 
  # var_name <- "xp"  # , px, u, xp
  # var_label <- "test name"
  # var_rescale <- NA
  # compute_shares <- 1
  # var_dimensions <- "aga-t"
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
  
  #' we need to rename the third variable, so that the function can work with different dimensions
  var_3rd <- setdiff(names(d), c("value","r","t","var","sim"))
  d %<>% rename(var3 = all_of(var_3rd))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3) Aggregate data if necessary (can be expanded)
  #' Note: this is different for MANAGE, as we need to match into maprep
  #' Acutally just need to harmonize the name of the dataframe containing the aggregation, and the columns in the data
  if(var_dimensions=="aga-t"){
  d %<>%
    mutate( var3 = d_agg$var_aggr[match(var3, d_agg$var)]) %>% 
    group_by(r,t,var,sim, var3) %>% 
    summarise( value = sum(value)) %>% 
    ungroup()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Compute shares if necessary
  if(compute_shares==T){
    d %<>%
      group_by(var, sim, r, t) %>%
      mutate(value = value/sum(value))
  }  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 5) Compute growth rates, and changes wrt baseline
  d %<>% changes.wrt.baseline
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 6) Add columns
  d %<>% mutate(domnames_v = var_dimensions,
                compute_shares = compute_shares)
}

d <- pmap_dfr(list(
      var_name = d_3dim_v$variable_name,
      var_rescale = d_3dim_v$rescale,
      compute_shares = d_3dim_v$compute_shares,
      var_dimensions = d_3dim_v$domnames_v
    ),
    read_in_all_data
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make charts  ----


#' Define a function that, for a given variable, will process all the rows 
#' in the Excel input file. For each row, it checks which chart to create
all.charts <- function(chart_type,year_span,theme,export_chart_data, chart_name, d_tmp=d_tmp, var_tmp=var_tmp){
  # ~~~~~~~~~~~~
  # debug start:
  # ~~~~~~~~~~~~
  # chart_type = "bau_level_bar"
  # chart_type = "sim_%bau_bar"
  # year_span = 2020
  # year_span = c(2025, 2030)
  # theme = "my_theme3"
  # export_chart_data = 1
  # chart_name = "test_chart"
  # ~~~~~~~~~~~
  # debug end
  # ~~~~~~~~~~~
  
  # use the selected theme
  gg_theme <- get(theme)
  
  if (chart_type=="bau_level_bar"){
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # baseline: bar chart with all levels on x dimension, one bar for each year
    map( d_tmp$r %>% unique,
         function(x){
           # debug: x="USA"
           d_chart <- d_tmp %>%
             filter(r == x,
                    sim == "BaU",
                    t %in% year_span)
           
           compute_shares <- d_chart$compute_shares[1]
           uni_factors <- d_chart$var3 %>% unique %>% length
           
           ggplot(d_chart, aes(x=var3, y=value, fill=factor(t))) +
             geom_col(position = position_dodge()) +
             scale_y_continuous(n.breaks = 8) +
             theme(legend.position="top",
                   legend.title=element_blank()) +
             labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                  subtitle = paste0(x, ", Baseline" ),
                  x = NULL,
                  y = ifelse(compute_shares==T, "% of total", "Baseline value" ), 
                  fill = NULL) +
             gg_theme +
             if(uni_factors>6){theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))}

           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                   units = "in",
                   scale = 0.8,
                   height = 5,
                   width = 8* (uni_factors/4)^0.2  )
         }
    )
  } else if (chart_type=="sim_%bau_bar"){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # simulation: for a given year, show the changes wrt baseline, for each level
    map( d_tmp$r %>% unique,
         function(x){
           # debug: x="USA"
           d_chart <- d_tmp %>%
             filter(r == x,
                    sim != "BaU",
                    t %in% year_span)
           
           compute_shares <- d_chart$compute_shares[1]
           uni_factors <- d_chart$var3 %>% unique %>% length # this is probably not enough, if we have many simulations
           
           ggplot(d_chart, aes(x=var3, y=change, fill=sim)) +
             geom_col(position = position_dodge()) +
             scale_y_continuous(n.breaks = 8) +
             theme(legend.position="top",
                   legend.title=element_blank()) +
             labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                  subtitle = paste0(x, ", Simulations" ),
                  x = NULL,
                  y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                  fill = NULL) +
             gg_theme +
             if(uni_factors>6){theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))}
           
           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                   units = "in",
                   scale = 0.8,
                   height = 5,
                   width = 8* (uni_factors/4)^0.2  )
         }
    )
  }
}


plot.var.3dim <- function(var_tmp, dimension){

  #debug: var_tmp = "xp"
  #debug: dimension = "a-t"
  
  # create a folder where to store the plots 
  dir.create(file.path(chart_dir, folder_name, var_tmp), showWarnings = F)
  
  # For a specific variable and associated dimension subset the data
  d_3dim_tmp <- d_3dim %>% filter(variable_name==var_tmp,
                                  domnames_v==dimension)
  d_tmp <- d %>% filter(var==var_tmp,
                        domnames_v==dimension)
  
  # save data if required
  if(d_3dim_tmp$export_var_data[1]==1){
    write.csv(d_tmp,
              file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, ".csv") ),
              row.names = F)
  }


  # now apply the chart function
  pmap(list(
    chart_type = d_3dim_tmp$chart_type,
    year_span = d_3dim_tmp$year_span,
    theme = d_3dim_tmp$theme,
    export_chart_data = d_3dim_tmp$export_chart_data,
    chart_name = d_3dim_tmp$chart_name
  ),
  all.charts,
  d_tmp=d_tmp, # pass in data
  var_tmp=var_tmp # pass in the variable name
  )
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot all variables  ----

#' now we can run the function for all of the variables in the list
map2( d_3dim_v$variable_name,
      d_3dim_v$domnames_v,
     plot.var.3dim)





