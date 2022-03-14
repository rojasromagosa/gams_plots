
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
         aggregate = if_else(is.na(aggregate), 0, aggregate),
         theme = if_else(is.na(theme), "my_theme3", theme),
         simulations = if_else(is.na(simulations),
                               input_files_names %>% list,
                               str_split(simulations, ',') %>% lapply(., str_trim)),
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
  d_3dim  %>%
    filter( aggregate == 0 ) %>%
    mutate( aggr = ""),  # list of variables with "new" 
  d_3dim %>%
    filter( aggregate == 1 ) %>%
    mutate(domnames_v = gsub("^a|a$|(?<=-)a(?=-)", "aga", domnames_v, perl = T))  %>%  # substitute a with aga, keeping track of different possible positions of a
    # insert here other aggregation types, following example above
    mutate( aggr = "aggr_")
)

#' Chart names
#' For a given variable, if we have the same plot with different years or styles, we need to create different names
d_3dim %<>%
  mutate(chart_name = paste0(aggr,
                             variable_name,
                             "_",
                             d_labels$new[match(chart_type, d_labels$old)])) %>% 
  rowwise() %>% 
  mutate(chart_name = if_else(grepl("_oneyear_bar$", chart_type), gsub("1yb$", year_span[[1]], chart_name), chart_name)) %>%  # sub in the year if oneyear_chart
  #' in case chart names are not unique:
  group_by(chart_name) %>%
  mutate(index = row_number()) %>% 
  mutate(chart_name = if_else(index>1, paste0(chart_name, index-1), chart_name)) %>% 
  select(-index) %>% 
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
#' (we do this to keep things simple, instead of adding data later) 
d_3dim_v <- d_3dim %>% 
              select(variable_name, variable_label, rescale, compute_shares, export_var_data, domnames_v, aggr) %>%
              unique()


read_in_all_data <- function(var_name,
                             var_rescale,
                             compute_shares,
                             var_dimensions){
  
  # ~~~~~~~~~~~~
  # debug start: 
  # var_name <- "xp"
  # var_label <- "test name"
  # var_rescale <- "inScale"
  # compute_shares <- 1
  # var_dimensions <- "aga-t" # "a-t"
  # 
  # var_name <- "lsT"
  # var_label <- "test name"
  # var_rescale <- "inScale"
  # compute_shares <- 0
  # var_dimensions <- "l-t"
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
  #' WiP: how do we do this if there are other types of aggregation?
  if(var_dimensions=="aga-t"){
  d %<>%
    mutate( var3 = d_agg$var_aggr[match(var3, d_agg$var)]) %>% 
    #group_by(r,t,var,sim, var3) %>%   # everything but value?
    group_by(across(c(-value))) %>%  
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


d %<>%
  mutate( sim_l = d_labels$new[match(d$sim, d_labels$old)] ) %>% 
  mutate( sim_l = ifelse(is.na(sim_l), sim, sim_l)) %>% 
  mutate( var3_l = d_labels$new[match(d$var3, d_labels$old)] ) %>% 
  mutate( var3 = ifelse(is.na(var3_l), var3, var3_l) )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make charts  ----

#' Define a function that processes one variable-dimension at a time. We also differentiate by
#' dimension to keep track of whether a dimension is aggregated or not.

plot.var.3dim <- function(var_tmp, dimension){

  #debug: var_tmp = "xp"
  #debug: dimension = "aga-t"
  
  #debug: var_tmp = "lsT"
  #debug: dimension = "l-t"
  
  # list of charts to be created for a given variable x dimension
  d_3dim_tmp <- d_3dim %>% filter(variable_name==var_tmp,
                                  domnames_v==dimension)
  # Get all data for a given variable x dimension
  d_tmp <- d %>% filter(var==var_tmp,
                        domnames_v==dimension)
  
  #' From here on var_tmp is used only to create the right paths
  #' We update it to include "_agg" if we are dealing with an aggregated dimension
  var_tmp <- paste0(var_tmp,
                    d_3dim_v %>%
                      filter(variable_name==var_tmp,
                             domnames_v==dimension) %>% 
                      pull(aggr)
  )
  
  # create a folder where to store the plots 
  dir.create(file.path(chart_dir, folder_name, var_tmp), showWarnings = F)
  
  #' save data if required
  if(d_3dim_tmp$export_var_data[1]==1){
    write.csv(d_tmp,
              file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, ".csv") ),
              row.names = F)
  }
  

  
  #' Define a function that, for a given variable, will process all the rows 
  #' in the Excel input file. For each row, it checks which chart to create
  all.charts <- function(chart_type,year_span,theme,export_chart_data, chart_name, simulations, d_tmp=d_tmp, var_tmp=var_tmp){
    # ~~~~~~~~~~~~
    # debug start:
    # ~~~~~~~~~~~~
    # chart_type = "bau_level_bar"
    # chart_type = "sim_%bau_oneyear_bar"
    # chart_type = "sim_%bau_allyears_bar"
    # year_span = 2020
    # year_span = c(2025, 2030)
    # theme = "my_theme3"
    # export_chart_data = 1
    # chart_name = "test_chart"
    # simulations = input_files_names
    # ~~~~~~~~~~~
    # debug end
    # ~~~~~~~~~~~
    
    # we need to look into the parents environment for the chart extension
    # the alternative would be to pass it in as an argument? Note <<- is the superassignment operator
    chart_ext <<- chart_ext
    
    # use the selected theme
    gg_theme <- get(theme)
    
    if (chart_type=="bau_level_bar"){
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # baseline: bar chart with all levels on x dimension, one bar for each year
      map( d_tmp$r %>% unique,
           function(x){
             # debug: x="IDN"
             d_chart <- d_tmp %>%
               filter(r == x,
                      sim == "BaU",
                      t %in% year_span)
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$var3 %>% unique %>% length
             uni_fill <- d_chart$t %>% unique %>% length # used to check whether need to delete legend
             
             
             ggplot(d_chart, aes(x=var3, y=value, fill=factor(t))) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Baseline" ),
                    x = NULL,
                    y = ifelse(compute_shares==T, "share of total", "Baseline value" ), 
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors) + delete.legend(uni_fill)
             
             # save
             custom.save.f(my_theme = gg_theme,
                         filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                         factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name))}
           }
      )
    } else if (chart_type=="sim_%bau_oneyear_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the % changes wrt baseline, for each level
      map( d_tmp$r %>% unique,
           function(x){
             
             if(length(year_span)>1)stop("For sim_%bau_oneyear_bar only one year can be used!")
             # debug: x="IDN"
             d_chart <- d_tmp %>%
               filter(r == x,
                      sim %in% simulations,
                      sim != "BaU",
                      t %in% year_span)
             
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$var3 %>% unique %>% length # this is probably not enough, if we have many simulations
             uni_fill <- d_chart$sim_l %>% unique %>% length # used to check whether need to delete legend
             
             ggplot(d_chart, aes(x=var3, y=change_per, fill=sim_l)) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Simulations (", year_span, ")" ),
                    x = NULL,
                    y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors) + delete.legend(uni_fill)
             
             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name))}
           }
      )
    } else if (chart_type=="sim_%bau_allyears_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the % changes wrt baseline, for each level
      map( d_tmp$r %>% unique,
           function(x){
             # debug: x="IDN"
             d_chart <- d_tmp %>%
               filter(r == x,
                      sim %in% simulations,
                      sim != "BaU",
                      t %in% year_span)
             
             # we need to create new labels: yearXsim
             d_chart %<>% mutate(lab = paste0(t, "\n", sim_l))
             
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$lab %>% unique %>% length # this is probably not enough, if we have many simulations
             uni_fill <- d_chart$var3 %>% unique %>% length # used to check whether need to delete legend
             
             
             ggplot(d_chart, aes(x=lab, y=change_per, fill=var3)) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Simulations" ),
                    x = NULL,
                    y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors) + delete.legend(uni_fill)
             
             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name))}
           }
      )
    } else if (chart_type=="sim_levdiff_oneyear_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the level changes wrt baseline, for each level
      map( d_tmp$r %>% unique,
           function(x){
             
             if(length(year_span)>1)stop("For sim_%bau_oneyear_bar only one year can be used!")
             # debug: x="IDN"
             d_chart <- d_tmp %>%
               filter(r == x,
                      sim %in% simulations,
                      sim != "BaU",
                      t %in% year_span)
             
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$var3 %>% unique %>% length # this is probably not enough, if we have many simulations
             uni_fill <- d_chart$sim_l %>% unique %>% length # used to check whether need to delete legend
             
             
             ggplot(d_chart, aes(x=var3, y=change_lev, fill=sim_l)) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Simulations (", year_span, ")" ),
                    x = NULL,
                    y = paste0("change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors) + delete.legend(uni_fill)
             
             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name))}
           }
      )
    } else if (chart_type=="sim_levdiff_allyears_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the level changes wrt baseline, for each level
      map( d_tmp$r %>% unique,
           function(x){
             # debug: x="IDN"
             d_chart <- d_tmp %>%
               filter(r == x,
                      sim %in% simulations,
                      sim != "BaU",
                      t %in% year_span)
             
             # we need to create new labels: yearXsim
             d_chart %<>% mutate(lab = paste0(t, "\n", sim_l))
             
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$lab %>% unique %>% length # this is probably not enough, if we have many simulations
             uni_fill <- d_chart$var3 %>% unique %>% length # used to check whether need to delete legend
             
             
             ggplot(d_chart, aes(x=lab, y=change_lev, fill=var3)) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Simulations" ),
                    x = NULL,
                    y = paste0("change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors) + delete.legend(uni_fill)
             
             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name))}
           }
      )
    }  # add new chart here: else if (chart_type=="new chart name")
    
    #' Finally close and save the Excel file containing all the charts data for
    #' a given variable
    #' Note: we do this only if the workbook contains at least one sheet
    # if(length(openxlsx::sheets(wb))>0){
    # openxlsx::saveWorkbook(wb,
    #                        file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, "_charts.xlsx") ),
    #                        overwrite = TRUE) 
    # }
  }


  # now apply the chart function
  pmap(list(
    chart_type = d_3dim_tmp$chart_type,
    year_span = d_3dim_tmp$year_span,
    theme = d_3dim_tmp$theme,
    export_chart_data = d_3dim_tmp$export_chart_data,
    chart_name = d_3dim_tmp$chart_name,
    simulations = d_3dim_tmp$simulations
  ),
  all.charts,
  d_tmp=d_tmp, # pass in data
  var_tmp=var_tmp # pass in the variable name
  )
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot all variables  ----

#' now we can run the function for all of the 
#' variable X dimension pairs
map2( d_3dim_v$variable_name,
      d_3dim_v$domnames_v,
     plot.var.3dim)





