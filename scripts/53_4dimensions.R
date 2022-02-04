#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description  ----

#' this file processes variables with 4 dimensions, r-var3-var3f-t
#' where var3f is a dimension that we keep fixed.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process Excel input file  ----

#' First we need to fill in some values at the variable level
d_4dim %<>%
  fill(variable_name, variable_label, .direction = "down")

d_4dim %<>% 
  group_by(variable_name) %>% 
  fill(rescale, export_var_data, compute_shares, .direction = "down")

#' Deal with chart parameters
d_4dim %<>%
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
d_4dim %<>% rowwise %>% mutate(year_span = year.fix(year_span, chart_type) %>% list)

#' first add some information about the variables listed in the excel file
d_4dim <- merge(
  d_4dim,
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

d_4dim <- bind_rows(
  # list of variables
  d_4dim  %>%
    filter( aggregate == 0 ) %>%
    mutate( aggr = ""),  # list of variables with "new" dimension
  d_4dim %>%
    filter( aggregate == 1 ) %>%
    mutate(domnames_v = gsub("^a|a$|(?<=-)a(?=-)", "aga", domnames_v, perl = T))  %>%  # substitute a with aga, keeping track of different possible positions of a
    mutate( aggr = "_aggr")
)



#' For a given variable, if we have the same plot with different years or styles, we need to create different names
d_4dim %<>%
  group_by(variable_name, chart_type) %>%
  mutate(chart_name = row_number() -1) %>% 
  mutate(chart_name = paste0(variable_name, "_", gsub("\\%", "", chart_type), if_else(chart_name>0, as.character(chart_name), ""))) %>% 
  mutate(chart_name = if_else(grepl("aga", domnames_v), paste0("agg_", chart_name), chart_name)) %>% 
  ungroup()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables with 4 dimensions  ----

#' check everything is correct:
#' - the variables have correct names
#' - the varaibles have indeed dimension 3
if(any(is.na(d_4dim$domnames_v))){stop("some variables could not be found. Look for typos in the Excel file")}



folder_name <- "4dim"
dir.create(file.path(chart_dir, folder_name), showWarnings = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in data  ----


#' Data frame with variables to read in
#' Note that we need to read in some variables twice, if the third dimension can be aggregated
#' (we do this to keep things simple, instead of adding data later) 
d_4dim_v <- d_4dim %>% 
  select(variable_name, variable_label, rescale, compute_shares, export_var_data, domnames_v, fixed_dimension, aggr) %>%
  unique()

read_in_all_data4 <- function(var_name,
                             var_rescale,
                             compute_shares,
                             var_dimensions,
                             fixed_dimension){
  
  # ~~~~~~~~~~~~
  # debug start: 
  # var_name <- "plab1"
  # var_label <- "test name"
  # var_rescale <- "inScale"
  # compute_shares <- 0
  # var_dimensions <- "a-v-t"
  # var_dimensions <- "aga-v-t"
  # fixed_dimension <- "v"
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
  
  #' we need to rename the third and fourth variable, so that the function can work with different dimensions
  #' we fix one variable by default
  var_3rd <- setdiff(names(d), c("value","r","t","var","sim", fixed_dimension))
  d %<>% rename_with(~ c("var3", "var3f"), all_of(c(var_3rd, fixed_dimension)))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3) Aggregate data if necessary (can be expanded)
  #' Note: this is different for MANAGE, as we need to match into maprep
  #' Acutally just need to harmonize the name of the dataframe containing the aggregation, and the columns in the data
  if( grepl("aga", var_dimensions)){
    d %<>%
      mutate( var3 = d_agg$var_aggr[match(d$var3, d_agg$var)]) %>%
      group_by(across(c(-value))) %>% 
      summarise( value = sum(value)) %>%
      ungroup()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Compute shares if necessary
  if(compute_shares==T){
    d %<>%
      group_by(var, sim, r, t, var3f) %>%
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
  var_name = d_4dim_v$variable_name,
  var_rescale = d_4dim_v$rescale,
  compute_shares = d_4dim_v$compute_shares,
  var_dimensions = d_4dim_v$domnames_v,
  fixed_dimension = d_4dim_v$fixed_dimension
),
read_in_all_data4
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make charts  ----

#' Define a function that processes one variable-dimension at a time. We also differentiate by
#' dimension to keep track of whether a dimension is aggregated or not.

plot.var.4dim <- function(var_tmp, dimension){
  
  #debug: var_tmp = "plab1"
  #debug: dimension = "aga-v-t"
  
  # list of charts to be created for a given variable x dimension
  d_4dim_tmp <- d_4dim %>% filter(variable_name==var_tmp,
                                  domnames_v==dimension)
  # Get all data for a given variable x dimension
  d_tmp <- d %>% filter(var==var_tmp,
                        domnames_v==dimension)
  
  #' From here on var_tmp is used only to create the right paths
  #' We update it 
  var_tmp <- paste0(var_tmp,
                    d_4dim_v %>%
                      filter(variable_name==var_tmp,
                             domnames_v==dimension) %>%
                      pull(aggr)
  )
  
  # create a folder where to store the plots 
  dir.create(file.path(chart_dir, folder_name, var_tmp), showWarnings = F)
  
  #' save data if required
  #' How not to write over when processing the aggregated variables?
  if(d_4dim_tmp$export_var_data[1]==1){
    write.csv(d_tmp,
              file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, ".csv") ),
              row.names = F)
  }
  
  #' Create Workbook where all plots for this variable will be saved
  # wb <- openxlsx::createWorkbook()
  
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
    
    # a function that adds a sheet to the workbook that is created
    custom.add.sheet <- function(d,  s_name){ # tmp_wb = wb,
      tmp_wb <<- wb
      openxlsx::addWorksheet(tmp_wb,
                             sheetName = s_name)
      openxlsx::writeData(tmp_wb,
                          sheet = s_name,
                          d)
    }
    
    # use the selected theme
    gg_theme <- get(theme)
    
    if (chart_type=="bau_level_bar"){
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # baseline: bar chart with all levels on x dimension, one bar for each year
      map2( d_tmp$r %>% unique,
           d_tmp$var3f %>% unique,
           function(x,y){
             # debug: x="IDN"
             # debug: y="Old"
             d_chart <- d_tmp %>%
               filter(r == x,
                      var3f == y,
                      sim == "BaU",
                      t %in% year_span)
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$var3 %>% unique %>% length
             
             ggplot(d_chart, aes(x=var3, y=value, fill=factor(t))) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, " ", y, ", Baseline" ),
                    x = NULL,
                    y = ifelse(compute_shares==T, "% of total", "Baseline value" ), 
                    fill = NULL) +
               gg_theme  + rot.axis(uni_factors)

             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, "_", y, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name, "_", y))}
           }
      )
    } else if (chart_type=="sim_%bau_oneyear_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the changes wrt baseline, for each level
      map2( d_tmp$r %>% unique,
            d_tmp$var3f %>% unique,
            
            function(x,y){
              # debug: x="IDN"
              # debug: y="Old"
              
              if(length(year_span)>1)stop("For sim_%bau_oneyear_bar only one year can be used!")
              
              d_chart <- d_tmp %>%
                filter(r == x,
                       var3f == y,
                       sim %in% simulations,
                       sim != "BaU",
                       t %in% year_span)
             
             compute_shares <- d_chart$compute_shares[1]
             uni_factors <- d_chart$var3 %>% unique %>% length # this is probably not enough, if we have many simulations
             
             ggplot(d_chart, aes(x=var3, y=change, fill=sim)) +
               geom_col(position = position_dodge()) +
               scale_y_continuous(n.breaks = 8) +
               labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                    subtitle = paste0(x, ", Simulations (", year_span, ")" ),
                    x = NULL,
                    y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                    fill = NULL) +
               gg_theme + rot.axis(uni_factors)
             
             # save
             custom.save.f(my_theme = gg_theme,
                           filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, "_", y, chart_ext) ),
                           factors = uni_factors)
             
             # add data to the Excel file
             if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name, "_", y))}
           }
      )
    } else if (chart_type=="sim_%bau_allyears_bar"){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # simulation: for a given year, show the changes wrt baseline, for each level
      map2( d_tmp$r %>% unique,
            d_tmp$var3f %>% unique,
            
            function(x,y){
              # debug: x="IDN"
              # debug: y="Old"
              
              d_chart <- d_tmp %>%
                filter(r == x,
                       var3f == y,
                       sim != "BaU",
                       sim %in% simulations,
                       t %in% year_span)
              
              # we need to create new labels: yearXsim
              d_chart %<>% mutate(lab = paste0(t, "\n", sim))
              
              compute_shares <- d_chart$compute_shares[1]
              uni_factors <- d_chart$lab %>% unique %>% length # this is probably not enough, if we have many simulations
              
              ggplot(d_chart, aes(x=lab, y=change, fill=var3)) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(n.breaks = 8) +
                labs(title = d_3dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # automatically takes first of vector
                     subtitle = paste0(x, ", Simulations (", y, ")" ),
                     x = NULL,
                     y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                     fill = NULL) +
                gg_theme + rot.axis(uni_factors)
              
              # save
              custom.save.f(my_theme = gg_theme,
                            filename = file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, "_", y, chart_ext) ),
                            factors = uni_factors)
              
              # add data to the Excel file
              if(export_chart_data==1){custom.add.sheet(d=d_chart, s_name=paste0(x, "_", chart_name, "_", y))}
            }
      )
    }
    
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
    chart_type = d_4dim_tmp$chart_type,
    year_span = d_4dim_tmp$year_span,
    theme = d_4dim_tmp$theme,
    export_chart_data = d_4dim_tmp$export_chart_data,
    chart_name = d_4dim_tmp$chart_name,
    simulations = d_4dim_tmp$simulations
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
map2( d_4dim_v$variable_name,
      d_4dim_v$domnames_v,
      plot.var.4dim)










