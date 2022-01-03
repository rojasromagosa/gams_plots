 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description  ----

#' this file can process variables of two dimensions


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process Excel input file  ----

#' First we need to fill in some values at the variable level
d_2dim %<>%
  fill(variable_name, variable_label, .direction = "down")

d_2dim %<>% 
  group_by(variable_name) %>% 
  fill(rescale, export_var_data, group, .direction = "down")

#' Deal with chart parameters
d_2dim %<>%
  mutate(rescale = if_else(is.na(rescale), "inScale", rescale), # if empty use inScale
         export_var_data = if_else(is.na(export_var_data), 0, export_var_data),
         theme = if_else(is.na(theme), "my_theme3", theme),
         export_chart_data = if_else( is.na(export_chart_data), 0, export_chart_data))# if empty dont save

#' Process selected years for each chart
d_2dim %<>% rowwise %>% mutate(year_span = year.fix(year_span, chart_type) %>% list)

#' For a given variable, if we have the same plot with different years or styles, we need to create different names
d_2dim %<>%
  group_by(variable_name, chart_type) %>%
  mutate(chart_name = row_number() -1) %>% 
  mutate(chart_name = paste0(variable_name, "_", gsub("\\%", "", chart_type), if_else(chart_name>0, as.character(chart_name), ""))) %>% 
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare data  ----

#' We read in all of the data. The import function takes three parameters
#' - gdx names
#' - variable name
#' - scaling variable
d_2dim_v <- crossing(gdx = input_files,
                           d_2dim %>%
                            select(variable_name, rescale) %>% 
                            unique)

d <- pmap_dfr(list(
  gdx_name = d_2dim_v$gdx,
  var_name = d_2dim_v$variable_name,
  inScale = d_2dim_v$rescale
),
import.data
)


d %<>% changes.wrt.baseline


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot single variables  ----

# create folders if not there yet
folder_name <- "2dim"
dir.create(file.path(chart_dir, folder_name), showWarnings = F)

map(d$var %>% unique,
    function(x){
      dir.create(file.path(chart_dir, folder_name, x), showWarnings = F)
    }
)

#' Define a function that, for a given variable, will process all the rows 
#' in the Excel input file. For each row, it checks which chart to create
all.charts <- function(chart_type,year_span,theme,export_chart_data, chart_name, d_tmp=d_tmp, var_tmp=var_tmp){
  # ~~~~~~~~~~~~
  # debug start:
  # ~~~~~~~~~~~~
  # chart_type = "bau_%g_line"
  # chart_type = "all_%g_line"
  # chart_type = "bau_level_line"
  # chart_type = "sim_%bau_line" 
  # chart_type = "sim_%bau_bar" 
  # year_span = 2020:2035
  # year_span = c(2025, 2030, 2040)
  # themev = "my_theme3"
  # export_chart_data = 1
  # ~~~~~~~~~~~
  # debug end
  # ~~~~~~~~~~~
  
  # use the selected theme
  gg_theme <- get(theme)
  
  if (chart_type=="bau_%g_line"){
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # baseline: line graph with growth rates
    map( d_tmp$r %>% unique,
         function(x){
           # debug: x="USA"
           d_chart <- d_tmp %>%
             filter(r == x,
                    sim == "BaU",
                    t != min(t),
                    t %in% year_span)
           
           ggplot(d_chart, aes(x=t, y=value_g)) +
             geom_line(size=0.8, color= "blue") +
             scale_x_continuous(breaks = d_chart$t, minor_breaks = NULL, ) +
             scale_y_continuous(n.breaks = 8) +
             theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) +
             labs(title = d_2dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # a better way of doing this
                  subtitle = paste0(x, ", % growth rate in baseline" ),
                  x = NULL,
                  y = "% growth in baseline") + 
             gg_theme
           
           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, ".pdf") ),
                   units = "in",
                   scale = 0.8,
                   height = 5,
                   width = 8* (length(unique(d_chart$t))/16)^0.2  )
         }
    )
  } else if (chart_type=="bau_level_line"){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # baseline: line graph with baseline levels
    map( d_tmp$r %>% unique,
         function(x){
           # debug: x="USA"
           d_chart <- d_tmp %>%
             filter(r == x,
                    sim == "BaU",
                    t %in% year_span)
           
           ggplot(d_chart, aes(x=t, y=value)) +
             geom_line(size=0.8, color= "blue") +
             scale_x_continuous(breaks = d_chart$t, minor_breaks = NULL, ) +
             scale_y_continuous(n.breaks = 8) +
             theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) +
             labs(title = d_2dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # a better way of doing this
                  subtitle = paste0(x, ", baseline level" ),
                  x = NULL,
                  y = NULL) + 
             gg_theme
           
           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, ".pdf") ),
                   units = "in",
                   scale = 0.8,
                   height = 5,
                   width = 8* (length(unique(d_chart$t))/16)^0.2  )
         }
    )
    
  } else if (chart_type=="all_%g_line"){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # bau + simulations: line graph with growth rates
    map( d_tmp$r %>% unique,
         function(x){
           # debug: x="USA"
           d_chart <- d_tmp %>%
             filter(r == x,
                    t != min(t),
                    t %in% year_span)
           
           ggplot(d_chart, aes(x=t, y=value_g, color=sim)) +
             geom_line(size=0.8) +
             scale_x_continuous(breaks = d_chart$t, minor_breaks = NULL, ) +
             scale_y_continuous(n.breaks = 8) +
             gg_theme +
             theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9),
                   legend.position="top",
                   legend.title=element_blank()) +
             labs(title = d_2dim %>% filter(variable_name==var_tmp) %>% pull(variable_label), # a better way of doing this
                  subtitle = paste0(x, ", % growth rate" ),
                  x = NULL,
                  y = "% growth") 
           
           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, ".pdf") ),
                   units = "in",
                   scale = 0.8,
                   height = 6,
                   width = 8* (length(unique(d_chart$t))/16)^0.2  )
         }
    )
    
  } else if (chart_type=="sim_%bau_line"){
    map(  d$r %>% unique,
          function(x){
            # debug: x="CHN"
            d_chart <- d_tmp %>%
              filter(r == x,
                     sim != "BaU",
                     t %in% year_span)
            
            ggplot(d_chart, aes(x=t, y=change, colour=sim)) +
              geom_line(size=2) + 
              scale_x_continuous(breaks = d_chart$t, minor_breaks = NULL, ) +
              scale_y_continuous(n.breaks = 8) +
              gg_theme + 
              theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9),
                    legend.position="top",
                    legend.title=element_blank()) +
              labs(title = d_2dim %>% filter(variable_name==var_tmp) %>% pull(variable_label),
                   subtitle = paste0(x, ", % change w.r.t. baseline" ),
                   x = "",
                   y = "% change w.r.t. baseline") 
            
            ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, ".pdf") ),
                    units = "in",
                    scale = 0.8,
                    height = 6,
                    width = 8* (length(unique(d_chart$t))/16)^0.2  )          }
    )
    
  } else if (chart_type=="sim_%bau_bar"){
    map( d$r %>% unique,
         function(x){
           # debug: x="CHN"
           d_chart <- d_tmp %>% filter( r == x,
                                        sim != "BaU",
                                        t %in% year_span )
           
           
           ggplot(d_chart, aes(x=factor(t), y=change, fill=sim)) +
             geom_col(position=position_dodge()) +
             scale_y_continuous(n.breaks = 8) +
             gg_theme + 
             theme(legend.position="top",
                   legend.title=element_blank()) +
             labs(title = d_2dim %>% filter(variable_name==var_tmp) %>% pull(variable_label),
                  subtitle = paste0(x, ", % change w.r.t. baseline" ),
                  x = NULL,
                  y = "% change w.r.t. baseline") 
           
           ggsave( file.path(chart_dir, folder_name, var_tmp, paste0(x, "_", chart_name, ".pdf") ),
                   units = "in",
                   scale = 0.8,
                   height = 6,
                   width = 8* (length(unique(d_chart$t))/16)^0.2  )          }
    )
    
  }
}


#' Define a function that subsets the whole data for a single variable
#' and then applies the chart function above.

plot.var.2dim <- function(var_tmp){
  
  #debug: var_tmp = "rgdppc"
  d_2dim_tmp <- d_2dim %>% filter(variable_name==var_tmp)
  d_tmp <- d %>% filter(var==var_tmp)
  

  # save data if required
  if(d_2dim_tmp$export_var_data[1]==1){
    write.csv(d_tmp,
              file.path(chart_dir, folder_name, var_tmp, paste0(var_tmp, ".csv") ),
              row.names = F)
  }
  
  # now apply the chart function
  pmap(list(
      chart_type = d_2dim_tmp$chart_type,
      year_span = d_2dim_tmp$year_span,
      theme = d_2dim_tmp$theme,
      export_chart_data = d_2dim_tmp$export_chart_data,
      chart_name = d_2dim_tmp$chart_name
    ),
    all.charts,
    d_tmp=d_tmp, # pass in data
    var_tmp=var_tmp # pass in the variable name
  )
  
  
}


map( d_2dim$variable_name %>% unique,
     plot.var.2dim)








# 
# 
# my.count <- function(x){nrow(x)}
# 
# 
# my.f <- function(var_tmp){
#   
#   #debug: var_tmp = "rgdppc"
#   d_tmp <- d %>% filter(var==var_tmp)
#   
#   # now apply the chart function
#   my.count(d_tmp)
#   
# }
# lapply( d_2dim$variable_name %>% unique,
#         my.f)


#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Plot groups of variables  ----
#' 
#' #' in the Excel file in the 2 dimension sheet,
#' #' the variables can be grouped by group. For
#' #' each group we plot 2 sets of charts
#' #' - for a given year, all simulations
#' #' - for a given simulation, selected years
#' 
#' #' get the variables by group
#' d_2dim_g <- d_2dim %>% 
#'               filter(!is.na(group)) %>% 
#'               group_by(group) %>% 
#'               group_map( ~ unique(.x$variable_name))
#' 
#' 
#' 
#' 
#' group.plots <- function(group_nb){
#'   
#'   #debug: group_nb <- 1
#'   
#'   
#'   #' - all simulations, one of years5 (all sim, loop over years5)
#'   pmap( crossing( x = d$r %>% unique,
#'                   y = d_2dim_g[group_nb],
#'                   tt = years_5),
#'         function(x,y,tt){
#'           # debug: x="CHN"
#'           # debug: y=d_2dim_g[[group_nb]]
#'           # debug: tt = years_5[3]
#'           d %>%
#'             filter(r == x,
#'                    var %in% y,
#'                    t == tt,
#'                    sim != "BaU") %>%  
#'             ggplot(., aes(x=change, y=var, fill=sim)) +
#'             geom_col(position = position_dodge()) + 
#'             labs(title = x,
#'                  #subtitle = paste0(", growth w.r.t. baseline" ),
#'                  x = "% change w.r.t. baseline",
#'                  y = "") + 
#'             my_theme2 
#'           ggsave( file.path(chart_dir, folder_name, paste0(x,  "_group", group_nb ,"_allsim_", tt, ".pdf") ))
#'         }
#'   )
#'   
#'   
#'   #' - one simulation, years5 (all years, loop over simulations)
#'   pmap( crossing( x = d$r %>% unique,
#'                   s = d$sim %>% unique %>% .[.!="BaU"],
#'                   y = d_2dim_g[group_nb]),
#'         function(x,s,y){
#'           # debug: x="CHN"
#'           # debug: y=d_2dim_g[[group_nb]]
#'           # debug: s = d$sim %>% unique %>% .[.!="BaU"] %>% .[1]
#'           d %>%
#'             filter(r == x,
#'                    sim == s,
#'                    var %in% y,
#'                    t %in% years_5) %>%  
#'             ggplot(., aes(x=change, y=var, fill=factor(t))) +
#'             geom_col(position = position_dodge()) +
#'             labs(title = x,
#'                  #subtitle = paste0(", growth w.r.t. baseline" ),
#'                  x = "% change w.r.t. baseline",
#'                  y = "") + 
#'             my_theme2 
#'           ggsave( file.path(chart_dir, folder_name, paste0( x, "_group", group_nb ,"_", s, ".pdf") ))
#'         }
#'   )
#' }
#' 
#' 
#' map(1:length(d_2dim_g), group.plots)
#' 












