#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description ----

#' This file contains code for a prototype of a Shiny app
#' 
#' To keep things easy in the app code, for now there is some data processing 
#' defined outside of the ui and server functions. Therefore need to run 
#' the code until the beginning of the ui section before lunching the app.
#' This will change in the future.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Directories ----

rm(list = ls())

#' Set the work directory of the main folder
try(setwd("~/Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino") , silent = T)
try(setwd("C:/Users/valdes/Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino") , silent = T)
try(setwd("Z:/Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino") , silent = T)
#try(setwd("C:/Users/wb388321/OneDrive - WBG/Projects/STC_Valentino/Rcode_Valentino") , silent = T)
try(setwd("C:/Users/wb388321/Documents/GitHub/gams_plots") , silent = T)
try(setwd("C:/Users/desil/Documents/GitHub/gams_plots") , silent = T)


#' Set the name of folder where the gdx files are located
#input_dir <- file.path(getwd(), "input_data/Manage")
#input_dir <- file.path('Z:Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino/input_data/Envisage')
input_dir <- file.path('Z:/Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino/input_data/Manage')
#input_dir <- file.path(getwd(), "input_data/Envisage")
#input_dir <- file.path("C:/Users/wb388321/Documents/CGEmodels/MANAGE_GHA_CCDR/res")


#' directory where to save all plots (the folder needs to exist already)
#chart_dir <- file.path(getwd(), "charts/Manage")
#chart_dir <- file.path('Z:Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino/charts/Envisage')
chart_dir <- file.path('Z:/Dropbox/Valentino/Projects/WB_plots/Rcode_Valentino/charts/Manage')
#chart_dir <- file.path("C:/Users/wb388321/Documents/CGEmodels/MANAGE_GHA_CCDR/charts")

#' GAMS directory
gams_dir <- "C:/GAMS/36"
#gams_dir <- "C:/Program Files/GAMS36"

#' names of files to be imported (there is one file for each simulation)
# input_files <- c("BaU.gdx", "Sim1_RenW.gdx", "Sim2_RenAlt.gdx") # use a list of files
input_files <- list.files(input_dir, pattern = "\\.gdx$") # read in the names of files with a .gdx extension
input_files_names <- gsub("\\.gdx", "", input_files)

#' name of the Excel file containing the list of variables
#input_excel <- file.path( getwd(),
input_excel <- file.path(#"C:/Users/wb388321/Documents/CGEmodels/MANAGE_GHA_CCDR/charts/Manage_input_variables_list_v02.xlsx"
    "input_variables_list/Manage_input_variables_list_IDN_v01.xlsx"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dependencies ----
packages <- c("magrittr",
              "dplyr",
              "ggplot2",
              "stringr",
              "tidyr",
              "purrr",
              "forcats",
              "gdxrrw",
              "reshape2",
              "openxlsx",
              "readxl",
              "RColorBrewer",
              "DT",
              "shinyWidgets"
              # "hrbrthemes",
              #"patchwork"
)
to.install <- setdiff(packages, rownames(installed.packages()))
if (length(to.install) > 0) {
    install.packages(to.install)
}
lapply(packages, library, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load functions and plot settings  ----

source("scripts/functions/custom_ggplot.R") # plot themes

source("scripts/functions/year_fix.R") # process and format years in Excel variable file
source("scripts/functions/import_data.R") # used to import one specific variable in one gdx file
source("scripts/functions/changes_wrt_baseline.R") # computes growth rates, and %changes wrt baseline

# set extension type for exported charts
# this is accessed with the superassignment operator <<- in the function that produces the charts
# chart_ext <- ".pdf"
chart_ext <- ".png"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAMS  ----

# https://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r

#' The data is read in using the gdxrrw package.
#' Most likely need to tell R where GAMS is located, since some libraries are used to import the data.
print(igdx(gamsSysDir = gams_dir))
#igdx()

#' Ideally should make sure that the GDXRRW package works correctly.
#' Can do that by running the test function included in the package
#' Need to set the working directory to the location of the library,
#' otherwise it doesnt work (other files are sourced)
#' Press ESC to exit tests
test_gdx <- F
if(test_gdx==T){
    dir <- getwd() # save current wd
    setwd(file.path(path.package('gdxrrw'),'tests')) # setwd to package location
    source("tAll.R") # run tests
    setwd(dir) # restore original dir
    rm(dir)
}

#' check system paths, if necessary
# Sys.getenv("PATH") %>% strsplit(. ,";")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data info  ----

#' get a data frame containing all of the variables, parameters, sets and their properties
#' It is used to check the dimensions of the variables in the excel files

d_info <- gdxInfo(file.path(input_dir, input_files[1]), #
                  dump = F,
                  returnDF = T)

#' Explore content:
#' contains some data frames and some vectors
sapply(d_info, class)

#' (almost) all data frames have the same structure:
sapply( d_info[sapply(d_info, is.data.frame)], names  ) # what s aliases?

#' merge all the data frames together, makes it easier to explore
d_meta <- d_info[sapply(d_info, is.data.frame)] %>%
    bind_rows(., .id = "type")

#' convert the dimension names to character
d_meta %<>%
    mutate( domnames_v = sapply(domnames,
                                function(x) x %>%
                                    unlist %>%
                                    .[!grepl("^\\s*$|^*$", .)] %>% # this doesnt work
                                    paste0(., collapse = "-")  )) # %>% sort

#' add a count for the number of dimenstions
#' Note: this is not really necessary for now, as the variables are selected in the Excel file.
#' problem 1: sometimes it is zero ?
d_meta %<>%
    mutate("nb_dimensions" = ifelse(type=="variables" , str_count(domnames_v, "-") + 1, NA))
#' For Manage, we need to add 1. If there is mapRep then we are processing Manage,
#' therefore ad 1 to account for the implicit dimension "r".
if("mapRep" %in% d_meta$name){
    d_meta %<>%
        mutate("nb_dimensions" = nb_dimensions + 1 )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregation and labels  ----

# 1) Aggregation of dimensions

#' The aggregate variable is different for Manage and Envisage
#' therefore we create the aggregation dataframe depending on the
#' presence/absence of the aggregation set name in d_meta

#' In Envisage the aggregation variable is called "mapaga"
if("mapaga" %in% d_meta$name){
    d_agg <-  rgdx( file.path(input_dir, input_files[1]),
                    list(name = "mapaga", form = "full"))$val %>%
        as_tibble(rownames = "aga") %>%
        melt(id="aga", variable.name="a") %>%
        filter(value !=0) %>% # remove non relevant
        filter(aga!=a) %>% # remove same aggregation
        filter(aga!="ttot-a") %>% # remove total from aggregation
        select(-value) %>%
        rename(var = a,
               var_aggr = aga)
    #' In Manage the aggregation variable is called "mapRep"
} else if ("mapRep" %in% d_meta$name) {
    d_agg <-  rgdx( file.path(input_dir, input_files[1]),
                    list(name = "mapRep", form = "full"))$val %>%
        as_tibble(rownames = "is") %>%
        melt(id="is", variable.name="isrep") %>%
        filter(value !=0) %>% # remove non relevant
        #filter(is0!=a) %>% # remove same aggregation
        select(-value) %>%
        rename(var = is,
               var_aggr = isrep) # WiP: will need to harmonize the names
}



## 2) Years for reporting

# years for reporting ( "treport" is not available in this gdx file?)
years_all <- rgdx( file.path(input_dir, input_files[1]), list(name = "t", form = "full"))$val %>%
    as_tibble(rownames = "y") %>%
    filter(V1==1) %>%
    pull(y) %>%
    as.integer # all years as a vector

# years selections
years_5 <- seq(min(years_all), max(years_all), 5)



## 3) Labels

#' For Manage, we import the labels from the Excel file
d_labels <- read_excel(input_excel,
                       sheet = "labels")
#' if there are empty rows we fill them in with
#' the original name
d_labels %<>% mutate(new = ifelse(is.na(new), old, new))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3 dim ----

#' The list of variables to be plotted from the Excel file
d_3dim  <- read_excel(input_excel,
                      sheet = "3dim",
                      col_types = c("text","text","text","numeric","numeric","text","numeric","text","text","numeric","text"))

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
        mutate( aggr = "_aggr")
)

#' For a given variable, if we have the same plot with different years or styles, we need to create different names
d_3dim %<>%
    group_by(variable_name, chart_type) %>%
    mutate(chart_name = row_number() -1) %>% 
    mutate(chart_name = paste0(variable_name, "_", gsub("\\%", "", chart_type), if_else(chart_name>0, as.character(chart_name), ""))) %>% 
    mutate(chart_name = if_else(grepl("aga", domnames_v), paste0("agg_", chart_name), chart_name)) %>% 
    ungroup()

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

select_vars <- d_3dim_v %>% pull(variable_label) %>% unique
select_country <- d %>% pull(r) %>% unique





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ui ----

#prod_codes <- setNames(products$prod_code, products$title)

ui <- navbarPage("GAMS simulations",
   tabPanel("Variables overview",
            DTOutput('d_meta')),
   
   tabPanel("Project pipeline",
            img(src = "chart1.png", height="50%", width="50%", align="center")
            ),
   
   

   navbarMenu("Charts",
              tabPanel("2 Dimensions"),
              tabPanel("3 Dimensions",
                       
                       
                       # full width row with year selection
                       fluidRow(
                           column(12,
                                  # sliderInput("year", "Select year", min(in_year), max(in_year),
                                  #             value = range(in_year), step= 1, sep = "", width = "100%", animate = T, dragRange = T)
                           )
                       ),
                       fluidRow(
                           column(12,
                                  textOutput("text")
                           )
                       ),
                       # select sectors and countries
                       fluidRow(
                           column(6,
                                  pickerInput(
                                      inputId = "select_var",
                                      label = "Select variable",
                                      width = "100%",
                                      choices = select_vars,
                                      selected = "Total emissions",
                                      multiple = FALSE,
                                      options = list(`actions-box` = TRUE, `selected-text-format`= "count")
                                  )
                           ),
                           column(6,
                                  pickerInput(
                                      inputId = "select_country",
                                      label = "Select Country",
                                      width = "100%",
                                      choices = select_country,
                                      selected = "IDN",
                                      multiple = FALSE,
                                      options = list(`actions-box` = TRUE, `selected-text-format`= "count")
                                  )
                           )
                       ),
                       # fluidRow(
                       #     column(4, tableOutput("diag")),
                       #     column(4, tableOutput("body_part")),
                       #     column(4, tableOutput("location"))
                       # ),
                       fluidRow(
                           column(6, plotOutput("baseline")),
                           column(6, plotOutput("simulation"))
                       )
                       # fluidRow(
                       #     column(2, actionButton("story", "Tell me a story")),
                       #     column(10, textOutput("narrative"))
                       # )                   
                       
                       
            ),
              tabPanel("4 Dimensions"))

)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### server ----

server <- function(input, output, session) {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Data preparation
    
    
    # variable of selected variable label
    tmp_var <- reactive( d_3dim_v %>%
                             filter( variable_label == input$select_var) %>%
                             pull(variable_name) %>%
                             unique )
    
    # using the inputs, get data for Baseline
    d_tmp_bau <- reactive(
                          d %>%
                            filter( sim == 'BaU',
                                    var == tmp_var(),
                                    #var == d_3dim_v$variable_name[match(input$select_var, d_3dim_v$variable_label)[1]],
                                    t %in% c(2017, 2018))
    )
    
    # using the inputs, get data for Simulations
    d_tmp_sim <- reactive(
        d %>%
            filter( sim != 'BaU',
                    var == tmp_var(),
                    #var == d_3dim_v$variable_name[match(input$select_var, d_3dim_v$variable_label)[1]],
                    t %in% c(2022, 2027)) %>%
            mutate(lab = paste0(t, "\n", sim_l))
    )
    
    # set parameters for charts
    compute_shares <- T
    gg_theme <- my_theme3
    uni_factors_bas <- reactive( d_tmp_bau() %>% pull(var3) %>% unique %>% length )
    uni_factors_sim <- reactive( d_tmp_sim() %>% pull(lab) %>% unique %>% length )
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output section
    
    #' Meta data tale
    output$d_meta = renderDT(
        d_meta %>% filter(type=="variables") %>% select(-(type)), options = list(lengthChange = FALSE)
    )
    
    #' Baseline bar chart
    output$baseline <- renderPlot({
        d_tmp_bau() %>%
            ggplot(aes(x=var3, y=value, fill=factor(t))) +
            geom_col(position = position_dodge()) +
            scale_y_continuous(n.breaks = 8) +
            labs(title = "Baseline", # automatically takes first of vector
                 #subtitle = paste0(x, ", Baseline" ),
                 x = NULL,
                 y = ifelse(compute_shares==T, "% of total", "Baseline value" ), 
                 fill = NULL) +
            gg_theme  + rot.axis(uni_factors_bas())
    }, res = 120)
    
    #' Baseline bar chart
    output$simulation <- renderPlot({
        d_tmp_sim() %>%
            ggplot(aes(x=lab, y=change, fill=var3)) +
            geom_col(position = position_dodge()) +
            scale_y_continuous(n.breaks = 8) +
            labs(title = "Simulations", # automatically takes first of vector
                 #subtitle = paste0(x, ", Simulations" ),
                 x = NULL,
                 y = paste0("% change w.r.t. baseline", ifelse(compute_shares==T, " shares", "")),
                 fill = NULL) +
            gg_theme + rot.axis(uni_factors_sim())
    }, res = 120)
    
    
    # output$baseline <- renderPlot({
    #     ggplot(iris, aes( Sepal.Length, Species)) +
    #         geom_boxplot()
    # }, res = 96)
    
    
    output$text <- renderText( getwd() )

    
    
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### run ----
shinyApp(ui, server)
