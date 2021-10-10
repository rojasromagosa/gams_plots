#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description ----

#' This file creates charts using the EMVISAGE output from GAMS



#' Stuff to think about:
#' 
#' - need to chose how much processing of data is to be made within R: for instance, for all variables need to multiply
#'   by the var0 variable?
#' - when importing can either import as a matrix/array, which maintains the names of the dimensions, or long which uses indexes which
#'   need to be matched to a vector. For two dimensions arrays work great, a lot less with arrays with >2 dimensions

#' Notes
#' - import.data cant be used to import parameters yet, because of the is no value in the data.frame


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Directories ---- 

rm(list = ls())
try(setwd("~/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("C:/Users/valdes/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("Z:/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("somewhere/Hugo/Github/plots/fixme") , silent = T)

#' directory of input files
input_dir <- file.path(getwd(), "Rcode_Valentino")

#' directory where to save all plots
#' (can then use file.path(chart_dir, "plot_name.pdg") when saving)
chart_dir <- file.path(getwd(), "Charts")

#' GAMS directory
gams_dir <- "C:/GAMS/36"

#' names of the files
input_files <- c("BaU.gdx", "Sim1_RenW.gdx", "Sim2_RenAlt.gdx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dependencies ----
packages <- c("magrittr",
              "tidyverse",
              "gdxrrw",
              "reshape2"
              # "ggeasy",
              # "hrbrthemes",
              #"patchwork"
              )
to.install <- setdiff(packages, rownames(installed.packages()))
if (length(to.install) > 0) {
  install.packages(to.install)
}
lapply(packages, library, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Custom graph theme  ----

#' import customized ggplot:
#' - my_theme1 :
#' - my_theme2 : 
#' - xaxis :
source("scripts/00_custom_ggplot.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAMS  ----

# https://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r

#' The data is read in using the gdxrrw package. 
#' Most likely need to tell R where GAMS is located, since some libraries are used to import the data.
igdx(gamsSysDir = gams_dir)

#' Ideally should make sure that the package works correctly.
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data info  ----


d_info <- gdxInfo(file.path(input_dir, "BaU.gdx"), 
                  dump = F, # set to true to read in all data?
                  returnDF = T)

#' Explore content:
#' contains some data frames and some vectors
sapply(d_info, class)

#' (almost) all data frames have the same structure:
sapply( d_info[sapply(d_info, is.data.frame)], names  ) # what s aliases?

#' merge all the data frames together, makes it easier to explore
d_meta <- d_info[sapply(d_info, is.data.frame)] %>% 
              bind_rows(., .id = "type")

#' check if names are unique
d_meta %>% 
  group_by(name) %>% 
  count %>% 
  filter(n>1) # name is unique if this is empty

#' county by type
d_meta %>% 
  group_by(type) %>% 
  count

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions  ----

#' IMPORT DATA
#' Use a general function to import data.
#' There are two options: form="full" or long format. The first one is nice because it keeps the values of the dimensions,
#' but it gets a bit messy with more than two dimensions, as need to flatten the array...
#' It is seems better to import long and then match the names

#' One issue is whether the data needs to multiplies with its value0. 
#' For now pass in a T/F argument to the function

#' this function can be used to import data
#' it works for all variables that require the variable to be multiplied by variable0, and then divided by the scalar
import.data <- function(gdx_name, var_name, var_name0 = T, inScale = T){
  # ~~~~~~~~~~~~
  # debug start: 
  # gdx_name <- input_files[1]
  # var_name <- "rgdpmp"
  # var_name0 <- T
  # inScale <- T
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
  
  #' if we need to multiply by var0 read in and multiply
  if(var_name0==T){
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
  if(inScale==T){
    scale <- rgdx( file.path(input_dir, gdx_name),
                   list(name = "inScale", form = "full"))$val
    d %<>%
      mutate(value = value/scale)
  }
  
  #' need to change year into numeric (it is stored as character in the gdx file)
  d %<>% mutate(t = as.integer(t))
  
  #' filter year
  d %<>% filter(t>=2021)
  
  #' end of processing data
  return(d)
}


#' CHANGES to BASELINE
#' the data to be passed in has all the simulations in long form. We want:
#' - compute growth rates for all in "sim" and all dimensions
#' - compute changes relative to BaU
growth.changes <- function(x){
  # debug: x <- d
  # 1) compute growth rates
  x %<>%
    arrange(t) %>% 
    group_by_at(vars(-t, -value))  %>% # group by every dimension except for time, and value
    mutate(value_g = 100*(value-lag(value))/lag(value)) %>% 
    ungroup()
  
  # add baseline as row
  names_by <- setdiff(names(x), c("sim","value", "value_g"))
  x <- left_join(x,
                  x %>% filter(sim=="BaU") %>% select(-sim),
                  by = names_by,
                  suffix = c("", "_bau"))
  
  #' percentage change w.r.t. baseline
  x %<>%
    mutate(change = 100*(value - value_bau)/value_bau) 
  
  #' output
  return(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process variables  ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## aggregation and labels  ----

# sectoral aggregates
d_agg <- rgdx( file.path(input_dir, input_files[1]),
               list(name = "mapaga", form = "full"))$val %>% 
          as_tibble(rownames = "aga") %>%
          melt(id="aga", variable.name="a") %>%
          filter(value !=0) %>% 
          select(-value)

# 4 sectors
sectors4 <- c("tagr-a", "tman-a", "tsrv-a", "toth-a")

# years selection
years5 <- c(2021, seq(2025, 2040, 5))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## rgdpmp (GDP)  ----

var_name <- "rgdpmp"
dir.create(file.path(chart_dir, var_name),  showWarnings = F)

#' read in the data from multiple gdx files, multiply by var0, and the scalar
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = T,
            inScale = T)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

# compute growth rates, and changes wrt baseline
d %<>% growth.changes


#' plots
#' - growth rates in baseline:
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim == "BaU",
               t != min(t)) %>% 
      ggplot(., aes(x=t, y=value_g)) +
        geom_line(size=2, color= "blue") + 
        labs(title = paste0(x, ": GDP growth rate in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "growth rate") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("rgdpmp_bau_", x, ".pdf") ), width = 6, height = 4)
})

#' change wrt baseline, all years, line
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim != "BaU",
               t != min(t)) %>% 
        ggplot(., aes(x=t, y=change, colour=sim)) +
        geom_line(size=2) + 
        labs(title = paste0(x, ": GDP growth rate in simulations" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline",
             fill = "Simulation:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("rgdpmp_sim_line_", x, ".pdf") ), width = 6, height = 4)
})

#' change wrt baseline, years5, bar
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim != "BaU",
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=change, fill=sim)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": GDP growth rate in simulations" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline",
             fill = "Simulation:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("rgdpmp_sim_bar_", x, ".pdf") ), width = 6, height = 4)
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## xp (sectoral structure)  ----

#' two types of charts:
#' - using all sectors: bar first and last year,  
#' - using 4 aggregate: line all years, bar every 5years, 

var_name <- "xp"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' read in the data from multiple gdx files, multiply by var0, and the scalar
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = T,
            inScale = T)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

#' compute shares
d %<>%
  group_by(sim, r, t) %>%
  mutate(value = value/sum(value))

# compute growth rates, and changes wrt baseline
d %<>% growth.changes

#' plots
#' - Baseline bar first and last year:
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim == "BaU",
               (t == min(t) | t == max(t)) ) %>% 
        ggplot(., aes(x=a, y=value, fill=factor(t))) +
        geom_col(position = position_dodge()) + 
        labs(title = paste0(x, ": sectoral output in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% of total output",
             fill = "") + 
        my_theme2 +
        theme(axis.text.x = element_text(angle = 60, vjust = 1.7, hjust=1.5))
      ggsave( file.path(chart_dir, var_name, paste0("xp_bau_", x, ".pdf") )) #, width = 6, height = 4)
})

#' - Simulations
#'   one plot for each simulation, % change wrt to baseline
d_cmbn <- crossing(r_id = d$r %>% unique,
                   sim_id = d$sim %>% unique %>% .[.!="BaU"])
map2(d_cmbn$r_id,
     d_cmbn$sim_id,
    function(x,y){
      # debug: x="CHN"
      # debug: y="Sim2_RenAlt"
      d %>%
        filter(r == x,
               sim == y,
               (t == min(t) | t == max(t)) ) %>% 
        ggplot(., aes(x=a, y=change, fill=factor(t))) +
        geom_col(position = position_dodge()) + 
        labs(title = paste0(x, ": sectoral output in ", y ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline share",
             fill = "") + 
        my_theme2 +
        theme(axis.text.x = element_text(angle = 60, vjust = 1.7, hjust=1.5))
      ggsave( file.path(chart_dir, var_name, paste0("xp_", y, "_",  x, ".pdf") )) #, width = 6, height = 4)
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## xp 4 (sectoral structure aggregate)  ----

#' two types of charts:
#' - using all sectors: bar first and last year,  
#' - using 4 aggregate: line all years, bar every 5years, 

var_name <- "xp"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' read in the data from multiple gdx files, multiply by var0, and the scalar
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = T,
            inScale = T)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

#' aggregate sectors
d %<>% 
  left_join(., 
            d_agg %>% filter(aga %in% sectors4),
            by = "a") %>% 
  group_by(sim, r, t, aga) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

#' compute shares
d %<>%
  group_by(sim, r, t) %>%
  mutate(value = value/sum(value))

# compute growth rates, and changes wrt baseline
d %<>% growth.changes

#' plots
#' - Baseline line for each sector:
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim == "BaU",
               t != min(t)) %>% 
        ggplot(., aes(x=t, y=value, color=factor(aga))) +
        geom_line(size=2) + 
        labs(title = paste0(x, ": sectoral output in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% of total output",
             color = "Sectors:") + 
        my_theme2
      ggsave( file.path(chart_dir, var_name, paste0("xp4_bau_line_", x, ".pdf") )) #, width = 6, height = 4)
})

#' - Baseline bar every 4 years:
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim == "BaU",
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=value, fill=aga)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": sectoral output in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% of total output",
             fill = "Sectors") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("xp4_bau_bar_", x, ".pdf") ), width = 6, height = 4)
})

#' - Simulations: bar every 4 years with changes wrt baseline:
#'   One plot for each simulation
d_cmbn <- crossing(r_id = d$r %>% unique,
                   sim_id = d$sim %>% unique %>% .[.!="BaU"])
map2(d_cmbn$r_id,
     d_cmbn$sim_id,
     function(x,y){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim == y,
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=change, fill=aga)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": sectoral output in ", y),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline share",
             fill = "Sectors:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("xp4_", y, "_bar_", x, ".pdf") ), width = 6, height = 4)
})





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lab (Sectoral employment)  ----

#' contains labor by activity and skill.
#' We aggregate skill level

var_name <- "lab"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' lab 4 dimensions, there is no lab0
#' read in the data from multiple gdx files
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name="lab",
            var_name0 = F,
            inScale = F)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

#' aggregate labor skill
d %<>% 
  group_by(sim,r,a,t) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

#' aggregate sectors
d %<>% 
  left_join(., 
            d_agg %>% filter(aga %in% sectors4),
            by = "a") %>% 
  group_by(sim, r, t, aga) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

#' compute shares
d %<>%
  group_by(sim, r, t) %>%
  mutate(value = value/sum(value))

# compute growth rates, and changes wrt baseline
d %<>% growth.changes


# baseline: chart with employment shares by 4 sec aggregates
#' - Baseline bar every 4 years:
map(d$r %>% unique,
    function(x){
      # debug: x="CIV"
      d %>%
        filter(r == x,
               sim == "BaU",
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=value, fill=aga)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": sectoral employment in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% of total employment",
             fill = "Sectors:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("val4_empl_bau_bar_", x, ".pdf") ), width = 6, height = 4)
})


# simulations: chart with employment shares by 4 sec aggregates, relative to baseline
d_cmbn <- crossing(r_id = d$r %>% unique,
                   sim_id = d$sim %>% unique %>% .[.!="BaU"])
map2(d_cmbn$r_id,
     d_cmbn$sim_id,
     function(x,y){
       # debug: x="CHN"
       d %>%
         filter(r == x,
                sim == y,
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=change, fill=aga)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": sectoral employment in", x ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline share",
             fill = "Sectors:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("val4_empl_", y, "_bar_", x, ".pdf") ), width = 6, height = 4)
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lab (skill composition)  ----

# skill composition, across the whole economy

var_name <- "lab"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' lab 4 dimensions, there is no lab0
#' read in the data from multiple gdx files
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = F,
            inScale = F)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

#' aggregate all sectors
d %<>% 
  group_by(sim, r, t, l) %>% 
  summarise(value = sum(value)) %>% 
  ungroup

#' compute shares
d %<>%
  group_by(sim, r, t) %>%
  mutate(value = value/sum(value)) %>% 
  ungroup()

# compute growth rates, and changes wrt baseline
d %<>% growth.changes

#' plots
#' baseline: bar chart every 4 years
map(d$r %>% unique,
    function(x){
      # debug: x="CIV"
      d %>%
        filter(r == x,
               sim == "BaU",
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=value, fill=l)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": skill composition in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% of total labor force",
             fill = "Skill level:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("val4_skill_bau_bar_", x, ".pdf") ), width = 6, height = 4)
    })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# xp_val (energy mix)  ----

var_name <- "xp_val"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' xp_val 3 dimensions, there is no xp_val0
#' read in the data from multiple gdx files
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = F,
            inScale = F)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")


#' compute shares
d %<>%
  group_by(sim, r, t) %>%
  mutate(value = value/sum(value))
  
#' compute growth rates, and changes wrt baseline
d %<>% growth.changes

# plot baseline only
map(d$r %>% unique,
    function(x){
      # debug: x="ZAF"
      d %>%
        filter(r == x,
               sim == "BaU",
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=value, fill=elya)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": Energy composition in baseline" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "share of total energy generation",
             fill = "Power source:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("xp_val_baseline_", x, ".pdf") ), width = 6, height = 4)
    })

#' plot simulations
#' - do we want to plot one chart per simulation?
d_cmbn <- crossing(r_id = d$r %>% unique,
                   sim_id = d$sim %>% unique %>% .[.!="BaU"])
map2(d_cmbn$r_id,
     d_cmbn$sim_id,
    function(x,y){
      # debug: x="ZAF"
      d %>%
        filter(r == x,
               sim == y,
               t %in% years5) %>% 
        ggplot(., aes(x=factor(t), y=value, fill=elya)) +
        geom_col(position=position_dodge()) + 
        labs(title = paste0(x, ": Energy composition in", y ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline total share",
             fill = "Power source:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("xp_val_", y, "_" , x, ".pdf") ), width = 6, height = 4)
    })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rwage (real wage)  ----

var_name <- "rwage"
dir.create(file.path(chart_dir, var_name), showWarnings = F)

#' rwage 3 dimensions (country, skill, time), there is no rwage0, but 2014 values are 1?
#' plots just for the simulations

#' read in the data from multiple gdx files
d <- lapply(input_files, # names of gdx files
            import.data, 
            var_name=var_name,
            var_name0 = F,
            inScale = F)
names(d) <- gsub("\\.gdx", "", input_files)

d %<>% bind_rows(., .id = "sim")

# compute growth rates, and changes wrt baseline
d %<>% growth.changes

d %<>%
  mutate(sim_l = paste0(sim, "-", l))

#' Plots
#' - line chart with deviations from baseline, all simulations, all skills
map(d$r %>% unique,
    function(x){
      # debug: x="CHN"
      d %>%
        filter(r == x,
               sim != "BaU",
               t != min(t)) %>% 
        ggplot(., aes(x=t, y=change, colour=sim_l)) +
        geom_line(size=2) + 
        labs(title = paste0(x, ": real wage in simulations" ),
             #subtitle = "Power source shares of total energy generation",
             x = "",
             y = "% change w.r.t. baseline",
             fill = "Simulation:") + 
        my_theme2 
      ggsave( file.path(chart_dir, var_name, paste0("rwage_sim_line_", x, ".pdf") ), width = 6, height = 4)
})






















