#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description ----

#' This file

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Directories ---- 

rm(list = ls())

#' Set the work directory of the main folder
try(setwd("~/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("C:/Users/valdes/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("Z:/Dropbox/Valentino/Projects/WB_plots") , silent = T)
try(setwd("somewhere/Hugo/Github/plots/fixme") , silent = T)

#' This is the path to the folder where all plots are located
#' (this location will be searched recursively)
charts_path <- file.path(getwd(), "charts/Manage")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dependencies ----
packages <- c("magrittr", "readxl")
to.install <- setdiff(packages, rownames(installed.packages()))
if (length(to.install) > 0) {
  install.packages(to.install)
}
lapply(packages, library, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Variables list ----

#' We need the list of variables only for the variables labels?

input_excel <- file.path("Rcode_Valentino/input_variables_list/Manage_input_variables_list_v00.xlsx")

d_var <- bind_rows(read_excel(input_excel,  sheet = "2dim"), 
                   read_excel(input_excel,  sheet = "3dim"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in template ----

#' This is an empty latex file
d <- read.delim("Rcode_Valentino/latex/latex_test/template_v00.tex", header = F)


# test writing file
# write.table(d,
#             file = "Rcode_Valentino/latex/latex_test/charts_v00.tex",
#             sep = "\t",
#             row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## latex charts ----

#' Get a list of all charts included in the folder, looking in subdirectories
files <- list.files(charts_path, recursive = T, full.names = T)

#' Now get a list with the names of all the variables
#' We could either look at the names in the files, or by the name of the folders
#' For now used the list from above
variables <- files %>% 
                gsub(".*\\/[A-Z]+_", "", .) %>%
                str_extract(., "^[a-zA-Z]+") %>% 
                unique %>%
                .[! . %in% "group"]


#' For each of the variables we want to add a latex section, and for each of the plots
#' for a given variable include a chunk of code for inserting the chart

# this function is used in the function below, and adds the necessary code for importing a given chart
latex.chart <- function(z, x){
  #debug: z = "Z:/Dropbox/Valentino/Projects/WB_plots/charts/Manage/2dim/debtStkD/USA_debtStkD_bau.pdf"
  
  # Create a caption for the chart: use the variable name, and the country name
  tmp_chart = z %>% gsub("^.*\\/|\\.pdf$", "", .)
  caption =  paste0(d_var$variable_label[match(x, d_var$variable_name)], ", ", str_extract(tmp_chart, "^[A-Z]+"))
  
  data.frame(
    V1 = rbind(
      "%------------------------------------",
      "%------------------------------------",
      "\\begin{figure}[H]",
      #"\\caption{Baseline, real GDP growth rates (\\%)}",
      paste0("\\caption{", caption, "}"),
      #"\\label{fig_bau_gdp_gr}",
      paste0("\\label{fig_", tmp_chart, "}" ),
      "\\centering",
      paste0("\\includegraphics[width=0.75\\textwidth]{", shQuote(z), "}"),  #shQuote(z)
      #"\includegraphics[trim=0 0 0 10mm, clip=true, width=0.65\textwidth]{C:/Users/wb388321/OneDrive - WBG/Projects/GHA/Graphs/bau_gdp_gr.pdf}",
      "\\end{figure}"
      #"------------------------------------",
      #"------------------------------------"
    )
  )
}


latex.section <- function(x){
  # debug: x = variables[1]
  files_tmp <- files[ grepl(paste0("_", x ,"_"), files) ]
  print(x)
  print(files_tmp)
  
  #' Apply the latex.chart function to each chart we want to import
  tmp_out <- do.call("rbind",
                     lapply(files_tmp, latex.chart, x=x))
  
  #' Add a section
  h <- data.frame(
    V1 = rbind(
      "",
      "%-----------------------------------------------------------------",
      "%-----------------------------------------------------------------",
      "\\newpage",
      paste0("\\section{", d_var$variable_label[match(x, d_var$variable_name)], "}")
    )
  )
  
  tmp_out <- rbind(h, tmp_out)
  
}


d_charts <- do.call("rbind",
                    lapply(variables, latex.section))

variables
do.call("rbind", lapply("px", latex.section))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save file ----

# check where to split the template file
latex_template_split <- grep("\\end{document}", d[,1], fixed = T)

# create full tex file
d_out <- data.frame(
  rbind(
    d[1:(latex_template_split-1),,drop=F],
    d_charts,
    d[latex_template_split,]
  )
)

# save file
write.table(d_out,
            file = "Rcode_Valentino/latex/latex_test/charts_v00.tex",
            sep = "\t", 
            quote = F,
            col.names = F,
            row.names = F)



