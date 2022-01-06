This repository contains R code for creating charts using GAMS simulations data. Both ENVISAGE and MANAGE files can be processed.

# General info
1. The raw data, in gdx format, is imported using the **gdxrrw** package. This requires that GAMS is already installed.
2. Both ENVISAGE and MANAGE have many variables of different dimensions. MANAGE has an implicit dimension for all variables, given it contains only one region/country, instead of many. A 1 dimensional variable in MANAGE (e.g. GDP, which varies only across time) is equivalent to a 2 dimensional variable in ENVISAGE (region/country - time). Therefore, for each MANAGE variable, an "artificial" dimension containing the name of the country is created. This allows the use of the same code for both models.

# Data preparation
1. The selection of variables to read in is done via an Excel file. Additional info can be found in the readme of the Excel file. For each variable, it is necessary to specify which charts are to be produced. The preparation of the data follows these steps and is identical for variables of different dimensions
* Fill in default values in the Excel file when options are not speficied
* Call the *import_data* function for each combination of variable and simulation gdx file
* Call the *changes_wrt_baseline* function to calculate growth rates, and percentage change wrt baseline. Note that the number of dimensions to pass to **dplyr::group_by** is detected automatically (assumed to be everything except for the column containing the values, and the "t" dimension)

# Charts
1. Every row of the input Excel file corresponds to a specific chart for a given variable. Names of the charts in the Excel file needs to match the names of the charts as specified in the R code. Given its modular construction, additional charts types can be easily added.
2. In the Excel file, for each chart, a given chart style can be choosen. Given its modular construction, additional charts styles can be easily added.


# Scripts structure
* *59_run_all*: this is the main script, where directories should be defined (setwd, input_dir, chart_dir, gams_dir, input_excel). The following scripts are sourced automatically, and create the charts for 2 and 3 dimensional variables, respectively.
	* *51_2dimensions*
		* year_fx
		* import_data
		* changes_wrt_baseline 	
	* *52_3dimensions* 
		* year_fx
		* import_data
		* changes_wrt_baseline 	




