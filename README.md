Here you find some code to visualize with Tidyverse GAMS simulations output. 

General info:
1. Read in gdx data using the **gdxrrw** package. This requires that GAMS is already installed.
2. At the moment there is just one R file, in which common functions are defined, and used to import and plot different variables.
3. The input folder is empty, as the data is very heavy.
4. At the moment there are two functions that can be used to process different variables:
		* **import.data**: in general there will be multiple gdx files, one for each simulation. This function can be applied to a vector of names containing the names of the simulations, to read in specific variables from each simulation:
				* variables of any dimensions can be imported. The labels of the dimensions are matched automatically
				* by setting **var_name0 = T** the values are multiplied by its "var0" value. 
				* by setting **inScale = T** the values are rescaled using the parameter "inScale"
		* **growth.changes**: the output of the above function can be passed to this function in order to calculate growth rates between values in the same simulation, and perchantage changes wrt to the baseline. Note that the number of dimensions to pass to **dplyr::group_by** is detected automatically (assumed to be everything except for the column containing the values, and the "t" dimension)
4. To produce plots for the different dimensions we use "purr:map" or "purr:map2". When the dimensions are more than one, the combinations can be calculated using "tidyr::crossing". Plots are saved in a folder having the same name as the variable.  
