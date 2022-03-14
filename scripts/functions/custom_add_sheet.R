# a function that adds a sheet to the workbook that is created
custom.add.sheet <- function(d,  s_name){ # tmp_wb = wb,
  
  # debug: 
  s_name <- paste0(rep("a", 32), collapse = "")
  
  #' Get the Workbook created in the run_all.R file
  #' (using superassignment to look into upper environment)
  tmp_wb <<- wb
  
  #' The name needs to be shorter or equal 31
  if(nchar(s_name)>31){
    warning( paste("Sheet name ", s_name, "was shortened to fit 31 char limit"))
    s_name <- substr(s_name, 0, 31)
  } 
  
  #' Just in case... if the sheet is not unique:
  #' add some random characters until it is unique
  while(s_name %in% openxlsx::sheets(tmp_wb)){
    warning( paste("Had to change sheet name:", s_name, "because not unique!"))
    s_name <- paste0(substr(s_name, 0, 27), "_", paste0(sample(letters, 3), collapse = "")) # add random letters
  }

  #' Add sheet and write data on it
  openxlsx::addWorksheet(tmp_wb,
                         sheetName = s_name)
  openxlsx::writeData(tmp_wb,
                      sheet = s_name,
                      d)
}