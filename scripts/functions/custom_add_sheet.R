# a function that adds a sheet to the workbook that is created
custom.add.sheet <- function(d,  s_name){ # tmp_wb = wb,
  tmp_wb <<- wb # superassignment to get the workbook
  openxlsx::addWorksheet(tmp_wb,
                         sheetName = s_name)
  openxlsx::writeData(tmp_wb,
                      sheet = s_name,
                      d)
}