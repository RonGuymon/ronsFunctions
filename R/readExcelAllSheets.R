#' @title Read in all Excel sheets from a single Excel file.
#' @description Each sheet will be saved as a separate dataframe using the name of the tab.
#' @param filename The name of the Excel file, in quotes. The path should also be included if it's not in the working directory.
#' @return Dataframes.
#' @export
readExcelAllSheets <- function(filename = ""){
  sheets <- excel_sheets(filename) #Gets each name of all worksheets into a list
  for(i in 1:length(sheets)){
    assign(paste(sheets[i]), read_excel(filename,sheet=sheets[i]), envir = globalenv())
  }
}
