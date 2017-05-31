#' @title Combine all files from a directory into one dataframe
#' @description Useful if you have a folder with many instances of essentially the same dataframe. This assumes that all of the files are tidy datasets.
#' @param df The name of the dataframe that will be returned.
#' @return Dataframe
#' @export
readFiles <- function(df){
  all.files <- list.files() #Gets the name of all the files in the directory

  contacts.readin <- function(files="all"){
    if(files=="all"){
      data <- lapply(all.files, contacts.readin)
      ldply(data) ##Turns the list into a dataframe.
    }
    else {
      table <- read.csv(files,skip=2)

    }
  }
  df <- contacts.readin()
}
