#' @title Finds combinations of numbers that add up to a target value.
#' @description This was created to find a combination of check deposits that add up to a total deposit amount.
#' @param nums A vector of numbers.
#' @param target The target number.
#' @return A string with a list of numbers.
#' @export
findIt <- function(nums, target){
  # Given a list of numbers in vector nums, it finds the combinations of those numbers that add up to the number in target
  for(i in 1:length(nums)){
    a <- combn(nums, i) # Creates a matrix of all possible combinantions of length i
    a <- data.frame(nums = t(a)) # Transposes the matrix
    a$sums <- rowSums(a) # sums up the columns for each row of the matrix
    idx <- match(target, a$sums) # Gives the row number of the combination of numbers that matches the target number
    if(!is.na(idx)){
      b <- a[idx,]
      # return(b)
      assign(paste(b, i, sep = "_"), b, envir = .GlobalEnv)
    }
  }
}
