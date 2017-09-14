#' test_chi_square
#'
#' It is a wrapper for the chisq.test. Two main diffenrece are that this function
#' evaluates if the expected values of each cells is greater than 5 before computing
#' test and it also adds a observed proportion table.
#'
#'
#'
#' @param df A data frame.
#' @param cat_1 The quoted column name which contains one of categorical variable.
#' @param cat_2 The quoted column name which contains the other categorical
#' variable to be adressed.
#'
#' @return The same list as chisq.test would return plus the observed proportion.
#'
#' @examples
#' test_chi_square(df = data, cat_1 = "Pclass", cat_2 = "Survived")
#'
#' @export


test_chi_square <- function(df,cat_1,cat_2){

  nomes <- c(cat_1,cat_2)
  category_1 <- unlist(df[,which(colnames(df)==cat_1)])
  category_2 <- unlist(df[,which(colnames(df)==cat_2)])

  chi_sq_test <- chisq.test(category_1,category_2)
  names(attributes(chi_sq_test$observed)[[2]]) <- nomes
  names(attributes(chi_sq_test$expected)[[2]]) <- nomes
  ####
  # Testing if all expected values are greater than 5
  if(!any(chi_sq_test$expected <= 5)){
    chi_sq_test$observed_proportion <- prop.table(chi_sq_test$observed)
    chi_sq_test$observed_proportion <- as_tibble(round(v$observed_proportion*100,2)) %>%
      arrange(Pclass)
    names(attributes(chi_sq_test$observed_proportion)[[2]]) <- nomes

    output <- chi_sq_test
  } else {
    warning("There are cells with expected value less than 5.")
    expected <- chi_sq_test$expected
    output <- expected
  }
  return(output)
}
