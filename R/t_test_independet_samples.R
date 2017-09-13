#'t_test_independent
#'
#' Tests if the diference in the mean of a variable between two groups is statistically significant.
#'
#' @param df A data.frame
#' @param categorical The quoted column name of the column which contains the groups
#' @param quantitative The quoted column name of the column which contains the variable to be tested
#' @param confi The confidence level choosen
#'
#' @return A list with two elements. The first being the t test results and the
#' second being Levene test result of equal variance, which has as null hypothesis
#' that the variances between the groups are equal.
#'
#' @examples
#' t_test_independent(df=data, categorical="blood_type", quantitative="pressure",confi=0.99)
#'
#' @import car
#' @export


t_test_independent_samples <- function(df,categorical,quantitative,confi = 0.95){

  # 1) Levene test (H0:the variance between groups is equal)
  lev_test <- car::leveneTest(df[,quantitative],df[,categorical],center = median)

  var_eq <- ifelse(lev_test$`Pr(>F)`[1] >= 1-confi, T,F)

  # 2) t test itself (H0: group means are equal)

  test <- t.test(df[,quantitative]~df[,categorical],mu=0,alt="two.sided",conf=confi,var.eq=var_eq,paired=F)

  return(list(t_test = test, levene_test = lev_test))
}
