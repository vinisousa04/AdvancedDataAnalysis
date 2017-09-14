#' test_equality_paired
#'
#' Tests if the mean difference of the same group in different situation, e.g.
#' before and after a training, is statiscally different. In order to do so first
#' it is tested if the differen between the situations, e.g.again the time before
#' the training and the time after the training is normally distibuted. If it is
#' then a paired t test takes place, otherwise it is computed the Wilcoxon Signed-Rank
#' Test.
#'
#'
#'  @param df A data frame containing the values of the different situations in different
#'  columns, example in one column the time before the training and in other column the time
#'  after the training.
#'
#' @param time1 The quoted column name of one of the situations.
#' @param time2 The quoted column name of the other situation.
#' @param confi The confidence level.
#' @param alternative The alternative hypothesis for the t test and wilcox test. The default
#' value is "two.sided", others options are "greater" and "less".
#'
#' @return A list containg the shapiro test of normality and the appropriate test of mean/median
#' equality.
#'
#' @examples
#'
#' test_equality_paired(df = data, time1=score_before, time2=score_after,confi = 0.95, alternative = "two.sided")
#'
#' @export

test_equality_paired <- function(df,time1,time2,confi = 0.95, alternative = "two.sided"){

  Time1 <- unlist(df[,which(colnames(df)==time1)])
  Time2 <- unlist(df[,which(colnames(df)==time2)])

  ####
  ## Computing the difference
  difference <- Time2 - Time1

  ####
  ## testing difference normality
  test_shap <- shapiro.test(difference)

  if(test_shap$p.value <= 0.05){
    ####
    # Normality rejected

    test_wilcox_signed <- wilcox.test(x = Time1,
                                      y = Time2,
                                      mu = 0, alternative ="two.sided",
                                      conf.int = T,conf.level = confi,
                                      paired = T,exact = F)

    output <- list(result_shapiro = test_shap,
                   result_wilcox_signed = test_wilcox_signed)
  } else {
    ####
    # Normality not rejected

    # testing euality of variance
    test_levene <- leveneTest(Time1,Time2,center = median)

    equal_variance <-  ifelse(test_levene$`Pr(>F)`[1] <= 1-confi,F,T)

    t_test <- t.test(Time1,
                     Time2,
                     mu=0,alternative ="two.sided",
                     paired=T,
                     var.equal = equal_variance,
                     conf.level = confi)
    output <- list(result_shapiro = test_shap,
                   result_t_test = t_test)
  }
  return(output)
}
