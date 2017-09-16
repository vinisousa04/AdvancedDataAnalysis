#' test_equality_between_groups
#'
#' Tests if the mean difference between two groups of a quantitative variable is
#' statiscally significant. In order to do so, it first tests if in both
#' groups the quantitative variable is normally distributed, if this is the case
#' then a independent sample t test takes place (in this case it is also
#' computed the Levene test for equal variance between groups). If the hypothesis
#' of normallity is rejected (p.value <= 1-confi) for at least one of the groups,
#' then the wilcox rank sum test takes place. NOTE: both tests (t test and
#' wilcox rank sum test) have as null hypothesis that the quantitative groups'
#' mean do not differ between groups.
#'
#'
#' @param df A data frame.
#' @param categorical The quoted name of the column which contains the group
#' variable.
#' @param quantitative The quoted name of the column which contains the
#' quantitative variable to be compared between groups.
#' @param confi Confidence level.
#'
#' @return If both groups have the quantitative variable normally distributed it
#' is returned a list with the normality, Levene and t test1s results. Otherwise,
#' it is returned the normality and Wilcox test's results.
#'
#' @examples
#' test_equality_between_groups(df=data, categorical="blood_type", quantitative="pressure",confi=0.99)
#'
#' @export

test_equality_between_groups <- function(df,categorical,quantitative,confi = 0.95){

  ####
  ## testing normality

  categories <- unique(df[,which(colnames(df)==categorical)])

  by_categorical <- vector(mode = "list",length = length(categories))
  names(by_categorical) <- categories

  tab_result <- tibble(Group = categories,
                       Test_Stat = vector(mode = "double",length(categories)),
                       DF = vector(mode = "double",length(categories)),
                       p_value = vector(mode = "double",length(categories)))

  for(category in categories){
    by_categorical[[category]] <- df[which(df[,categorical] == category),
                                     which(colnames(df)==quantitative)]
  }

  shap_test <- map(.x = by_categorical,.f = shapiro.test)

  for(category in categories){


    tab_result[which(tab_result[,"Group"]==category),-1] <- c(shap_test[[category]][["statistic"]],
                                                              length(by_categorical[[category]]),
                                                              shap_test[[category]][["p.value"]])

  }

  if(!any(tab_result$p_value <= 1 - confi)){
    ####
    # t_test

    # 1) Levene test (H0:the variance between groups is equal)
    lev_test <- car::leveneTest(df[,quantitative],df[,categorical],center = median)

    var_eq <- ifelse(lev_test$`Pr(>F)`[1] >= 1-confi, T,F)

    # 2) t test itself (H0: group means are equal)

    test <- t.test(df[,quantitative]~df[,categorical],mu=0,alt="two.sided",conf=confi,var.eq=var_eq,paired=F)

    output <- list(result_shapiro=tab_result, levene_test = lev_test,t_test = test)

  } else {
    ####
    # Wilcox rank sum test
    wilcox_test <- wilcox.test(by_categorical[[categories[[1]]]],by_categorical[[categories[[2]]]],mu=0,alt="two.sided",conf.int=T,conf.level=0.95,
                               paired=F,exact=F,correct=T)

    output <- list(result_shapiro=tab_result, wilcox_test = wilcox_test)
  }
  return(Result = output)
}
