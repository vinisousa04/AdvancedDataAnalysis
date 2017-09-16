#'boxplot_by_categories
#'
#' This is a wrapper for a ggplot2 boxplot for a variable with different groups,
#' and also adds the labels for outliers, if any.
#'
#' @param df A data frame.
#'
#' @param quantitative The quoted name of the column with the quantitative
#' variable.
#'
#' @param categorical The quoted name of the column with the categorical
#' variable, i.e. the variables that divides the data in groups.
#'
#' @param id The quoted name name of the column with the individual variables,
#' i.e. the observations. It is set as default to NULL for the cases where there
#' is no column identifying the indioviduals in the data set.
#'
#' @return Boxplot of the quantitative variable for each group in the
#' categorical variable. And also idintify the outlier, when they exist
#' is the data.
#'
#'
#' @examples
#' boxplot_by_categories(df=data, quantitative="pressure", categorical="blood_type",id="patient_name")
#'
#' @import tidyverse
#' @export

boxplot_by_categories <- function(df,quantitative,categorical,id=NULL){

  if(!is.null(id)){
    variable_names <- c(id,categorical,quantitative)

    sub_df <- df[,c(which(colnames(df)==variable_names[1]),which(colnames(df)==variable_names[2]),which(colnames(df)==variable_names[3]))]
    colnames(sub_df) <- c("id","categorical","quantitative")
    sub_df <- sub_df[complete.cases(sub_df),]

    is_outlier <- function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }

    sub_df %>%
      group_by(categorical) %>%
      mutate(outlier = ifelse(is_outlier(quantitative),id, as.numeric(NA))) %>%
      ggplot(aes(x=categorical,y=quantitative))+
      geom_boxplot(colour="darkblue",fill="lightblue")+
      geom_text(aes(label=outlier),na.rm = T,hjust=-0.3, colour="red")+
      xlab(variable_names[2])+ ylab(variable_names[3])
  } else {
    variable_names <- c(categorical,quantitative)

    sub_df <- df[,c(which(colnames(df)==variable_names[1]),which(colnames(df)==variable_names[2]))]
    colnames(sub_df) <- c("categorical","quantitative")
    sub_df <- sub_df[complete.cases(sub_df),]

    is_outlier <- function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }

    sub_df %>%
      group_by(categorical) %>%
      mutate(outlier = ifelse(is_outlier(quantitative),quantitative, as.numeric(NA))) %>%
      ggplot(aes(x=categorical,y=quantitative))+
      geom_boxplot(colour="darkblue",fill="lightblue")+
      geom_text(aes(label=outlier),na.rm = T,hjust=-0.3,colour="red")+
      xlab(variable_names[1])+ ylab(variable_names[2])
  }
}
