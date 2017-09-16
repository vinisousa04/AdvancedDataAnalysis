#'freq_table_with_percent
#'
#' Generates a frequency table with the percentage of each group.
#'
#' @param df A data frame.
#'
#' @param cat_variable The quoted column name that contains the categorical
#' variable.
#'
#' @return A table in which the rownames are the groups of the categorical
#'  variable. The first column if the count of the respective group and the
#'  second column is the percentage of the the group.
#'
#' @examples
#' freq_table_with_percent(df=data,cat_variable="eye_color")
#'
#' @export

freq_table_with_percent <- function(df,cat_variable){
  col <- which(colnames(df)==cat_variable)
  aux <- table(df[,col])

  freq_table <- cbind(
    t(t(aux)),
    t(t(aux)/sum(aux))*100)
  freq_table <- rbind(freq_table,colSums(freq_table))

  colnames(freq_table) <- c("Freq","Percent")
  rownames(freq_table) <- c(rownames(freq_table)[-nrow(freq_table)],'Total')


  return(data.frame(freq_table))
}
