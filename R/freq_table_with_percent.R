#'freq_table_with_percent
#'
#' Generates a frequency table with the percentage of each group.
#'
#' @param df A data frame
#' @param cat_variable The quoted column name that contains the categorical variable
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
