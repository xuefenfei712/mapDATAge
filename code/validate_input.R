
colname=c( "Sample","Age","latitude","longitude","Site","Species","Sex")
validate_input_files <- function(table) {
  
  # 1. Column names exist.
  if (is.null(colnames(table))) {
    stop("The input table should have column names.")
  }
  
  # 2. One of the columns is inside"
  if (!(all(colname %in% colnames(table)))) {
    stop("Please make sure that all required columns are provided in the uploaded table")
  }
  
  # 3. All columns apart from sum.taxonomy should be numeric
  if (!(all(sapply(table[,c("Age","latitude","longitude")],is.numeric)))) {
    stop("Please make sure that all required columns are provided in the uploaded table!")
  }
}
  