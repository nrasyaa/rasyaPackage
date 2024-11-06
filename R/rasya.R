data("airquality")

replace_na_with_median <- function(data) {

  if (!is.data.frame(data)) {
    stop("The input must be a data frame.")
  }
  for (col_name in names(data)) {
    if (is.numeric(data[[col_name]])) {
      column_median <- median(data[[col_name]], na.rm = TRUE)
      data[[col_name]][is.na(data[[col_name]])] <- column_median
    }
  }
  return(data)
}


missing_values <- sapply(airquality, function(x) sum(is.na(x)))
print("Missing Values in each column:")
print(missing_values)

modified_airquality <- replace_na_with_median(airquality)

print("Modified dataset without missing values:")
print(modified_airquality)

missing_values_after <- sapply(modified_airquality, function(x) sum(is.na(x)))
print("Missing Values after replacing:")
print(missing_values_after)
