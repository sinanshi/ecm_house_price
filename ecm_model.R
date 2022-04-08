library(readxl)
library(ggplot2)
library(data.table)


read_sheet <- function(sheet_id, country) {
	read_excel("data.xlsx", sheet=sheet_id, col_names=T)[, c("Date", country)]
}

load_country <- function(country) {
	sheet_names <- readxl::excel_sheets("data.xlsx")
	country_tb <- data.table(read_sheet(1, country)[, 1])
	for (i in seq(sheet_names)) {
		new_tb <- read_sheet(i, country)
		new_tb <- data.table(new_tb)[!is.na(new_tb[[2]])]
		names(new_tb) <- c("Date", sheet_names[i])
		country_tb <- merge(country_tb, new_tb, by="Date")
	}
	return(country_tb)
}

create_lag_vec <- function(x, lag) {
	if (lag > length(x) - 1) 
	  stop("lag size should be smaller than the vector size  1")
	return(c(rep(NA, lag), x[1:(length(x)-lag)]))
}

create_delta_lags <- function(x, lag) {
	create_lag_vec(x, lag) - create_lag_vec(x, lag+1)
}

param <- list(
y = "rent_index", 
long_term = c("rent_index", "real_house_prices", "long_term_rates"), 
transitory= c("short_term_rates", "price_income_ratio", "price_rent_ratio"),
transitory_lags = c(1, 3, 4)
)

prepare_variables <- function(x, param) {
	y_name <- param[["y"]]
	 <- x[[y_name]]
	for (i in param[["long_term"]]) {
		
	}
}

