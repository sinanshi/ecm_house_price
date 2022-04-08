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
							long_term = c("real_house_prices", "long_term_rates"), 
							transitory= c("short_term_rates", "price_income_ratio", "price_rent_ratio"),
							transitory_lags = c(1, 3, 4)
)

prepare_variables <- function(x, param) {
	y_name <- param[["y"]]
	x$y_ll1 <- x[[y_name]]
	
	cat("Long term variables: ")
	for (i in param[["long_term"]]) {
		if (!i %in% names(x)) stop(i, "is not a right column name in", names(x))
		x[[paste0(i, "_ll1")]] <- create_lag_vec(x[[i]], 1)
		cat(paste0("\n  - ", i, " (", i, "_ll1)"))
	}
	cat("\n")

	cat("Transitory variables: ", paste0(param[["transitory"]], " x ", param[["transitory_lags"]], sep=", "), "\n")
	for (i in seq(param[["transitory"]])) {
		if (!param$transitory[i] %in% names(x)) stop(param$transitory[i], "is not a right column name in", names(x))
		for (j in seq(param$transitory_lags[i])) {
			x[[paste0(param$transitory[i], "_tl", j)]] <- create_delta_lags(x[[param$transitory[i]]], j)
			cat(paste0(paste0("  - ", param$transitory[i], "_tl", j)), "\n")
		}
	}
	return(x)
}

france_test <- prepare_variables(load_country("France"), param)

ecm_model <- function(param, country) {
	cat("----------------------------------\n")
	cat("ECM model for", country, "\n")
	cat("----------------------------------\n")
	dt <- load_country(country)
	cat("Data cleaning: data only available from ", dt[1]$Date, "to", dt[nrow(dt)]$Date, "\n")
	dt <- prepare_variables(dt, param)
	dt <- dt[!is.na(rowSums(dt[, -1]))]
	cat("The data set is further reduced due to lagging variables: ", dt[1]$Date, "to", dt[nrow(dt)]$Date, "\n")
	names(dt)[which(names(dt) == param$y)] <- "y"
	return(lm(y ~ ., data=dt[, -1]))
}

for (i in c("France", "Germany", "Ireland", "Italy", "Netherlands", "Spain"))
	print(ecm_model(param, i))

