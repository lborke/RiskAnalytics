
get.nasdaq.companies = function(plotMarketCap = TRUE) {
	# companylist2015 - downloaded from this source
	# http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=Finance&sortname=country&sorttype=1
	# for symbols in Yahoo finance

	# companylist from web
	nd = getURL("http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=Finance&render=download", ssl.verifypeer = FALSE)
	n_csv = read.csv(text = nd, na.strings = ".")
	companylist = n_csv[with(n_csv, order(-MarketCap)), ]

	if (plotMarketCap) {
		par(mfrow=c(1,3))
		plot(1:nrow(companylist), companylist$MarketCap, xlab = "all companies sorted by market capitalization")
		plot(1:200, companylist$MarketCap[1:200], xlab = "top 200 companies sorted by market capitalization")
		plot(1:100, companylist$MarketCap[1:100], xlab = "top 100 companies sorted by market capitalization")
	}
	
	return(companylist)
}


get.yahoo.data = function(companylist, max_comp_num = 200, from_date = "2006-12-29", to_date = Sys.Date(), loop_break_in_sec = 0.1, bad_list = c()) {
	# number of firms used
	# sometimes data for a company can not be captured (bad internet connection,
	# problems with correct name) in this case extend this list the list can vary from time to time
	#bad_list = c("ZIONZ", "SNFCA", "KMPA", "CATYW", "MBFIP", "HAWKB", "JLL", "CTRX", "HCC", "HCBK", "CYN", "SUSQ")
	
	# Array with firm names sorterd by market capitalization
	firm_names = as.character(companylist[, 1])

	# calls Yahoo Finance through internet
	example      = getSymbols(Symbols = firm_names[1], src = "yahoo", env = NULL, from = from_date, to = to_date, warnings = FALSE)
	example_time = as.matrix(example[-1, 1])
	time_points  = nrow(as.data.frame(example))

	# setting parameters for the loop number of firms
	n = length(firm_names)
	# counter (initial value is 1)
	s = 1

	# creating initial matrix for all firms and all time points
	firms_closed_price           = matrix(0, time_points, n)
	colnames(firms_closed_price) = firm_names

	loading_error_list = c()
	diff_length_list = c()

	# Main loop : data from yahoo to firms_closed_price
	for (i in 1:n) {
	  # 1) Check whether contained in bad list, if then skip
	  if (firm_names[i] %in% bad_list) { next }
	  # 2) check whether the complete time series is available, if not skip and print name
	  result = try( prices <- getSymbols(firm_names[i], src = "yahoo", from = from_date, to = to_date, auto.assign = FALSE) )
	  if ("try-error" %in% class(result) ) {
		print(paste("loading error :", firm_names[i]))
		loading_error_list = c(loading_error_list, firm_names[i])
		next
	  } else if (nrow(prices) != time_points) {
		# skip this firm if different length of time points
		print(paste("diff length :", firm_names[i]))
		diff_length_list = c(diff_length_list, firm_names[i])
		next
	  } else {
		  # 3) get data and save in firms_closed_price
		  prices_data = as.data.frame(prices)
		  firms_closed_price[, i] = as.vector(prices_data[, 6])
		  print(paste(s, ":", firm_names[i]))
		  # increase counter
		  s = s + 1
		  #system break
		  Sys.sleep(loop_break_in_sec)
	  }
	  # 4) stop if max_comp_num achieved
	  if (s > max_comp_num) 
		break
	}

	# checking which firms were filled
	cs = colSums(firms_closed_price)
	# taking submatrix from firms_closed_price with 200 companies
	collected_firms = names(cs[cs > 0])
	data_firms      = firms_closed_price[, collected_firms]

	# transform the company_prices_200 into log returns
	returns_final           = diff(log(data_firms))
	rownames(returns_final) = rownames(example_time)

	# compare and check that the company names are the same as in the last auto_download run
	# data_check    = read.csv("input/200_firms_returns_and_scaled_macro_2016-02-04.csv")

	# ch_r = colnames(returns_final) %in% colnames(data_check[,-1])
	# length(ch_r[ch_r == TRUE]) ; length(ch_r[ch_r == FALSE])

	res = list()
	res$returns_final = returns_final
	res$loading_error_list = loading_error_list
	res$diff_length_list = diff_length_list
	
	return(res)
}


get.macro.data = function(from_date = "2006-12-28", to_date = Sys.Date() - 1, plot_macros = TRUE) {
	macro_names = c("^VIX", "^GSPC", "IYR", "3MTCM", "Yield", "Credit")

	# Part 1: download VIX, GSPC (S&P500) and IYR (iShares Dow Jones US Real Estate) from yahoo finance
	VIX  = as.matrix(getSymbols(Symbols = macro_names[1], src = "yahoo", env = NULL, from = from_date, to = to_date, warnings = FALSE)[, 6])
	GSPC = as.matrix(getSymbols(Symbols = macro_names[2], src = "yahoo", env = NULL, from = from_date, to = to_date, warnings = FALSE)[, 6])
	IYR  = as.matrix(getSymbols(Symbols = macro_names[3], src = "yahoo", env = NULL, from = from_date, to = to_date, warnings = FALSE)[, 6])

	# data of the first three variables
	data_ft              = as.matrix(cbind(VIX, GSPC, IYR))
	# transform GSPC and IYR into log returns without VIX
	First_three_macro_in = cbind(data_ft[-1, 1], diff(log(data_ft[, -1])))
	# remove last row
	First_three_macro    = First_three_macro_in[-nrow(First_three_macro_in), ]

	# Part 2: download the other 3 macro variables from Federal reserve Bank;
	# measure the length of first three macro variables, so that the last three variables have the same length with them.
	macro_length = nrow(First_three_macro)

	# 3 month Treasury change download 3 month Treasury maturities
	#x = getURL("https://research.stlouisfed.org/fred2/series/DGS3MO/downloaddata/DGS3MO.csv", ssl.verifypeer = FALSE)
	#ThreeMT      = as.numeric(as.matrix(read.csv(text = x, na.strings = ".")[, -1]))
	ThreeMT = na.omit(as.vector(read.csv("https://research.stlouisfed.org/fred2/series/DGS3MO/downloaddata/DGS3MO.csv", na.strings = ".")[, -1]))
	# set the length of this variable the same as the first three variables, take last c values
	output_ThreeMT = ThreeMT[(length(ThreeMT) - macro_length):length(ThreeMT)]
	# calculate the 3 month Treasury change
	change_ThreeMT = diff(output_ThreeMT)

	# Slope of yield curve download 10 year Treasury maturities
	# x = getURL("https://research.stlouisfed.org/fred2/series/DGS10/downloaddata/DGS10.csv", ssl.verifypeer = FALSE)
	# Tenyield      = as.numeric(as.matrix(read.csv(text = x, na.strings = ".")[, -1]))
	Tenyield = na.omit(as.vector(read.csv("https://research.stlouisfed.org/fred2/series/DGS10/downloaddata/DGS10.csv", na.strings = ".")[, -1]))
	output_Tenyield = Tenyield[(length(Tenyield) - macro_length):length(Tenyield)]
	# calculate Slope of yield curve
	slope_yield     = (output_Tenyield - output_ThreeMT)[-1]

	# credit spread download BAA
	# x <- getURL("https://research.stlouisfed.org/fred2/series/DBAA/downloaddata/DBAA.csv", ssl.verifypeer = FALSE)
	# DayBAA      = as.numeric(as.matrix(read.csv(text = x, na.strings = ".")[, -1]))
	DayBAA = na.omit(as.vector(read.csv("https://research.stlouisfed.org/fred2/series/DBAA/downloaddata/DBAA.csv", na.strings = ".")[, -1]))
	output_DayBAA = DayBAA[(length(DayBAA) - macro_length):length(DayBAA)]
	# calculate credit spread
	credit_spread = (output_DayBAA - output_Tenyield)[-1]

	# combine all macro variables
	six_macro = cbind(First_three_macro, change_ThreeMT, slope_yield, credit_spread)
	m = matrix(0, dim(six_macro)[1], dim(six_macro)[2])
	# scale variables to [0,1]
	for (i in 1:ncol(six_macro)) { m[, i] = (six_macro[, i] - min(six_macro[, i]))/(max(six_macro[, i]) - min(six_macro[, i])) }
	colnames(m) = macro_names
	
	if (plot_macros) {
		boxplot(m)
	}
	
	return(m)
}


combine.data = function(yahoo_data, macro_data, print_summary = TRUE, summary_dim = c(1:6, 102:107)) {
	full_data = cbind(yahoo_data$returns_final, macro_data)
	full_data = round(full_data, digits = 9)

	Date                = as.data.frame(format(strptime(rownames(full_data), "%Y-%m-%d"), "%d/%m/%Y"))
	names(Date)         = "Date"
	rownames(full_data) = NULL
	final_data          = cbind(Date, full_data)
	
	if (print_summary) {
		print(paste("Dimension of the final data:", dim(final_data)[1], "*", dim(final_data)[2]))
		print(final_data[1:10, summary_dim])
	}
	
	return(final_data)
}

