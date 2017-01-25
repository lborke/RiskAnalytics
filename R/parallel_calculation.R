
# number of new working days
# Number of cores on which calculations will be conducted in parallel
# winsize = 126

parallel.lasso.computation = function(final_data, max_companies, new_days = 5, parallel_cpu = 4, p = 0.05, winsize = 60, work_dir,
									  plotDataCheck = TRUE, saveCSV = FALSE) {

	data_in = final_data[,-1]
	data    = as.matrix(data_in)

	# 3 Plots will be generated for a quick visual check of data consistency
	if (plotDataCheck) {
		par(mfrow=c(3,1))
		hist(sqrt(colSums(data^2)), breaks = 400)
		boxplot(sqrt(colSums(data^2)))
		boxplot(sqrt(colSums(data[,1:max_companies]^2)))
	}

	setwd(work_dir)
	if (saveCSV) {
		dir_name = as.character(Sys.Date())
		# create current folder for this calculation process
		dir.create(dir_name, showWarnings = FALSE)
		setwd(dir_name)
	}

	## Parallelization and execution of calculations (5 Steps)
	## Group of companies - Output are max_comp_num csv-Files (one for each company, without macro vars)
	# Parameters for the main algo
	lengthfull  = nrow(data)
	var_n 		= ncol(data)
	i_max	= lengthfull - winsize
	i_start = i_max - new_days + 1

	# Set up environment for parallelized calculations
	# 1. Initialisation of snowfall
	sfInit(parallel = TRUE, cpus = parallel_cpu)

	# 2. Loading data to be available on all used cores
	sfExportAll()
	# 3. Define process_lasso function (will be called in next step of parallelization)
	process_lasso = function(c_akt){
		if (c_akt > var_n) break

		y            = data[,c_akt]
		x            = data[,-c_akt]
		lambda_initial = matrix(0, (lengthfull - winsize), 1)
		beta_series	   = matrix(0, (lengthfull - winsize), var_n - 1)

		for (i in i_start : i_max){
		  ycut = y[i:(i + winsize)]
		  xcut = x[i:(i + winsize),]
		  fit  = lambda_linear_func(ycut, xcut, p)
		  lambda_initial[i] = fit$lambda
		  beta_series[i,]   = fit$beta
		}

		if (saveCSV) {
			write.csv(lambda_initial, file = paste("lambda_comp", c_akt, ".csv", sep =""), row.names = FALSE)
			write.csv(beta_series, file = paste("beta_series_comp", c_akt, ".csv", sep =""), row.names = FALSE)
		}

		res = list()
		res$lambda = lambda_initial
		res$beta = beta_series
		return(res)
	}

	ptm = proc.time()

	# 4. Distribute calculation on number of cores
	parResult = sfLapply(1:max_companies, process_lasso)
	# parResult = sfLapply(1:4, process_lasso)

	# 5. Stop snowfall
	sfStop()

	# Stop the clock
	print(proc.time() - ptm)

	return(parResult)
}



aggregate.parallel.results = function(final_data, max_companies, parResult, new_days = 5, winsize = 60, work_dir, file_suffix = "",
									  fromCSV = FALSE, addPreviousLambda = FALSE, last_lambda_fname = "lambda_mean_100vars_2017-01-24.csv") {
	data_in	= final_data
	full_lambda_mat = as.data.frame(data_in[-c(1:winsize), 1])

	if (fromCSV) {
		# aggregate from csv files
		for (i in 1 : max_companies){
			comp_akt = read.csv(paste("lambda_comp", i, ".csv", sep =""))
			full_lambda_mat = cbind(full_lambda_mat, comp_akt)
		}
	} else {
	# aggregate from memory : parResult
		for (i in 1 : max_companies){
		# for (i in 1:4){
			full_lambda_mat = cbind(full_lambda_mat, parResult[[i]]$lambda)
		}
	}

	#dim(full_lambda_mat)

	lambda_mean = cbind(as.data.frame(full_lambda_mat[,1]), apply(full_lambda_mat[,-1], 1, mean))
	colnames(lambda_mean) = c("date", "price")

	i_last = nrow(lambda_mean) - new_days
	lambda_mean_last = lambda_mean[-c(1:i_last),]

	wd_lambda_hist = "lambda_hist"
	setwd(paste(work_dir, "/", wd_lambda_hist, sep = ""))

	if (addPreviousLambda) {
		# optional: if last_lambda_fname is provided
		last_lambda	= read.csv(last_lambda_fname)
		last_lambda = rbind(last_lambda, lambda_mean_last)
	} else {
		last_lambda = lambda_mean_last
	}

	new_lambda_fname = paste("lambda_mean_100vars_", Sys.Date(), file_suffix, ".csv", sep= "")
	write.csv(last_lambda, file = new_lambda_fname, row.names = FALSE)

	return(last_lambda)

}
