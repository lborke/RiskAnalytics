

lambda.analytics = function(last_lambda) {
	# plotting last_lambda to get a feeling
	l_v = last_lambda[,2]
	n = length(l_v)
	last_val = l_v[n]

	plot(1:n, l_v, type = "h")

	print("Lambda Analytics Summary")
	print(summary(l_v))
	print(quantile(l_v, probs = seq(0, 1, 0.2)))

	# last lambda value is a quantile for this probability
	print(paste("last lambda value is the quantile for this probability:", length(l_v[l_v <= last_val]) / n))
	print(paste("last lambda value:", last_val))
}


data.analytics = function(yahoo_data, macro_data) {
	print(comp_norm <- sqrt(colSums(yahoo_data$returns_final^2)))
	print(macro_norm <- sqrt(colSums(macro_data^2)))

	par(mfrow=c(1,2))
	boxplot(comp_norm, main = "NASDAQ companies")
	boxplot(macro_norm, main = "Macro variables")

	dev.new()
	matplot(macro_data, type = "p", pch = 20, col = 1:6, cex = 1.5, main = "Macro variables", ylim=c(-0.3,1))
	legend("bottomleft", legend = colnames(macro_data), col = 1:6, pch = 15)

	macro_corr = cor(macro_data)
	print(round(macro_corr, 2))
	
	res = list()
	res$comp_norm = comp_norm
	res$macro_norm = macro_norm
	res$macro_corr = macro_corr
	
	return(res)
}
