for (i in dir(pattern=".csv")) {
	cat("processing ",i,'\n');
	covid19.cases <- read.csv(i,header=TRUE);
	RDSfilename <- paste0(i,'.RDS')
	cat("saving INTO  ",RDSfilename,' \n')
	save(covid19.cases, file=RDSfilename)
	cat('\n\n')
	cat('------------','\n')
}
