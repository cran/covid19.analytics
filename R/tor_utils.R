# eunctions to analyze Toronto data about covid19 cases
# *NOT* final, work in progress...
#
# M.Ponce
# covid19.analytics

report.Tor <- function(colTgts=c("Source.of.Infection","Age.Group","Client.Gender","Outcome","Neighbourhood.Name"),
			report=TRUE,
			staticPlt=TRUE, horiz.plts=4,vert.plts=3, same.Yaxis=TRUE,
			interactiveFig=FALSE, interactive.display=TRUE) {
#' function to obtain main indicators from Toronto data
#'
#' @param  colTgts  optional argument to indicate which columns from the Toronto data to process
#' @param  report  optional argument indicating whether a report will be printed to the screen
#' @param  staticPlt  optional argument to indicate whether the 'static' graphical output is wanted or not
#' @param  	horiz.plts  number of plots in the horizontal direction
#' @param  	vert.plts  number of plots in the vertical direction
#' @param  	same.Yaxis  graphical argument to indicate if plots will use same y-axis
#' @param  interactiveFig  boolean flag to indicate whether interactice figures would be generated or not
#' @param  interactive.display  boolean flag to indicate whether the interactive plot will be displayed (pushed) to your browser
#'
#' @return  list with statistics by selected as indicated in colTgts
#'
#' #@export
#'
#' @importFrom  grDevices dev.new

	processCol <- function(col1, data=tor.data,col2=dateCol,date0=date0,date1=date1, grp.vars=NULL) {
		return( tapply(data[,c(col1)], substr(data[,col2],date0,date1), table) )
	}


	# read data
	tor.data <- covid19.Toronto.data(origin="OD",data.fmt='original')

	colNs <- names(tor.data)
	dateCol <- "Reported.Date"
	date0 <- 1
	date1 <- 7

	grouping.vars <- c("Client.Gender","Outcome")

	colTgts <- c("Source.of.Infection","Age.Group","Client.Gender","Outcome","Neighbourhood.Name")

	# identify unique months
	YYMM <- unique(substr(tor.data[,dateCol],date0,date1))

#### not being used########
if (FALSE) {
for (j in colTgts) {
if (! j %in% grouping.vars) {
	dev.new()
	par(mfrow=c(vert.plts,horiz.plts))

	print(j)
	for (mnt in YYMM) {
		#cat("Date: ",mnt,'\n')
		data.mnt <- tor.data[substr(tor.data[,dateCol],date0,date1)==mnt,]

#			header('',j)
#			print(table(data.mnt[,j],data.mnt[,grouping.vars]))
			xyz <- sapply(split(data.mnt,list(data.mnt[,j],data.mnt$Client.Gender,data.mnt$Age.Group)),dim)[1,]
			barplot(xyz, col=seq_along(unique(data.mnt$Client.Gender)), args.legend=list(x='topleft', bty = 'n') )
			title(paste(mnt,' -- ',j))
		}
	#	xy <- function(x, data=tor.data) { tapply(tor.data[,x],substr(tor.data[,dateCol],date0,date1),table)}
	#	print(mapply(colTgts,xy, data=data.mnt))
	#	header('-')
}
	}
}
###########################

	cat("Dates reported in dataset: ",YYMM,'\n')
	header('=')

	#tapply(tor.data$Client.Gender,substr(tor.data[,dateCol],date0,date1),table)
	x <- lapply(colTgts,processCol, tor.data,dateCol,date0,date1, grouping.vars)
	mnts <- names(x[[1]])
	names(x) <- colTgts

	if (report) {
		# preserve user options
		old.opts <- options("width")
		options(width=200)
		for (i in seq_along(colTgts)) {
			cat(colTgts[i],'\n')
			print( sapply(x[[i]],function(x){x[which(x==max(x))]}) )
			header('-')
		}

		# reset options to user ones
		options(old.opts)
	}

	if(staticPlt) {
		### preserve user graphical env.
		# save the state of par() before running the code
		oldpar <- par(no.readonly = TRUE)
		# restore the previous state after the fn is done, even if it fails, so the user environment is not altered
		on.exit(par(oldpar))
		#########


		## TOTALS per outcome
		dev.new()
		par(mfrow=c(2,3))
		for (i in seq_along(colTgts)) {
			if (colTgts[i] != "Outcome") {
				stck.var <- tor.data[,"Outcome"]
				if (colTgts[i]=="Neighbourhood.Name") {par(new=TRUE); par(mfrow=c(2,1))}
				barplot(table(stck.var,tor.data[,colTgts[i]]), col=seq_along(stck.var),
					legend=unique(stck.var), args.legend=list(x='topleft', bty = 'n')
					#, beside=TRUE
				)
			}
		}

		cat("Generating plots for ",paste(colTgts,sep="  "),'\n')

	for (i in seq_along(colTgts)) {
		dev.new()
		par(mfrow=c(vert.plts,horiz.plts))
#		mapply( function(x,mnts='',idx.mnt=1){barplot(x)}, x[[i]], MoreArgs=list(mnts=mnts,idx.mnt=1:length(mnts)) )
		par(new=FALSE)
		#mapply( function(mnt){mtext(mnt)}, names(x[[i]]))
		maxY <- max(sapply(x[[i]],max))
		Xentries <- max(sapply(lapply(sapply(x[[i]],names),length),max))+1
		for (j in seq_along(mnts)) {
			if (j==length(mnts)) {
				legend(x="top",legend=names(x[[i]][[j]]), pch=15, col=1:Xentries, bty='n')
			}
			if (same.Yaxis) {
					barplot(x[[i]][[j]],
						xlim=c(0,Xentries), ylim=c(0,maxY),
						col=1:Xentries
						)
			} else {
				barplot(x[[i]][[j]],
					col=1:Xentries
					)
			}
			mtext(mnts[j], side=1,line=2)
		}
		par(new=TRUE)
		par(mfrow=c(1,1))
		title(colTgts[i])
	}
	}


	if (interactiveFig) {
		# read data from City of Toronto in original format
#		torCty.data <- covid19.Toronto_city.data(data.fmt='original')

#		Cases.by.Date <- as.data.frame(torCty.data["Cases by Episode Date"] )

		#torCty_cases.ifig <- plot_ly(Cases.by.Date, x=~Cases.by.Episode.Date.Episode.Date, y=~Cases.by.Episode.Date.Case.Count, type='bar')
		#torCty_cases.ifig <- plot_ly(Cases.by.Date, x=~Cases.by.Episode.Date.Episode.Date, y=~Cases.by.Episode.Date.Active, type='bar')

		tor.data <- covid19.Toronto.data()

		nbrRecs <- length(tor.data)
		rec0 <- 5
		torCty_cases.ifig <- plot_ly(tor.data, x=~names(tor.data[case,rec0:(nbrRecs-1)]))
		#for (case in 1:4) {
			torCty_cases.ifig <- torCty_cases.ifig %>% add_trace(y=~as.numeric(tor.data[1,rec0:(nbrRecs-1)]), name=tor.data[1,nbrRecs], type='bar', visible= "legendonly")
			torCty_cases.ifig <- torCty_cases.ifig %>% add_trace(y=~as.numeric(tor.data[2,rec0:(nbrRecs-1)]), name=tor.data[2,nbrRecs], type='bar', visible= "legendonly")
			torCty_cases.ifig <- torCty_cases.ifig %>% add_trace(y=~as.numeric(tor.data[3,rec0:(nbrRecs-1)]), name=tor.data[3,nbrRecs], type='bar', visible=TRUE)
			torCty_cases.ifig <- torCty_cases.ifig %>% add_trace(y=~as.numeric(tor.data[4,rec0:(nbrRecs-1)]), name=tor.data[4,nbrRecs], type='bar', visible="legendonly")
		#}
		torCty_cases.ifig <- torCty_cases.ifig %>% layout(xaxis = list(title = 'Dates'), yaxis = list(title = 'Nbr of Cases'), barmode = 'group')



		if (interactive.display) {
			print(torCty_cases.ifig)
		} else {
			return(torCty_cases.ifig)
		}
	}

	return(x)
}


#######################################################

covid.infectionSRC.totals <- function(covid.data, tgt.col="Source.of.Infection") {

    #  covid.infectionSRC.totals
    # 
    #  This function prints out the total number of delay
    #  incidents for each incident type.
    # 
    #  @param covid.data The data frame containing the input data.
    
    ## Introductory sentence.
    cat("Total number of cases per incident type:",'\n')
    
    ## identify unique infection types.
    infectionSRC <- unique(covid.data[,tgt.col])
    
    ## compute totals for each infection type, and print.
    for (infectionType in infectionSRC) {
        cat('\t', infectionType, ' -- ',
            sum(covid.data[,tgt.col] == infectionType),'\n')
    }
    
}


#######################################################
    

covid.confirmed.range <- function(covid.data,
			tgt.col1="Classification", tgt.case="CONFIRMED",
			tgt.col2="Age.Group", tgt.range="40 to 49 Years") {

    #  covid.confirmed.range 
    # 
    #  This function calculates the total nunber of CONFIRMED cases in a given range of ages
    # 


    ##  Obtain the entries with CONFIRMED cases AND ages in a given range
    confirmed.cases <- covid.data[,tgt.col1]==tgt.case &
    			covid.data[,tgt.col2]==tgt.range

    ## Print out the result.
    cat("The number of confirmed cases in the 40-49 years age group is",
        sum(confirmed.cases, na.rm = TRUE), '\n')
    
}


#######################################################


covid.Most.fatalities <- function(covid.data, tgt.col="Neighbourhood.Name",
					tgt.col1='Outcome',tgt.outcome='FATAL') {

    #  covid.Most.fatalities
    # 
    #  This function calculates the neighborhood with the most fatalities in a given month of data
    # 
   

    # identify FATAL cases and slice corresponding Neighbourhoods 
    neighbourhoods <- covid.data[covid.data[,tgt.col1]==tgt.outcome,tgt.col]
   
    # aggregate Neighbourhood in a frequency table and sort it from the largest to the smallest nbr of ocurrences
    sorted.neigborhoods <- sort(table(neighbourhoods),decreasing=T)


    ## Print out just the first entry which represents the largest entry in the table
    cat('The neighbourhood with the most fatalities is',
        names(sorted.neigborhoods[1]), "with ",sorted.neigborhoods[1][1],".\n")

}
    

#######################################################
