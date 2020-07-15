library(data.table)
library(plotly)



# Get current time for later use
currenttime <- paste("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (Pacific Time)", sep="")

# load source
download.file("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv", "statewide_cases.csv")
cases <- fread("statewide_cases.csv")
pop <- fread("../01-manual-cleanup/ca-county-population.csv")


# mini clean up
cases[, date := as.Date(date)]
cases <- cases[county != "Out Of Country"]
cases <- cases[county != "Unassigned"]


# get some basic info
ca_county_names <- cases[, sort(unique(county))]
date_earliest <- cases[, min(date)]
date_latest <- cases[, max(date)]


# might be stupid but allows me to vectorize the calculation:
# merge the population into the "cases" data.table, 
# then calculate the per100k numbers
cases <- merge(cases, pop, by="county")
cases[, totalcountconfirmed100k := round(totalcountconfirmed / population * 100000, 3)]
cases[, newcountconfirmed100k := round(newcountconfirmed / population * 100000, 3)]


# dcast into different data.tables
cases_total <- dcast(cases, date~county, value.var="totalcountconfirmed")
cases_total100k <- dcast(cases, date~county, value.var="totalcountconfirmed100k")
cases_new <- dcast(cases, date~county, value.var="newcountconfirmed")
cases_new100k <- dcast(cases, date~county, value.var="newcountconfirmed100k")






# Calculate (smoothed) 7-day average for the day change
# feed in a vector, returns a vector
seven_day_mean <- function(x){

	# initialize
	placeholder <- NULL
	
	# loop through each row
	vectorlength <- length(x)
	for (i in 1:vectorlength){

		# for first 6 rows, calculate the mean from row 1 to current row
		if (i %in% 1:6){
			targetmean <- mean(x[1:i], na.rm=T)
		} 
		
		# otherwise, calculate mean from current-6th row to current row
		else {
			targetmean <- mean(x[(i-6):i], na.rm=T)
		} 
		
		placeholder <- c(placeholder, round(targetmean, 3))

	}

	# return the resulted vector	
	return(placeholder)
}



# copy data.tables to avoid subassignment
cases_new_7day <- copy(cases_new)
cases_new_7day_100k <- copy(cases_new100k)

# calculate 7-day average
cases_new_7day[, (ca_county_names) := lapply(.SD, seven_day_mean), .SDcols=ca_county_names]
cases_new_7day_100k[, (ca_county_names) := lapply(.SD, seven_day_mean), .SDcols=ca_county_names]



#########
# plots #
#########

# cummulative # of cases
fig_cummu_raw <- 
plot_ly() %>%
	layout(title="Cumulative # of COVID-19 cases",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of cases"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_total[, date], y=cases_total[, get(target_county)])
	fig_cummu_raw <- add_trace(fig_cummu_raw, data=temp_dt, type="scatter", mode="lines", x=~date, y=~y, name=ca_county_names[i])
}


# cummulative # of cases / 100k
fig_cummu_100k <- 
plot_ly() %>%
	layout(title="Cumulative # of COVID-19 cases per 100k population",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of cases / 100k population"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_total100k[, date], y=cases_total100k[, get(target_county)])
	fig_cummu_100k <- add_trace(fig_cummu_100k, data=temp_dt, type="scatter", mode="lines", x=~date, y=~y, name=ca_county_names[i])
}



# new cases
fig_new_raw <- 
plot_ly() %>%
	layout(title="Daily new COVID-19 cases",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of new cases"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_new[, date], y=cases_new[, get(target_county)])
	fig_new_raw <- add_trace(fig_new_raw, data=temp_dt, type="scatter", mode="lines", x=~date, y=~y, name=ca_county_names[i])
}



# new cases per 100k
fig_new_100k <- 
plot_ly() %>%
	layout(title="Daily new COVID-19 cases per 100k population",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of new cases / 100k population"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_new100k[, date], y=cases_new100k[, get(target_county)])
	fig_new_100k <- add_trace(fig_new_100k, data=temp_dt, type="scatter", mode="lines", x=~date, y=~y, name=ca_county_names[i])
}





# new cases, 7-day average
fig_new_7day <- 
plot_ly() %>%
	layout(title="New COVID-19 cases, 7-day average",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of new cases, 7-day average"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_new_7day[, date], y=cases_new_7day[, get(target_county)])
	fig_new_7day <- add_trace(fig_new_7day, data=temp_dt, type="scatter", mode="lines", 
								x=~date, y=~y, name=ca_county_names[i], line=list(shape="spline"))
}





# new cases, 7-day average, per 100k population
fig_new_7day_100k <- 
plot_ly() %>%
	layout(title="New COVID-19 cases per 100k population, 7-day average",
		xaxis=list(title="Dates", range=c(date_earliest-1, date_latest +1)),
		yaxis=list(title="# of new cases / 100k population, 7-day average"))

for (i in 1:length(ca_county_names)){
	target_county <- ca_county_names[i]
	temp_dt <- data.table(date=cases_new_7day_100k[, date], y=cases_new_7day_100k[, get(target_county)])
	fig_new_7day_100k <- add_trace(fig_new_7day_100k, data=temp_dt, type="scatter", mode="lines", 
								x=~date, y=~y, name=ca_county_names[i], line=list(shape="spline"))
}








##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../docs/", overwrite=T)

