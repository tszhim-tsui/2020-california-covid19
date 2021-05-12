library(data.table)
library(plotly)



# Get current time for later use
currenttime <- paste("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (Pacific Time)", sep="")

# load source
download.file("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv", "covid19cases_test.csv")

cases <- fread("covid19cases_test.csv")

# mini clean up
names(cases) <- tolower(names(cases))
cases <- cases[date != ""]
cases[, date := as.Date(date)]
cases <- cases[area != "Out of state"]
cases <- cases[area != "Unknown"]

setorder(cases, area, date)


# get some basic info
ca_county_names <- cases[area_type=="County", sort(unique(area))]
ca_area_names <- cases[, sort(unique(area))]
date_earliest <- cases[, min(date)]
date_latest <- cases[, max(date)]


# calculate the per100k numbers
cases[, reported_cases_100k := round(reported_cases / population * 100000, 2)]


# dcast into different data.tables
cases_reported <- dcast(cases, date~area, value.var="reported_cases")
cases_reported100k <- dcast(cases, date~area, value.var="reported_cases_100k")




############################
# Cummulative num of cases #
############################

# copy data.tables to avoid subassignment
cases_cummulative <- copy(cases_reported)
cases_cummulative100k <- copy(cases_reported100k)

# Calculate cummulative sum
cases_cummulative[, (ca_area_names) := lapply(.SD, cumsum), .SDcols=ca_area_names]
cases_cummulative100k[, (ca_area_names) := lapply(.SD, cumsum), .SDcols=ca_area_names]



#################
# 7-day average #
#################

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
		
		placeholder <- c(placeholder, round(targetmean, 2))

	}

	# return the resulted vector	
	return(placeholder)
}


# copy data.tables to avoid subassignment
cases_7day <- copy(cases_reported)
cases_7day100k <- copy(cases_reported100k)

# calculate 7-day average
cases_7day[, (ca_area_names) := lapply(.SD, seven_day_mean), .SDcols=ca_area_names]
cases_7day100k[, (ca_area_names) := lapply(.SD, seven_day_mean), .SDcols=ca_area_names]



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
	temp_dt <- data.table(date=cases_cummulative[, date], y=cases_cummulative[, get(target_county)])
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
	temp_dt <- data.table(date=cases_cummulative100k[, date], y=cases_cummulative100k[, get(target_county)])
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
	temp_dt <- data.table(date=cases_reported[, date], y=cases_reported[, get(target_county)])
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
	temp_dt <- data.table(date=cases_reported100k[, date], y=cases_reported100k[, get(target_county)])
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
	temp_dt <- data.table(date=cases_7day[, date], y=cases_7day[, get(target_county)])
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
	temp_dt <- data.table(date=cases_7day100k[, date], y=cases_7day100k[, get(target_county)])
	fig_new_7day_100k <- add_trace(fig_new_7day_100k, data=temp_dt, type="scatter", mode="lines", 
								x=~date, y=~y, name=ca_county_names[i], line=list(shape="spline"))
}








########
# maps #
########

library(sf)
library(leaflet)

ca_county_sf <- st_read("../_01-manual-cleanup/CA_Counties_TIGER2016.shp")



# transform CRS: 3857 to EPSG 4326 for leaflet plotting
# https://stackoverflow.com/questions/44977471/transform-a-multipolygon-sf-object-in-r-from-xy-coordinates-to-lat-lon
ca_county_lnglat <- st_transform(ca_county_sf, "+init=epsg:4326")



# extract the latest 7-day numbers, melt them into 2-column format
# also extract "current_date" for plot titles
new_7day_latest <- cases_7day[dim(cases_7day)[1]]
new_7day_latest <- melt(new_7day_latest, id.vars=c("date"), measure.vars=ca_county_names)
current_date <- unique(new_7day_latest$date)
new_7day_latest$date <- NULL
names(new_7day_latest) <- c("NAME", "SEVENDAY")

new_7day_latest_100k <- cases_7day100k[dim(cases_7day100k)[1]]
new_7day_latest_100k <- melt(new_7day_latest_100k, id.vars=c("date"), measure.vars=ca_county_names)
new_7day_latest_100k$date <- NULL
names(new_7day_latest_100k) <- c("NAME", "SEVENDAY100K")



# combine 7-day numbers with the shapefile
ca_county_lnglat <- merge(ca_county_lnglat, new_7day_latest, by="NAME")
ca_county_lnglat <- merge(ca_county_lnglat, new_7day_latest_100k, by="NAME")


# make bins, colors, labels, then plot map
# cf https://rstudio.github.io/leaflet/choropleths.html


bins_SEVENDAY <- c(0, 2.5, 10, 25, 50, 100, 200, 500, 1000, Inf)
palette_SEVENDAY <- colorBin("Reds", domain=ca_county_lnglat$SEVENDAY, bins=bins_SEVENDAY)
label_SEVENDAY <- sprintf("<strong>%s</strong><br/>%g new cases/day, 7-day average",
  							ca_county_lnglat$NAME, ca_county_lnglat$SEVENDAY) %>%
  						lapply(htmltools::HTML)

map_new_7day <- 
leaflet() %>%
	setView(-119.449444, 37.166111, zoom=6) %>%
	addPolygons(data=ca_county_lnglat, color="black", weight=0.75, dashArray="3",
				fillColor=~palette_SEVENDAY(SEVENDAY), fillOpacity=0.7, 
				highlight = highlightOptions(weight=10, dashArray = "", bringToFront = TRUE), 
				label=label_SEVENDAY
				) %>%
	addTiles()





bins_SEVENDAY100K <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf)
palette_SEVENDAY100K <- colorBin("Reds", domain=ca_county_lnglat$SEVENDAY100K, bins=bins_SEVENDAY100K)
label_SEVENDAY100K <- sprintf("<strong>%s</strong><br/>%g cases/day/100k pop., 7-day average",
  							ca_county_lnglat$NAME, ca_county_lnglat$SEVENDAY100K) %>%
  						lapply(htmltools::HTML)

map_new_7day_100k <- 
leaflet() %>%
	setView(-119.449444, 37.166111, zoom=6) %>%
	addPolygons(data=ca_county_lnglat, color="black", weight=0.75, dashArray="3",
				fillColor=~palette_SEVENDAY100K(SEVENDAY100K), fillOpacity=0.7, 
				highlight = highlightOptions(weight=10, dashArray = "", bringToFront = TRUE), 
				label=label_SEVENDAY100K
				) %>%
	addTiles()










##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../docs/", overwrite=T)









