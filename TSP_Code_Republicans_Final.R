cat("\014") # clear screen
rm(list=ls()) # clear global environment

#Packages you must install befor you install the github ggmap
install.packages("TSP")
install.packages("tspmeta")
install.packages("lazyeval")
install.packages("ggmap")
install.packages("gmapsdistance")
library(TSP) # For Traveling Salesman
library(tspmeta) # For Traveling Salesman
library(ggmap) # For location of cities from Google
library(ggplot2)
library(gmapsdistance)
#Run after the above has been installed
#AIzaSyAaFaAgCJcDDTZx05_ZZbBRP-q21C0nzFM - google key
devtools::install_github("dkahle/ggmap")
register_google(key = "AIzaSyAaFaAgCJcDDTZx05_ZZbBRP-q21C0nzFM")
#user will probably have to use there own google key which can be found here: https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode


#============STARTING POINT========================================================================
#========================================================================================================================
#========================================================================================================================

#==================Tour 1 ================================================================================================

cities <- c("Washington D.C.",
          "North Charleston, SC",
            "Des Moines, IA",
            "St. Paul, MN",
            "Madison, WI",
            "Chicago, IL", 
            "Clear Lake, IA", 
            "Cedar Falls, IA",
            "Urbandale, IA ",
            "Sioux Center, IA", 
            "Iowa City, IA" )
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Washington D.C.")
start
end <- which(labels(distance_matrix) == "Des Moines, IA")
end

atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Washington D.C.",labels(cut_tour(tour, start_end)), "Des Moines, IA")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1

#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)



Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 1 - Iowa Caucus",
               x ="Long", y = "Lat")
mp
tour
#gmapsDistance
#========================================================================================================================

#=======================Tour 2===========================================================================================

cities <- c("Des Moines, IA",
            "Portsmouth, NH",
            "Salem, NH",
            "Exeter, NH",
            "Holderness, NH",
            "Manchester, NH")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Des Moines, IA")
start
end <- which(labels(distance_matrix) == "Manchester, NH")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
path_stuff <- c("Des Moines, IA",labels(cut_tour(tour, start_end)), "Manchester, NH")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1

#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)



Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 2",
               x ="Long", y = "Lat")
mp
tour
#==================================================================================================================


#=============Tour 3 ================================================================================

cities <- c("Manchester, NH",
            "Farmington NH",
            "Nashua, NH",
            "Walterboro, SC",
            "Beaufort, SC",
            "Pendleton, SC",
            "Greenville, SC")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)
distance_matrix

m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Manchester, NH")
start
end <- which(labels(distance_matrix) == "Greenville, SC")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Manchester, NH",labels(cut_tour(tour, start_end)), "Greenville, SC")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1
#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)



Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 3",
               x ="Long", y = "Lat")
mp
tour
#==================================================================================================================

#===================Tour 4 =======================================================================================

cities <- c("Greenville, SC",
            "Radford, VA",
            "Minneapolis, MN",
            "Highfill, AR",
            "Oklahoma City, OK",
            "Sparks, NV",
            "Las Vegas, NV",
            "Fort Worth, TX",
            "Millington, TN",
            "Madison, AL",
            "Atlanta, GA",
            "Valdosta, GA",
            "Kiawah, SC",
            "Myrtle Beach, SC")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Greenville, SC")
start
end <- which(labels(distance_matrix) == "Myrtle Beach, SC")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Greenville, SC",labels(cut_tour(tour, start_end)), "Myrtle Beach, SC")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1

#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)



Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 4",
               x ="Long", y = "Lat")
mp
tour

#==================================================================================================================

#=========TOUR 5=========================================================================================================

cities <- c("Warren, MI",
            "Cadillac, MI",
            "Orlando, FL",
            "Concord, NC",
            "Madison, MS",
            "Fayetteville, NC",
            "St.Louis, MO",
            "Cleveland, OH",
            "Dayton, OH",
            "Kansas City, MO",
            "Bloomington, IL",
            "Tampa, FL",
            "Salt Lake City, UT",
            "Tucson, AZ",
            "Janesville, WI",
            "Appleton, WI",
            "Milwaukee, WI",
            "Rothschild, WI",
            "Rochester, NY",
            "Albany, NY",
            "Pittsburgh, PA",
            "Hartford, CT",
            "Syracuse, NY",
            "Buffalo, NY",
            "Plattsburgh, NY",
            "Harrisburg, PA",
            "West Chester, PA",
            "Indianapolis, IN",
            "Fort Wayne, IN",
            "Carmel, IN",
            "Eugene, OR",
            "Omaha, NE",
            "Spokane, WA",
            "Fresno, CA",
            "San Diego, CA",
            "Sacramento, CA",
            "San Jose, CA")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix) # Using the preloaded stats package compute Euclidean distance


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Fayetteville, NC")
start
end <- which(labels(distance_matrix) == "Cleveland, OH")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Fayetteville, NC",labels(cut_tour(tour, start_end)), "Cleveland, OH")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1


Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 5",
               x ="Long", y = "Lat")
mp
tour
#======================= TOUR 6 ============================================================================================
cities <- c("Tampa, FL",
            "Atlanta, GA",
            "Dallas, TX",
            "Las Vegas, NV",
            "Phoenix, AZ",
            "Cincinnati, OH",
            "Cleveland, OH",
            "Toledo, OH",
            "Scranton, PA",
            "Colorado Springs, CO",
            "Denver, CO",
            "Daytona Beach, FL",
            "Jacksonville, FL",
            "Portland, ME",
            "Green Bay, WI",
            "Windham, NH",
            "Fayetteville, NC")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix) # Using the preloaded stats package compute Euclidean distance


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Cleveland, OH")
start
end <- which(labels(distance_matrix) == "Denver, CO")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Cleveland, OH",labels(cut_tour(tour, start_end)), "Denver, CO")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1

#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)

Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 6",
               x ="Long", y = "Lat")
mp
tour


#===================================================================================================================

#======================= TOUR 7 ============================================================================================
cities <- c("Manchester, NH",
            "Miami, FL",
            "Toledo, OH",
            "Naples, FL",
            "Tallahassee, FL",
            "Orlando, FL",
            "Concord, NC",
            "Hershey, PA",
            "Sarasota,FL",
            "Grand Rapids, MI",
            "Denver, CO",
            "Minneapolis, MN",
            "Las Vegas, NV")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix) # Using the preloaded stats package compute Euclidean distance


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) == "Denver, CO")
start
end <- which(labels(distance_matrix) == "Las Vegas, NV")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Denver, CO",labels(cut_tour(tour, start_end)), "Las Vegas, NV")
path_stuff1 <- as.matrix(path_stuff)
row.names(path_stuff1) <- path_stuff1

#path_ids <- match(path_labels, labels(distance_matrix))
#head(path_labels)

Cities_sort_accordingtotour <- as.matrix(path_stuff1) # Cities by Order of Path
longlat_accordingtotour <- geocode(unique(Cities_sort_accordingtotour))
row.names(longlat_accordingtotour) <- path_stuff1
mp <- NULL
mapUSA <- borders("usa", colour="gray50", fill="gray80") # create a layer of borders
mp <- ggplot() +   mapUSA
mp <- mp + geom_point(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat) ,color="red", size=3)
mp <- mp + geom_text(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat,label = row.names(longlat_accordingtotour)),position=position_jitter(width=1,height=1))
mp <- mp + geom_path(aes(x=longlat_accordingtotour$lon, y=longlat_accordingtotour$lat), color="black", size=1)
mp <- mp +labs(title="Plot of the Tour 7 - Final Days of Election",
               x ="Long", y = "Lat")
mp
tour
#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================


