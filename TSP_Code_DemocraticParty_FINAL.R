cat("\014") # clear screen
rm(list=ls()) # clear global environment

#Packages you must install befor you install the github ggmap
install.packages("TSP")
install.packages("tspmeta")
install.packages("lazyeval")
install.packages("ggmap")
install.packages("devtools")
library(TSP) # For Traveling Salesman
library(tspmeta) # For Traveling Salesman
library(ggmap) # For location of cities from Google
library(ggplot2)

##https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode
#Run after the above has been installed
#AIzaSyAaFaAgCJcDDTZx05_ZZbBRP-q21C0nzFM - google key
devtools::install_github("dkahle/ggmap")
register_google(key = "AIzaSyBJ1Al0zzqDu7xvXdQr9p6YyGVmhK7MPcE")


#============STARTING POINT========================================================================
#========================================================================================================================
#========================================================================================================================

#==================Tour 1 ================================================================================================

cities <- c("Washington D.C." , "Baltimore, MD",   "Virginia Beach, VA" ,    "Norfolk, VA" , "Chesapeake, VA" , "Arlington, VA" ,"Richmond, VA" ,"Cedar Rapids, IA" ,"Des Moines, IA" ,"St.Paul, MN" , "Madison, WI" ,"Chicago, IL" ,"Columbus, OH" , "Manchester, NH" ,"Nashua, NH" , "Boston, MA" ,"Las Vegas, NV" ,"North Charleston, SC" ,"Columbia, SC")

lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) =="Washington D.C.")
start
end <- which(labels(distance_matrix) == "Columbia, SC")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Washington D.C.",labels(cut_tour(tour, start_end)), "Columbia, SC")
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
mp <- mp +labs(title="Plot of the Tour 1",
               x ="Long", y = "Lat")
mp
tour

#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================



#==================Tour 2 ================================================================================================

cities <- c("Columbia, SC", "Greenville, SC", "Valdosta, GA","Minneapolis, MN","Kansas City, KS","Shreveport, LA","Bangor, ME" ,"Jackson, MS" ,"Denver, CO" , "Colorado Springs, CO" ,"Aurora, CO" ,"Fort Collins, CO" ,
            "Gulfport, MS" ,
            "Detroit, MI" ,
            "Grand Rapids, MI" ,
            "Virginia Beach, VA" ,
            "Phoenix, AZ" ,
            "Houston, TX" ,
            "Dallas, TX",
            "Salt Lake City, UT" ,
            "West Valley, UT" ,
            "Anchorage, AK" ,
            "Seattle, WA" ,
            "Honolulu, HI",
            "Pearl City, HI")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) =="Columbia, SC")
start
end <- which(labels(distance_matrix) == "Pearl City, HI")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Columbia, SC",labels(cut_tour(tour, start_end)), "Pearl City, HI")
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

#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================

#==================Tour 3 ================================================================================================

cities <- c("Pearl City, HI" ,"Milwaukee, WI" , "Columbus, OH" ,"Cleveland, OH" ,
            "Boston, MA" ,
            "New York, NY" ,
            "Philadelphia, PA" ,
            "Pittsburgh, PA" ,
            "Allentown, PA" ,
            "Erie, PA" , 
            "Reading, PA" ,
            "Providence, RI" ,
            "Indianapolis, IN" ,
            "Toledo, OH" ,
            "Akron, OH" ,
            "Charleston, WV" ,
            "Portland, OR" , 
            "Sioux Falls, SD")
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) =="Pearl City, HI")
start
end <- which(labels(distance_matrix) == "Sioux Falls, SD")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Pearl City, HI",labels(cut_tour(tour, start_end)),"Sioux Falls, SD")
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

#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================

#==================Tour 4 ================================================================================================

cities <- c("Sioux Falls, SD",
            "Baltimore, MD",
            "Arlington, VA" ,
            "Albuquerque, NM" ,
            "Washington, D.C." , 
            "Los Angeles, CA" ,
            "San Francisco, CA" ,
            "Columbus, OH" ,
            "Cleveland, OH",
            "Toledo, OH",
            "Akron, OH" ,
            "Pittsburgh, PA" ,
            "Allentown, PA" ,
            "Philadelphia, PA")
            
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) =="Sioux Falls, SD")
start
end <- which(labels(distance_matrix) == "Philadelphia, PA")
end
atsp <- ATSP(m[-c(start,end), -c(start,end)])
atsp <- insert_dummy(atsp, label = "start/end")
start_end <- which(labels(atsp) == "start/end")
atsp[start_end, ] <- c(m[-c(start,end), start], 0)
atsp[, start_end] <- c(m[end, -c(start,end)], 0)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
path_stuff <- c("Sioux Falls, SD",labels(cut_tour(tour, start_end)),"Philadelphia, PA")
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

#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================

#==================Tour 5 ================================================================================================

cities <- c("Philadelphia, PA",
            "Baltimore, MD" ,
            "Norfolk, VA",
            "Denver, CO",
            "Colorado Spring, CO ",
            "Aurora, CO",
            "Concord, CO",
            "Virginia Beach, VA" ,
            "Chesapeake, VA",
            "Orlando, FL",
            "Farmville, VA",
            "Warren, MI" ,
            "Sterling Heights, MI" ,
            "Ann Arbor, MI" ,
            "Hempstead, NY" ,
            "Las Vegas, NV")
            
lonlat <- geocode(unique(cities)) # using ggmap (you may want to put a pause function and a for loop here)
longlat_matrix <- as.matrix(lonlat) # converting the data frame to a matrix
row.names(longlat_matrix) <- cities
longlat_matrix
distance_matrix <- dist(longlat_matrix)


m <- as.matrix(distance_matrix)
labels(distance_matrix)
start <- which(labels(distance_matrix) =="Philadelphia, PA")
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
path_stuff <- c("Philadelphia, PA",labels(cut_tour(tour, start_end)),"Las Vegas, NV")
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
mp <- mp +labs(title="Plot of the Tour 5",
               x ="Long", y = "Lat")
mp

tour

#============ENDING POINT CODE========================================================================
#========================================================================================================================
#========================================================================================================================




