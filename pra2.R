df <- read.csv(file="https://raw.githubusercontent.com/dbagan13/WebScrapping/main/csv/recogiendo_tomates.csv",header=TRUE,sep=",")
str(df)
df$X <- NULL 
dim(df)
colSums(is.na(df))
colSums(df=="")
colSums(df=="NaN")

#Tratamiento inicial de nulos y cadenas vacías
df$Rating[df$Rating==""] <- NA
df$Genre[df$Genre==""] <- NA
df$Director[df$Director==""] <- NA
df$Producer[df$Producer==""] <- NA
df$Writer[df$Writer==""] <- NA
df$Production.Co[df$Production.Co==""] <- NA

#Runtime
horas <- as.integer(substr(df$Runtime, 1, 1))    
minutos <- as.integer(substr(df$Runtime, 3, 4)) 
minutos[is.na(minutos)] <- 0
runtime <- 60*horas+minutos
df$Runtime <- as.integer(runtime)
summary(df$Runtime)
#Fechas
colnames(df)[9] <- "Release.Date.Theaters"
colnames(df)[10] <- "Release.Date.Streamings"
dim(df[df$Release.Date.Streamings=="",])
dim(df[df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & !df$Release.Date.Theaters=="",])
dim(df[!df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])
head(df$Release.Date.Streamings)

library(stringr)
#-----------Streamings-------------
#Separacion fecha
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec" )
streamings.year <-  as.integer(str_sub(df$Release.Date.Streamings, -4, -1)) 
streamings.month <-  substr(df$Release.Date.Streamings, 1, 3)
streamings.day <-  as.integer(gsub(",","",substr(df$Release.Date.Streamings, 4, 5))) 
streamings.month <- match(streamings.month,months)
streamings.year_day <- streamings.day * streamings.month
streamings.year_day <- paste(str_sub(streamings.year, -2, -1),streamings.month,streamings.day,sep="/")
streamings.year_day <- as.Date(streamings.year_day,format="%D")
streamings.year_day <- as.integer(strftime(streamings.year_day, format = "%j"))
streamings <- data.frame(year=streamings.year,month=streamings.month,
                         day=streamings.day,year_day=streamings.year_day)
#Estación
spring <-  as.integer(strftime("2020-03-21", format = "%j"))
summer <-  as.integer(strftime("2020-06-22", format = "%j"))
autumn <-  as.integer(strftime("2020-09-23", format = "%j"))
winter <-  as.integer(strftime("2020-12-22", format = "%j"))
streamings$seasons[!is.na(streamings$year)] <- "winter"
streamings$seasons[streamings$year_day>spring] <- "spring" 
streamings$seasons[streamings$year_day>summer] <- "summer" 
streamings$seasons[streamings$year_day>autumn] <- "autumn" 
streamings$seasons[streamings$year_day>winter] <- "winter" 

head(streamings)
df$streamings.year <- streamings$year
df$streamings.month <- streamings$month
df$streamingss.day <- streamings$day
df$streamings.year_day <- streamings$year_day
df$streamings.seasons <- streamings$seasons
df$Release.Date.Streamings <- NULL

#----------------Theaters----------------
#Wide o limited
theaters <- df$Release.Date.Theaters
release_type <- df$Release.Date.Theaters
release_type[str_sub(release_type, -4, -1)=="wide"] <- "wide"
release_type[str_sub(release_type, -7, -1)=="limited"] <- "limited"

#Limpieza
theaters <- str_remove_all(theaters,"limited")
theaters <- str_remove_all(theaters,"wide")
theaters <- str_sub(theaters, 1, -3)
head(theaters)

#Separacion fecha
theaters.year <-  as.integer(str_sub(theaters, -4, -1)) 
theaters.month <-  substr(theaters, 1, 3)
theaters.day <-  as.integer(gsub(",","",substr(theaters, 4, 5))) 
theaters.month <- match(theaters.month,months)
theaters.year_day <- paste(str_sub(theaters.year, -2, -1),theaters.month,theaters.day,sep="/")
theaters.year_day <- as.Date(theaters.year_day,format="%D")
theaters.year_day <- as.integer(strftime(theaters.year_day, format = "%j"))
theaters <- data.frame(year=theaters.year,month=theaters.month,
                         day=theaters.day,year_day=theaters.year_day)
#Estación
theaters$seasons[!is.na(theaters$year)] <- "winter"
theaters$seasons[theaters$year_day>spring] <- "spring" 
theaters$seasons[theaters$year_day>summer] <- "summer" 
theaters$seasons[theaters$year_day>autumn] <- "autumn" 
theaters$seasons[theaters$year_day>winter] <- "winter" 

head(theaters)

df$theaters.year <- theaters$year
df$theaters.month <- theaters$month
df$theaters.day <- theaters$day
df$theaters.year_day <- theaters$year_day
df$theaters.seasons <- theaters$seasons
df$Release.Date.Theaters <- NULL

str(df)

head(summary(sort(as.factor(df$Director))),24)
df$Director[df$Director=="" | df$Director=="UnknownDirector"] <- NA
head(summary(sort(as.factor(df$Writer))),24)
head(summary(sort(as.factor(df$Producer))),24)
head(summary(sort(as.factor(df$Production.Co))),24)


