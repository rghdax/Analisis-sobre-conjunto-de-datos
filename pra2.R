df <- read.csv(file="https://raw.githubusercontent.com/dbagan13/WebScrapping/main/csv/recogiendo_tomates.csv",header=TRUE,sep=",")
str(df)
dim(df)
colSums(is.na(df))
colSums(df=="")
colSums(df=="NaN")


#Runtime
horas <- as.integer(substr(runtime, 1, 1))    
minutos <- as.integer(substr(runtime, 3, 4))    
runtime <- 60*horas+minutos
df$Runtime <- runtime
summary(df$Runtime)

#Fechas
colnames(df)[10] <- "Release.Date.Theaters"
colnames(df)[11] <- "Release.Date.Streamings"
dim(df[df$Release.Date.Streamings=="",])
dim(df[df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & !df$Release.Date.Theaters=="",])
dim(df[!df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])



library(stringr)
streamings.year <-  as.integer(str_sub(df$Release.Date.Streamings, -4, -1)) 
streamings.month <-  substr(df$Release.Date.Streamings, 1, 3)
streamings.day <-  as.integer(gsub(",","",substr(df$Release.Date.Streamings, 4, 5))) 
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec" )
streamings.month <- match(streamings.month,months)
streamings.year_day <- theaters.day * theaters.month

#Streamings 
streamings.year <-  as.integer(str_sub(df$Release.Date.Streamings, -4, -1)) 
streamings.month <-  substr(df$Release.Date.Streamings, 1, 3)
streamings.day <-  as.integer(gsub(",","",substr(df$Release.Date.Streamings, 4, 5))) 
streamings.month <- match(streamings.month,months)
streamings.year_day <- streamings.day * streamings.month

#Theaters
theaters <- df$Release.Date.Theaters
#Wide o limited
release_type <- theaters
release_type[str_sub(release_type, -4, -1)=="wide"] <- "wide"
release_type[str_sub(release_type, -7, -1)=="limited"] <- "limited"
#Limpieza
theaters <- str_remove_all(theaters,"limited")
theaters <- str_remove_all(theaters,"wide")
theaters <- str_sub(theaters, 1, -3)
#Theaters
theaters.year <-  as.integer(str_sub(theaters, -4, -1)) 
theaters.month <-  substr(theaters, 1, 3)
theaters.day <-  as.integer(gsub(",","",substr(theaters, 4, 5))) 
theaters.month <- match(theaters.month,months)
theaters.year_day <- theaters.day * theaters.month
