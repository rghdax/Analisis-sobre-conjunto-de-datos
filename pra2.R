library(stringr)
library(ggplot2)
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

#-------------Fechas------------
colnames(df)[9] <- "Release.Date.Theaters"
colnames(df)[10] <- "Release.Date.Streamings"
dim(df[df$Release.Date.Streamings=="",])
dim(df[df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & !df$Release.Date.Theaters=="",])
dim(df[!df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])
dim(df[df$Release.Date.Streamings=="" & df$Release.Date.Theaters=="",])
head(df$Release.Date.Streamings)

##-------------Limpieza Streaming------------
#Separacion fecha
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec" )
streamings.year <-  as.integer(str_sub(df$Release.Date.Streamings, -4, -1)) 
streamings.month <-  substr(df$Release.Date.Streamings, 1, 3)
streamings.day <-  as.integer(gsub(",","",substr(df$Release.Date.Streamings, 4, 5))) 
streamings.month <- match(streamings.month,months)
streamings.date <- paste(str_sub(streamings.year, -2, -1),streamings.month,streamings.day,sep="/")
streamings.date <- as.Date(streamings.date,format="%D")

#----------------Theaters----------------
#Wide o limited
theaters <- df$Release.Date.Theaters
release_type <- df$Release.Date.Theaters
release_type[str_sub(release_type, -4, -1)=="wide"] <- "wide"
release_type[str_sub(release_type, -7, -1)=="limited"] <- "limited"
df$Release.Type <- release_type

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
theaters.date <- paste(str_sub(theaters.year, -2, -1),theaters.month,theaters.day,sep="/")
theaters.date <- as.Date(theaters.date,format="%D")

#Seleccionar primera fecha
na_date <-  as.Date("68/12/31",format="%D")
theaters.date[is.na(theaters.date)] <- na_date
streamings.date[is.na(streamings.date)] <- na_date
df$Release.Date <- pmin(streamings.date, theaters.date)
df$Release.Date[df$Release.Date==na_date] <- NA
df$Release.YearDay <- as.integer(strftime(df$Release.Date, format = "%j"))
df$Release.Year <- strftime(df$Release.Date, format = "%Y")
#Estación
spring <-  as.integer(strftime("2020-03-21", format = "%j"))
summer <-  as.integer(strftime("2020-06-22", format = "%j"))
autumn <-  as.integer(strftime("2020-09-23", format = "%j"))
winter <-  as.integer(strftime("2020-12-22", format = "%j"))
df$Seasons[!is.na(df$Release.Year)] <- "winter"
df$Seasons[df$Release.YearDay>spring] <- "spring" 
df$Seasons[df$Release.YearDay>summer] <- "summer" 
df$Seasons[df$Release.YearDay>autumn] <- "autumn" 
df$Seasons[df$Release.YearDay>winter] <- "winter" 

df$Release.Date.Streamings <- NULL
df$Release.Date.Theaters <- NULL


head(summary(sort(as.factor(df$Director))),24)
df$Director[df$Director=="" | df$Director=="UnknownDirector"] <- NA
head(summary(sort(as.factor(df$Writer))),24)
head(summary(sort(as.factor(df$Producer))),24)
head(summary(sort(as.factor(df$Production.Co))),24)

df$DirectorIsWriter<- (df$Director == df$Writer)
df$Production.Co[is.na(df$Production.Co)] <- df$Producer[is.na(df$Production.Co)]
colnames(df)[10] <- "Production"
df$Writer <- NULL
str(df)

ggplot(df, aes(Runtime)) + geom_boxplot() 
ggplot(df, aes(Seasons)) + geom_bar() 
ggplot(df, aes(Release.Type)) + geom_bar() 
ggplot(df, aes(Release.YearDay)) + geom_bar() 
head(summary(sort(as.factor(df$Release.YearDay))),24)
ggplot(df, aes(DirectorIsWriter)) + geom_bar() 
ggplot(data=subset(df,!is.na(Release.Year)), aes(Release.Year)) + geom_bar()+ coord_flip()


summary(sort(as.factor(df$Release.Year)))
