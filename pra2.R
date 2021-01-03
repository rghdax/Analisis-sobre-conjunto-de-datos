library(stringr)
library(ggplot2)
library(knitr)
library(dplyr)
library(ggpubr)

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

#---------------Limpieza Rafael-------------------
# Eliminando simbolo %
df.Tomatometer <- as.integer(str_remove_all(df$Tomatometer, "%"))
# Agregando la variable Tomatometer sin simbolo al dataset original
df$Tomatometer <- df.Tomatometer 
# Conteo de NA
summary(df$Tomatometer)
# Eliminando NA de Tomatometer
df <- df[complete.cases(df$Tomatometer), ]
# Eliminando simbolo % 
df.Audience.score <- as.integer(str_remove_all(df$Audience.score, "%"))
# Agregando la variable Audience Score sin símbolo al dataset original
df$Audience.score <- df.Audience.score
# conteo de NA 
summary(df$Audience.score)
# La variable Fresh será igual a 1 cuando el Tomatometer sea >60 y 0 en caso
# contrario
df.fresh <- df$Tomatometer
df.fresh[df.fresh < 60] <- 0
df.fresh[df.fresh >= 60] <- 1
df$Fresh <- df.fresh
# Eliminando NA's de variable Audience Score. 
df <- df[complete.cases(df$Audience.score), ] 
df.Rating <- str_remove_all(df$Rating, "\\([a-zA-Z \\.,|/&-]*\\)") 
# Agregando la variable Rating al dataframe original
df$Rating <- df.Rating
# Imputando el valor G en los valores vacios de la variable Rating
df$Rating <- na_if(df$Rating, "")
df$Rating[is.na(df$Rating)] <- "G"
# Determinando cuales categorias hay
levels(factor(df$Rating))
p.control <- str_replace_all(df$Rating, "^G", "1")
p.control <- str_replace_all(p.control,"NC-17|PG-13|PG|R|TV14|TVG|TVMA|TVPG","0")
p.control <- as.integer(p.control)
df["Parental.Control"] <- p.control
df.Gen <- df$Genre
# Generación variable Gen1
df.Gen1 <- str_extract(df.Gen, "[a-zA-Z]*")
#df.Gen1
# Generación variable Gen2
df.Gen2 <- str_extract(df.Gen,",[a-zA-Z]*" )
df.Gen2 <- str_remove_all(df.Gen2, ",")
#df.Gen2
# Generación variable Gen3
df.Gen3 <- str_extract(df.Gen, "([a-zA-Z]*,){2}[a-zA-Z]*")
df.Gen3 <- str_extract(df.Gen3, ",[a-zA-Z]*$")
df.Gen3<- str_remove_all(df.Gen3, ",")
#df.Gen3
# Eliminando variable Genre y agregando variables nuevas Gen1, Gen2 y Gen3
# al dataset original
df <- subset(df, select = -c(Genre))
df["Gen1"] <- df.Gen1
df["Gen2"] <- df.Gen2
df["Gen3"] <- df.Gen3
# Reordenando dataframe
df <- df[c("Title","Tomatometer","Fresh","Audience.score","Rating","Parental.Control",
           "Gen1","Gen2","Gen3","Director","Producer","Writer",
           "Release.Date..Theaters.", "Release.Date..Streaming.","Runtime",
           "Production.Co")]
df



#Runtime
horas <- as.integer(substr(df$Runtime, 1, 1))
df$Runtime
minutos <- as.integer(substr(df$Runtime, 3, 4)) 
minutos[is.na(minutos)] <- as.integer(str_sub(df$Runtime[is.na(minutos)], -2, -2)) 
minutos[is.na(minutos)] <- 0
runtime <- 60*horas+minutos
df$Runtime <- as.integer(runtime)
summary(df$Runtime)
minutos
#-------------Fechas------------
colnames(df)[13] <- "Release.Date.Theaters"
colnames(df)[14] <- "Release.Date.Streamings"
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
release_type[str_sub(release_type, -4, -1)=="wide"] <- 1
release_type[str_sub(release_type, -7, -1)=="limited"] <- 0
df$Release.isWide <- as.integer(release_type)

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

df$Release.YearDay <- NULL
df$Release.Date.Streamings <- NULL
df$Release.Date.Theaters <- NULL

head(summary(sort(as.factor(df$Director))),24)
df$Director[df$Director=="" | df$Director=="UnknownDirector"] <- NA
head(summary(sort(as.factor(df$Writer))),24)
head(summary(sort(as.factor(df$Producer))),24)
head(summary(sort(as.factor(df$Production.Co))),24)
(summary(sort(as.factor(df$Release.Year))))

df$DirectorIsWriter<- (df$Director == df$Writer)
df$DirectorIsWriter[df$DirectorIsWriter] <- 1
df$DirectorIsWriter[!df$DirectorIsWriter] <- 0

df$Production.Co[is.na(df$Production.Co)] <- df$Producer[is.na(df$Production.Co)]
colnames(df)[14] <- "Production"
df$Writer <- NULL
df$Producer <- NULL

str(df)

#Representaciones
ggplot(df, aes(Runtime)) + geom_boxplot() 
ggplot(df, aes(Seasons)) + geom_bar() 
ggplot(df, aes(Release.Type)) + geom_bar() 
ggplot(df, aes(Release.YearDay)) + geom_bar() 
head(summary(sort(as.factor(df$Release.YearDay))),24)
ggplot(df, aes(DirectorIsWriter)) + geom_bar() 
ggplot(data=subset(df,!is.na(Release.Year)), aes(Release.Year)) + geom_bar()+ coord_flip()

#Representacion de relaciones
ggplot(df, aes(x=Runtime, y=Audience.score)) + geom_point() 
ggplot(df, aes(x=Runtime, y=Tomatometer)) + geom_point() 
ggplot(aes(y = Audience.score, x = Seasons), data = df) + geom_boxplot() + ggtitle("Audience score by season")
ggplot(aes(y = Tomatometer, x = Seasons), data = df) + geom_boxplot() + ggtitle("Tomatometer by season")
ggplot(aes(y = Audience.score, x = DirectorIsWriter), data = df) + geom_boxplot() + ggtitle("Director is writer")
ggplot(aes(y = Tomatometer, x = DirectorIsWriter), data = df) + geom_boxplot() + ggtitle("Director is writer")
ggplot(aes(y = Audience.score, x = Release.Year), data = subset(df,Release.Year>2005)) + geom_boxplot() + ggtitle("Audience score by year") + coord_flip()
ggplot(aes(y = Tomatometer, x = Release.Year), data = subset(df,Release.Year>2005)) + geom_boxplot() + ggtitle("Audience score by year") + coord_flip()
ggplot(aes(y = Audience.score, x = as.factor(Parental.Control)), data = df) + geom_boxplot() + ggtitle("Director is writer")
ggplot(aes(y = Tomatometer, x = as.factor(Parental.Control)), data = df) + geom_boxplot() + ggtitle("Director is writer")

