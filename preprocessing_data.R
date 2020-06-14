#load libraries
library(dplyr)
library(tidyverse)
#Load data
Y2018 <- read.csv2 ("original_data/data_2018.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2018$Total <- str_remove_all(Y2018$Total, pattern = "[.]") %>% 
  as.integer(Y2018$Total)
Y2017 <- read.csv2 ("original_data/data_2017.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2017$Total <- str_remove_all(Y2017$Total, pattern = "[.]") %>% 
  as.integer(Y2017$Total)
Y2016 <- read.csv2 ("original_data/data_2016.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2016$Total <- str_remove_all(Y2016$Total, pattern = "[.]") %>% 
  as.integer(Y2016$Total)
Y2015 <- read.csv2 ("original_data/data_2015.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2015$Total <- str_remove_all(Y2015$Total, pattern = "[.]") %>% 
  as.integer(Y2015$Total)
Y2014 <- read.csv2 ("original_data/data_2014.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2014$Total <- str_remove_all(Y2014$Total, pattern = "[.]") %>% 
  as.integer(Y2014$Total)
Y2013 <- read.csv2 ("original_data/data_2013.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2013$Total <- str_remove_all(Y2013$Total, pattern = "[.]") %>% 
  as.integer(Y2013$Total)
Y2012 <- read.csv2 ("original_data/data_2012.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2012$Total <- str_remove_all(Y2012$Total, pattern = "[.]") %>% 
  as.integer(Y2012$Total)
Y2011 <- read.csv2 ("original_data/data_2011.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2011$Total <- str_remove_all(Y2011$Total, pattern = "[.]") %>% 
  as.integer(Y2011$Total)
Y2010 <- read.csv2 ("original_data/data_2010.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2010$Total <- str_remove_all(Y2010$Total, pattern = "[.]") %>% 
  as.integer(Y2010$Total)
Y2009 <- read.csv2 ("original_data/data_2009.csv", colClasses=c("Comunidades.y.ciudades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2009$Total <- str_remove_all(Y2009$Total, pattern = "[.]") %>% 
  as.integer(Y2009$Total)
Y2008 <- read.csv2 ("original_data/data_2008.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2008$Total <- str_remove_all(Y2008$Total, pattern = "[.]") %>% 
  as.integer(Y2008$Total)
Y2007 <- read.csv2 ("original_data/data_2007.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2007$Total <- str_remove_all(Y2007$Total, pattern = "[.]") %>% 
  as.integer(Y2007$Total)
Y2006 <- read.csv2 ("original_data/data_2006.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2006$Total <- str_remove_all(Y2006$Total, pattern = "[.]") %>% 
  as.integer(Y2006$Total)
Y2005 <- read.csv2 ("original_data/data_2005.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2005$Total <- str_remove_all(Y2005$Total, pattern = "[.]") %>% 
  as.integer(Y2005$Total)
Y2004 <- read.csv2 ("original_data/data_2004.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2004$Total <- str_remove_all(Y2004$Total, pattern = "[.]") %>% 
  as.integer(Y2004$Total)
Y2003 <- read.csv2 ("original_data/data_2003.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2003$Total <- str_remove_all(Y2003$Total, pattern = "[.]") %>% 
  as.integer(Y2003$Total)
Y2002 <- read.csv2 ("original_data/data_2002.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2002$Total <- str_remove_all(Y2002$Total, pattern = "[.]") %>% 
  as.integer(Y2002$Total) 
Y2001 <- read.csv2 ("original_data/data_2001.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2001$Total <- str_remove_all(Y2001$Total, pattern = "[.]") %>% 
  as.integer(Y2001$Total) 
Y2000 <- read.csv2 ("original_data/data_2000.csv", colClasses=c("Comunidades.autónomas"="factor", "País.de.residencia"="factor", "Meses"="factor", "Total"="character"))
Y2000$Total <- str_remove_all(Y2000$Total, pattern = "[.]") %>% 
  as.integer(Y2000$Total)

#Add year in each dataset
List_Y = mget(ls(pattern = "Y[2000:2018]"))
year <- c(2000:2018)
for(y in seq_along(List_Y)) {List_Y[[y]]$Year <-  rep(year[y], nrow(List_Y[[y]]))}

#Merge data splitted by year into just one dataset
all_years_data = bind_rows(List_Y)

#Drop data from non-country categories
aggregates <- c("Españoles", "Residentes en España", "Total", "No residentes en España", "Extranjeros", "Unión Europea (sin España)",  "Unión Europea(sin España)")
all_years_data <- all_years_data[-which(all_years_data$País.de.residencia %in% aggregates),]

#Drop unnecessary columns
all_years_data$Comunidades.autónomas <- NULL
all_years_data$Comunidades.y.ciudades.autónomas <- NULL

#Replace NAs with zeros in column Total
is_missing_in_total <- is.na(all_years_data$Total)
all_years_data$Total[is_missing_in_total] <- 0

#Replace spanish months names with numeric 
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Enero", replacement=1)
all_years_data$Meses <- all_years_data$Meses  %>% gsub(pattern="Febrero", replacement=2)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Marzo", replacement=3)
all_years_data$Meses <- all_years_data$Meses  %>% gsub(pattern="Abril", replacement=4)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Mayo", replacement=5)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Junio", replacement=6)
all_years_data$Meses <- all_years_data$Meses  %>% gsub(pattern="Julio", replacement=7)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Agosto", replacement=8)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Setiembre", replacement=9)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Septiembre", replacement=9)
all_years_data$Meses <- all_years_data$Meses  %>% gsub(pattern="Octubre", replacement=10)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Noviembre", replacement=11)
all_years_data$Meses <- all_years_data$Meses %>% gsub(pattern="Diciembre", replacement=12)
all_years_data$Meses <- as.numeric(all_years_data$Meses)

#Order by country, year and month
all_years_data <- all_years_data [with(all_years_data, order(all_years_data$País.de.residencia, all_years_data$Year, all_years_data$Meses)),]

#Export data
write.csv(all_years_data,"preprocessed_data/data.csv", row.names = FALSE)