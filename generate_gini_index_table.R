#load libraries
library(dplyr)
library(ineq)
library(stats)

#load preprocessed data
data <- read.csv("preprocessed_data/data.csv")

#Tables with aggregated data (for destination as a whole) needed as inputs to decompose Gini index
data_aggregated_by_year <- data %>% group_by(Year) %>% summarize(Extranjero = sum(Total))
data_aggregated_by_month <- data %>% group_by(Year, Meses) %>% summarize (Total_mensual = sum(Total))
data_aggregated_by_month <- data_aggregated_by_month [with(data_aggregated_by_month, order(data_aggregated_by_month$Year, data_aggregated_by_month$Meses)),]
Gini_anual <- data_aggregated_by_month %>% group_by(Year) %>% summarize (Gini = ineq(Total_mensual, Gini))
data_aggregated_by_year <- mutate(data_aggregated_by_year, Gini=Gini_anual$Gini)

"CALCULATE GINI INDEX TABLE: 
This table will include data for each country on the three parameters (Gini index, market share and Gini correlation)
needed to calculate each countrys contribution to the overall seasonality of the destination and then calculate their
Relative Marginal Effect (RME)"

#Calculate Gini index for each coutnry
gini_index_table <- data %>% group_by (País.de.residencia, Year) %>% summarize (Anual_pais = sum(Total), Gini = ineq(Total,Gini))

#Calculate country's share of total tourism
    #define appropriate function
calculate_share <- function(country_vector, year_vector) {
  year <- year_vector[1]
  total_year <- data_aggregated_by_year %>% filter(Year == year)
  num_countries_year <- gini_index_table %>% filter (Year == year) %>% nrow() 
  total_year_vector <- rep(total_year$Extranjero, n=num_countries_year)
  result <- country_vector/total_year_vector
  return(result)
}
    #calculate share for each conutry   
share_aux_table <- gini_index_table %>% group_by (País.de.residencia, Year) %>% summarise(Share = calculate_share(Anual_pais, Year))         


#Calculate country's Gini correlation

  #define function to calculate cummulative frequency distrbution - there are pre-defined functions in other libraries, but for this exercise it is crucial all months have a different cummulative frequency (according to natural order), albeit their values might be the same
cum_freq <- function(data_country_vector) {
  ranking_vector <- rank(data_country_vector, ties.method = "first")
  result <- ranking_vector /length(data_country_vector)
  return(result)
}

  # define correlation gini formula (3 functions)
calculate_denominator <- function(data_country_vector) {
  cummulative_freq_country <- cum_freq(data_country_vector)
  result <- cov(data_country_vector, cummulative_freq_country)
  return(result)
}

calculate_numerator <- function(data_country_vector, monthly_agg_data) {
  cum_freq_agg <- cum_freq(monthly_agg_data)
  result <- cov(data_country_vector, cum_freq_agg)
  return(result)
}

calculate_gini_corr <- function(data_country_vector, year_vector) {
  year <- year_vector[1]
  montly_data_agg_by_year <- data_aggregated_by_month %>% filter(Year == year)
  total_montly_tourist <- montly_data_agg_by_year$Total_mensual
  numerator <- calculate_numerator(data_country_vector, total_montly_tourist)
  denominator <- calculate_denominator(data_country_vector)
  result <- numerator/denominator
  return(result)
}

  #calculate gini correlation for each country and year
gini_corr_aux_table <- data %>% group_by(País.de.residencia, Year) %>% summarise(Correlation = calculate_gini_corr(Total, Year))

#Add parameters "Share" and "Correlation" to Table
gini_index_table$Correlation <- gini_corr_aux_table$Correlation

gini_index_table$Share <- share_aux_table$Share

#Calculate each country's contribution to overall seasonality in each year (result of product of its Gini index, its market share and its Gini correlation)
gini_index_table$Contribution <- gini_index_table$Gini * gini_index_table$Share * gini_index_table$Correlation

#Calculate each country's RME
  # define RME function
calculate_rme <- function(contribution_vector, share_vector, year_vector) {
  ano <- year_vector[1]
  year_data <- data_aggregated_by_year %>% filter(Year == ano)
  total_gini_vector <- year_data$Gini
  quotient <- contribution_vector / total_gini_vector 
  result <- quotient - share_vector
  return(result)
}
  #Calculate RME for each country/year and add to Table
gini_index_table$RME <- (gini_index_table %>% group_by(País.de.residencia, Year) %>% summarise(RME = calculate_rme(Contribution, Share, Year)))$RME

#Classify countries accoridng to their effect on overall seasonality each year
gini_index_table$Effect_on_seasonality <- as.factor(ifelse (gini_index_table$RME >= 0.01, "Unfavourable", ifelse (gini_index_table$RME <= -0.01, "Favourable", "Neutral")))

#Export table
write.csv(gini_index_table,"result_data/gini_index_table.csv", row.names = FALSE)