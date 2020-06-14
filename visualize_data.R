
#Load libraries
library(dplyr)
library(ggplot2)
install.packages(ggrepel)
library(ggrepel)

#Load data with gini index information 
data_results <- read.csv("result_data/gini_index_table.csv")

## Graphs for countries with available data_results every year
#Select countries with data_results available every year
ocurrences <- count(data_results, País.de.residencia)
list_uncomplete_countries <- ocurrences[ocurrences$n<19,]
data_complete_countries <- data_results[-which(data_results$País.de.residencia %in% list_uncomplete_countries$País.de.residencia),]

#Gini evolution for complete countries - coloured by effect on seasonality
Graph_complete_gini <- data_complete_countries %>%
  ggplot (aes(x=Year, y=Gini)) + geom_bar(aes(color=Effect_on_seasonality, fill=Effect_on_seasonality), stat = "identity") + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + facet_wrap(~País.de.residencia) + theme_minimal() + xlab("") + labs(fill = "", color = "") + theme(legend.position = "bottom")

#RME in each year for complete countries - coloured by effect on seasonality
Grap_complete_rme <- data_complete_countries %>%
  ggplot (aes(x=Year, y=RME)) + geom_bar(aes(color=Effect_on_seasonality, fill=Effect_on_seasonality), stat = "identity") + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + facet_wrap(~País.de.residencia) + theme_minimal() + xlab("") + labs(fill = "", color = "") + theme(legend.position = "bottom")

#Volume of tourist evolution for complete countries
Graph_complete_total <- data_complete_countries %>%
  ggplot (aes(x=Year, y=Anual_pais)) + geom_line() + facet_wrap(~País.de.residencia) + theme_minimal() + xlab("") + ylab("Total tourists per year") 


##Relation between Gini and volume of tourists - all countries (selection of years)

#2018
Graph_relation_2018 <- data_results %>% filter(Year == 2018) %>%   
  ggplot(aes(x=Anual_pais, y=Gini)) + geom_point(aes(fill=Effect_on_seasonality, color=Effect_on_seasonality), size=3, shape=23) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + geom_text_repel (aes(label=País.de.residencia, color=Effect_on_seasonality), size=3) + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + xlab("Total tourists per year") + ggtitle("2018")+ theme_minimal() + theme(legend.position = c(0.75, 0.5), plot.title=element_text( hjust=1, vjust=0.5))
#2012
Graph_relation_2012 <- data_results %>% filter(Year == 2012) %>%   
  ggplot(aes(x=Anual_pais, y=Gini)) + geom_point(aes(fill=Effect_on_seasonality, color=Effect_on_seasonality), size=3, shape=23) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + geom_text_repel (aes(label=País.de.residencia, color=Effect_on_seasonality), size=3) + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + xlab("Total tourists per year") + ggtitle("2012")+ theme_minimal() + theme(legend.position = c(0.75, 0.5), plot.title=element_text( hjust=1, vjust=0.5))
#2006
Graph_relation_2006 <- data_results %>% filter(Year == 2006) %>%   
  ggplot(aes(x=Anual_pais, y=Gini)) + geom_point(aes(fill=Effect_on_seasonality, color=Effect_on_seasonality), size=3, shape=23) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + geom_text_repel (aes(label=País.de.residencia, color=Effect_on_seasonality), size=3) + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + xlab("Total tourists per year") + ggtitle("2006")+ theme_minimal() + theme(legend.position = c(0.75, 0.5), plot.title=element_text( hjust=1, vjust=0.5))
#2000
Graph_relation_2000 <- data_results %>% filter(Year == 2000) %>%   
  ggplot(aes(x=Anual_pais, y=Gini)) + geom_point(aes(fill=Effect_on_seasonality, color=Effect_on_seasonality), size=3, shape=23) + scale_fill_manual(values=c("forestgreen", "steelblue1", "red")) + geom_text_repel (aes(label=País.de.residencia, color=Effect_on_seasonality), size=3) + scale_color_manual(values=c("forestgreen", "steelblue1", "red")) + xlab("Total tourists per year") + ggtitle("2000")+ theme_minimal() + theme(legend.position = c(0.75, 0.5), plot.title=element_text( hjust=1, vjust=0.5))


## Boxplots to measure evolution of dispersion of countries' Gini indexes and Gini correlations
#define function to identfy outliers in boxplots (so they can be named)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#Boxplot - Gini
data_boxplot1 <- data_results %>% group_by(Year) %>% mutate(is_outlier=ifelse(is_outlier(Gini), Gini, as.numeric(NA)))
data_boxplot1$País.de.residencia[which(is.na(data_boxplot1$is_outlier))] <- as.numeric(NA)
Graph_boxplot_gini <- data_boxplot1 %>% ggplot()+geom_boxplot(aes(x=Year, y=Gini, group=Year), fill="slateblue", colour="slateblue4", notch=TRUE, alpha=0.3) + scale_x_continuous(breaks=c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) + geom_text_repel(aes(x=Year, y=Gini, label=País.de.residencia), na.rm=TRUE,nudge_y=0.05)+ theme_minimal() + xlab("Year") + theme(plot.title=element_text( hjust=1, vjust=0.5))

#Boxplot - Gini correlation
data_boxplot2 <- data_results %>% group_by(Year) %>% mutate(is_outlier=ifelse(is_outlier(Correlation), Correlation, as.numeric(NA)))
data_boxplot2$País.de.residencia[which(is.na(data_boxplot2$is_outlier))] <- as.numeric(NA)
Graph_boxplot_correlation <- data_boxplot2 %>% ggplot()+geom_boxplot(aes(x=Year, y=Correlation, group=Year), fill="firebrick", colour="firebrick4", notch=TRUE, alpha=0.3) + scale_x_continuous(breaks=c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) + geom_text_repel(aes(x=Year, y=Correlation, label=País.de.residencia),na.rm=TRUE,nudge_y=0.05)+ theme_minimal() + xlab("Year") + theme(plot.title=element_text( hjust=1, vjust=0.5))


## Evolution of volume of tourists - selected countries (those with over 15.000 in 2018)
selected_list <- data_complete_countries %>% filter (Year==2018) %>% filter(Anual_pais > 15000)
selected_data <- data_complete_countries %>% filter(País.de.residencia %in% selected_list$País.de.residencia)
Graph_evolution <- selected_data %>% 
  group_by (Year,País.de.residencia) %>% 
  ggplot(aes (x=Year, y=Anual_pais, group=País.de.residencia, color=País.de.residencia)) + geom_line() + geom_text_repel(dat=subset(selected_data, Year == "2017"), aes(label=País.de.residencia)) + scale_x_continuous(breaks=c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) + theme_minimal() + xlab("Year") + ylab("Total tourists") + theme(legend.position = "none")