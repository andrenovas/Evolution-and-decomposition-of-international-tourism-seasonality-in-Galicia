# Evolution-and-decomposition-of-international-tourism-seasonality-in-Galicia

This analysis calculates seasonality of international tourism in Galicia between 2000-2018, and decompose it into different market segments, in order to 
a) determine the contribution of each country of origin to the overall seasonality of the destination and
b) analyse whether they’ve had a favourable or unfavourable effect on the overall seasonality of Galician tourism.

Dependencies:
- dplyr
- tidyverse
- ineq
- stats
- ggplot2
- ggrepel

Steps for execution. 
The analysys is organized in 3 scripts, which must be executed in the following order:
analysis is organized in 3 scripts, which must be executed in the following order:
  - preprocessing_data: it cleans and prepares data to carry out the analysis. It takes the original data for each year (saved in the       folder “original_data”) and it produces a unique preprocessed dataset (exported to folder “preprocessed_data”).
  - generate_gini_index_table: this script carries out the main analysis over the preprocessed data, generating a table with data for all     variables and parameters above (Gini index, market share, Gini correlation, contribution to seasonality, RME, effect on seasonality) for   each country in every year (2000-2018), which is exported to folder “result_data”
  - visualize_data: this script offers several example of different alternatives to visualize the results of the data analysis.
