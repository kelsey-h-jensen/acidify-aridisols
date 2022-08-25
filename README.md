# acidify-aridisols
Data and code for study on acidification methods of Mojave FACE soils


data_calculations.R 
- converting %C into g C m^2 
- Bulk density was averaged across all rings at NDFF for the SOC stock calcs. Email kelsey.jensen.soil@gmail.com with questions about this decision

fulldata_stats.R 
- Linear models for cover type (plant), acid method (method), CO2 treatment (treatment) effects on SOC stocks and 13C 
- Estimated marginal means generated using emmeans package

emmeans_figures.R
- Plots created from estimated marginal means of SOC stocks and 13C 

correlation_figures.R
- correlation plots created from raw data
- stats taken from the linear models fit in "fulldata_stats.R"