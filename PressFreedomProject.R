#### CNTI Press Freedom Day Data Report ####


### From Varieties of Democracy's GitHub:
## First, you need to have the devtools package installed
install.packages("devtools")
## Install the vdemdata package directly from GitHub
devtools::install_github("vdeminstitute/vdemdata")


library(vdemdata)
vdem <- vdemdata::vdem


#codebook: https://www.v-dem.net/documents/38/V-Dem_Codebook_v14.pdf


## Select variables (columns) of interest
vdem <- vdem %>%
  select(country_name, country_text_id, country_id, year, v2mecenefm_osp, v2x_regime,
         v2mecenefi_osp, v2mefemjrn, v2meharjrn_osp, v2meslfcen_osp, v2x_freexp_altinf)


## Time period in 1993-2023
vdem_v2 <- subset(vdem, year >= 1993)


## Government censorship effort of the media (v2mecenefm_osp)
# 0: attempts to censor are direct and routine
# 4: government rarely attempts to censor major media in any way
plot(density(vdem_v2$v2mecenefm_osp, na.rm = T))

## Internet censorship effort (v2mecenefi_osp)
# 0: The government successfully blocks Internet access except to sites that are pro-gov
# 3: Internet access is unrestricted by government
plot(density(vdem_v2$v2mecenefi_osp, na.rm = T))

## Percentage of journalists in print and broadcast media who are women (v2mefemjrn)
plot(density(vdem_v2$v2mefemjrn, na.rm = T))

## Harassment of journalists (v2meharjrn_osp)
# 0: No journalists dare to engage in journalistic activities that would offend powerful actors
# 4: Journalists are never harassed by government or powerful nongov actors
plot(density(vdem_v2$v2meharjrn_osp, na.rm = T))


## Calculate global averages by year for government censorship of media (v2mecenefm_osp)
round(aggregate(vdem_v2$v2mecenefm_osp, list(vdem_v2$year), FUN=mean, na.rm = T),2) 


## Calculate global averages by year for journalist harassment ()
round(aggregate(vdem_v2$v2meharjrn_osp, list(vdem_v2$year), FUN=mean, na.rm = T),2) 


## Create table of 2023 regime types
vdem23 <- subset(vdem_v2, year == 2023)

## Assign names to regime types
vdem23$RegimeType <- ifelse(vdem23$v2x_regime == 0, "Closed Autocracy",
                            ifelse(vdem23$v2x_regime == 1, "Electoral Autocracy",
                                   ifelse(vdem23$v2x_regime == 2, "Electoral Democracy",
                                          ifelse(vdem23$v2x_regime == 3, "Liberal Democracy", NA))))

## Create table
table(vdem23$RegimeType) # Reordered for Datawrapper figure


## Calculate journalist harassment by regime type
journ_har <- vdem_v2 %>%
  group_by(v2x_regime, year) %>%
  summarize(regime_mean = mean(v2meharjrn_osp, na.rm = TRUE))

journ_har$regime_mean <- round(journ_har$regime_mean, 2)
View(journ_har)


## Appendix: Number of countries per year
countries <- numeric()

# Loop through each year from 1993 to 2023
for (year_subset in 1993:2023) {

  count <- nrow(subset(vdem_v2, year == year_subset))

  countries <- c(countries, count)
}

print(countries)

## END ##

