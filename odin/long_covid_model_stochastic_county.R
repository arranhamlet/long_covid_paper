# Initial states and dimensions -------------------------------------------
#Set up simulation dimensions
case_delay <- user()
omicron_long_covid_multiplier <- user()
time_omicron_switch <- user()
time_length <- user()
age_group_number <- user()
sex_number <- user()
race_ethnicity_number <- user()
vaccination_status <- user()
num_counties <- user()

#Set up incoming data streams
infection[,,,,,] <- user()
cases[,,,,,] <- user()
hospitalization[,,,,,] <- user()

#Set up initial states and dimensions
initial(long_non_hosp[,,,,]) <- 0
initial(permanent[,,,,]) <- 0
initial(long_hosp[,,,,]) <- 0
initial(into_long_covid[,,,,]) <- 0

# Parameters --------------------------------------------------------------
bd[,] <- user() #background death rate - default is the 1/average lifespan in Washington state (80.2 years)
non_hosp_additional_mortality_long <- user() #Additional mortality to long covid non-hospitalized - default is 30%
hosp_additional_mortality_long <- user() #Additional mortality to hospitalized long covid

infection_long_non_hosp[,,,] <- user() #The proportion of infections that get long COVID
cases_long_non_hosp[,,,] <- user() #The proportion of cases that get long COVID
hosp_long_hosp[,,,] <- user() #The proportion of hospitalized that get long COVID

recover_non_hosp <- 1 - permanent_non_hosp_prop #The proportion of non-hospitalized that recover
permanent_non_hosp_prop <- user() #The proportion of non-hospitalized that are permanent

recover_hosp <- 1 - permanent_hosp_prop #The proportion of hospitalized that recover
permanent_hosp_prop <- user() #The proportion of hospitalized that are permanent

recovery_rate_non_hosp <- user()#The rate at which non-hospitalized recover
recovery_rate_hosp_multiplier <- user() 

recovery_rate_hosp <- recovery_rate_non_hosp * 1/recovery_rate_hosp_multiplier #The rate at which hospitalized recover 


# Set up flows between compartments ---------------------------------------
#Set up in flow to long_non_hosp
dim(infection_long_non_hosp_upd) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)
dim(cases_long_non_hosp_upd) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)
dim(hosp_long_hosp_upd) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)

infection_long_non_hosp_upd[,,,] <- infection_long_non_hosp[i, j, k, l] * if(as.integer(step) >= time_omicron_switch) omicron_long_covid_multiplier else 1
cases_long_non_hosp_upd[,,,] <- cases_long_non_hosp[i, j, k, l] * if(as.integer(step) >= time_omicron_switch) omicron_long_covid_multiplier else 1
hosp_long_hosp_upd[,,,] <- hosp_long_hosp[i, j, k, l] * if(as.integer(step) >= time_omicron_switch) omicron_long_covid_multiplier else 1

infection_in_to_long_non_hosp[,,,,] <- rbinom(infection[i, j, k, l, i5, as.integer(step)], infection_long_non_hosp_upd[i, j, k, l])
cases_in_to_long_non_hosp[,,,,] <- rbinom(cases[i, j, k, l, i5, as.integer(step)], cases_long_non_hosp_upd[i, j, k, l])
into_long_non_hosp[,,,,] <- infection_in_to_long_non_hosp[i, j, k, l, i5] + cases_in_to_long_non_hosp[i, j, k, l, i5]
#Set up out flow from long_non_hosp
leaving_long_non_hosp[,,,,] <- rbinom(long_non_hosp[i, j, k, l, i5], recovery_rate_non_hosp * recover_non_hosp + permanent_non_hosp_prop + bd[i, j] * non_hosp_additional_mortality_long)
leaving_long_non_hosp_death[,,,,] <- rbinom(leaving_long_non_hosp[i, j, k, l, i5], (bd[i, j] * non_hosp_additional_mortality_long)/(recovery_rate_non_hosp * recover_non_hosp + permanent_non_hosp_prop + bd[i, j] * non_hosp_additional_mortality_long))
leaving_long_non_hosp_recovery[,,,,] <- delay(rbinom(leaving_long_non_hosp[i, j, k, l, i5] - leaving_long_non_hosp_death[i, j, k, l, i5], (recovery_rate_non_hosp * recover_non_hosp)/(recovery_rate_non_hosp * recover_non_hosp + permanent_non_hosp_prop)), case_delay)
leaving_long_non_hosp_permanent[,,,,] <- delay(leaving_long_non_hosp[i, j, k, l, i5] - leaving_long_non_hosp_death[i, j, k, l, i5] - leaving_long_non_hosp_recovery[i, j, k, l, i5], case_delay)
#Set up hospitalization
into_long_hosp[,,,,] <- rbinom(hospitalization[i, j, k, l, i5, as.integer(step)], hosp_long_hosp_upd[i, j, k, l])
#Set up out flow from long_hosp
leaving_long_hosp[,,,,] <- rbinom(long_hosp[i, j, k, l, i5], recovery_rate_hosp * recover_hosp + permanent_hosp_prop + bd[i, j] * hosp_additional_mortality_long)
leaving_long_hosp_death[,,,,] <- rbinom(leaving_long_hosp[i, j, k, l, i5], (bd[i, j] * hosp_additional_mortality_long)/(recovery_rate_hosp * recover_hosp + permanent_hosp_prop + bd[i, j] * hosp_additional_mortality_long))
leaving_long_hosp_recovery[,,,,] <- delay(rbinom(leaving_long_hosp[i, j, k, l, i5] - leaving_long_hosp_death[i, j, k, l, i5], (recovery_rate_hosp * recover_hosp)/(recovery_rate_non_hosp * 1/recovery_rate_hosp * recover_hosp + permanent_hosp_prop)), case_delay)
leaving_long_hosp_permanent[,,,,] <- delay(leaving_long_hosp[i, j, k, l, i5] - leaving_long_hosp_death[i, j, k, l, i5] - leaving_long_hosp_recovery[i, j, k, l, i5], case_delay)

# Model equations ---------------------------------------------------------
update(long_non_hosp[,,,,]) <- long_non_hosp[i, j, k, l, i5] + into_long_non_hosp[i, j, k, l, i5] - leaving_long_non_hosp[i, j, k, l, i5]
update(long_hosp[,,,,]) <- long_hosp[i, j, k, l, i5] + into_long_hosp[i, j, k, l, i5] - leaving_long_hosp[i, j, k, l, i5]
update(permanent[,,,,]) <- permanent[i, j, k, l, i5] + leaving_long_hosp_permanent[i, j, k, l, i5] + leaving_long_non_hosp_permanent[i, j, k, l, i5] - rbinom(permanent[i, j, k, l, i5], bd[i, j])
update(into_long_covid[,,,,]) <- into_long_non_hosp[i, j, k, l, i5] + into_long_hosp[i, j, k, l, i5]

# Set up dimensions -------------------------------------------------------
dim(infection) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties, time_length)
dim(cases) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties, time_length)
dim(hospitalization) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties, time_length)
dim(infection_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)
dim(cases_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)
dim(hosp_long_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status)
dim(bd) <- c(age_group_number, sex_number)
dim(infection_in_to_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(cases_in_to_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(into_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_non_hosp_death) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_non_hosp_recovery) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_non_hosp_permanent) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(into_long_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_hosp_death) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_hosp_recovery) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(leaving_long_hosp_permanent) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(long_non_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(long_hosp) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(into_long_covid) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)
dim(permanent) <- c(age_group_number, sex_number, race_ethnicity_number, vaccination_status, num_counties)

# Custom outputs -----------------------------------------------------------------
# output(leaving_nonhosp) <- sum(leaving_delay_long_non_hosp[i, j, k, l, i5])
# output(leaving_hosp) <- sum(leaving_delay_long_hosp[i, j, k, l, i5])