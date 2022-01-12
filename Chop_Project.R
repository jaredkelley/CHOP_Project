# Load Libraries and CSVs -------------------------------------------------
library(lubridate)
library(tidyverse)
library(readr)

# Read CSVs
allergies <- read_csv("https://raw.githubusercontent.com/chop-analytics/analyst-take-home-task/master/datasets/allergies.csv")
encounters <- read_csv("https://raw.githubusercontent.com/chop-analytics/analyst-take-home-task/master/datasets/encounters.csv")
medications <- read_csv("https://raw.githubusercontent.com/chop-analytics/analyst-take-home-task/master/datasets/medications.csv")
patients <- read_csv("https://raw.githubusercontent.com/chop-analytics/analyst-take-home-task/master/datasets/patients.csv")
procedures <- read_csv("https://raw.githubusercontent.com/chop-analytics/analyst-take-home-task/master/datasets/procedures.csv")


# EDA and Data setup ------------------------------------------------------

# view the different types of encounters to get a better idea of drug od
encounters %>% 
  count(REASONDESCRIPTION, sort = TRUE) %>% 
  view()

# change datetimes to dates
encounters <- encounters %>% 
  mutate(
    START_ENCOUNTER = date(START),
    STOP_ENCOUNTER = date(STOP)
  )

# filter out just drug overdoses & encounters that occured before 7/15/1999
encounters <- encounters %>% 
  filter(REASONDESCRIPTION == 'Drug overdose',
         START_ENCOUNTER >= '1999-07-15')

# Drop the START and STOP columns
encounters <- encounters %>% 
  select(-START, -STOP)

# Using Patients data, get only the columns we need
patients <- patients %>% 
  select(Id, BIRTHDATE, DEATHDATE)

# join the patients and encounters data
encounters_patients <- left_join(encounters, patients, by = c("PATIENT" = "Id"))

# get the current age of the patient at EACH overdoes
encounters_patients <- encounters_patients %>% 
  mutate(
    AGE = floor(interval(BIRTHDATE, START_ENCOUNTER) / years(1))
  )

# filter on patients that are between 18 & 35
encounters_patients <- encounters_patients %>% 
  filter(between(AGE,18,35))

# Add additional fields ---------------------------------------------------

# make the 4 date fields into dates
encounters_patients <- encounters_patients %>% 
  mutate(
    START_ENCOUNTER = as.Date(START_ENCOUNTER),
    STOP_ENCOUNTER = as.Date(STOP_ENCOUNTER),
    BIRTHDATE = as.Date(BIRTHDATE),
    DEATHDATE = as.Date(DEATHDATE)
  )

# create dummy variable if death date is = to the end of the encounter
encounters_patients <- encounters_patients %>% 
  mutate(
    DEATH_AT_VISIT = case_when(
      STOP_ENCOUNTER == DEATHDATE ~ 1,
      TRUE ~ 0
    )
  )

# Now working with medications dataset
# create dummy variable showing whether medication is an opioid 
medications <- medications %>% 
  mutate(
    OPIOID = case_when(
      CODE == 316049 ~ 1,
      CODE == 429503 ~ 1,
      CODE == 406022 ~ 1,
      TRUE ~ 0
    )
  )

medications <- medications %>% 
  mutate(START_MEDS = as.Date(START),
         STOP_MEDS = as.Date(STOP),
         COUNT_MEDS = 1
         )


# remove unncessary columns in encounters & patients data
encounters_patients <- encounters_patients %>% 
  select(
    -PROVIDER, -ENCOUNTERCLASS, DESCRIPTION, -COST
  )

# remove columns from medications
medications <- medications %>% 
  select(-START, -STOP)

# we only want medications from patients in our cohort
cohort_patients <- encounters_patients %>% 
  distinct(PATIENT)

medications <- medications %>% 
  filter(PATIENT %in% cohort_patients$PATIENT
  )

# make STOP dates with NAs in medications be current dates
medications <- medications %>% 
  mutate(
    STOP_MEDS = replace_na(STOP_MEDS, "2021-12-31")
    )
  )

# inner join the medications and encounters_patients tables in order to get all
# instances of medications and encounters
medications_encounters_patients <- inner_join(medications, encounters_patients, by = "PATIENT")

# select only the columns we need
medications_encounters_patients <- medications_encounters_patients %>% 
  select(PATIENT, START_MEDS, STOP_MEDS, OPIOID, START_ENCOUNTER, STOP_ENCOUNTER, 'CODE.x', 'DESCRIPTION.x')

# add an additional column that counts a 1 if the encounter date is within the medication dates
medications_encounters_patients <- medications_encounters_patients %>% 
  mutate(
    COUNT_CURRENT_MEDS = case_when(
      START_ENCOUNTER >= START_MEDS &
      START_ENCOUNTER <= STOP_MEDS ~ 1,
      TRUE ~ 0
    )
  )

# update opioid column to be current opioids
medications_encounters_patients <- medications_encounters_patients %>% 
  mutate(
    CURRENT_OPIOID_IND = case_when(
      OPIOID == 0 ~ 0,
      OPIOID == 1 & START_ENCOUNTER >= START_MEDS &
      START_ENCOUNTER <= STOP_MEDS ~ 1,
      TRUE ~ 0
    )
  )

medications_encounters_patients_summary <- medications_encounters_patients %>% 
  group_by(PATIENT, START_ENCOUNTER, STOP_ENCOUNTER) %>% 
  summarise(COUNT_CURRENT_MEDS = sum(COUNT_CURRENT_MEDS),
            CURRENT_OPIOID_IND = sum(CURRENT_OPIOID_IND))

# combine patient and start date column in order to match back with encounters data
medications_encounters_patients_summary <- medications_encounters_patients_summary %>% 
  unite(PATIENT_START, PATIENT:START_ENCOUNTER, sep = "-", remove = FALSE)

# do the same process in the encounters data
encounters_patients <- encounters_patients %>% 
  unite(PATIENT_START, PATIENT,START_ENCOUNTER, sep = "-", remove = FALSE)

# now join the two tables by PATIENT_START
encounters_patients <- left_join(encounters_patients, medications_encounters_patients_summary, by = "PATIENT_START")

# check to make sure the 10 encounters that did not have meds have NAs
encounters_patients %>% 
  filter(is.na(COUNT_CURRENT_MEDS)) %>% 
  view()

# drop columns we don't need
encounters_patients <- encounters_patients %>% 
  select(
    Id, PATIENT.x, CODE, DESCRIPTION, REASONCODE, REASONDESCRIPTION,
    START_ENCOUNTER.x, STOP_ENCOUNTER.x, BIRTHDATE, DEATHDATE, AGE,
    DEATH_AT_VISIT, COUNT_CURRENT_MEDS, CURRENT_OPIOID_IND
  )

# make NAs 0 in the two columns for 10 records that did not have meds
encounters_patients <- encounters_patients %>% 
  mutate(
    COUNT_CURRENT_MEDS = replace_na(COUNT_CURRENT_MEDS, 0),
    CURRENT_OPIOID_IND = replace_na(CURRENT_OPIOID_IND, 0)
  )

# rename columns to drop .x
encounters_patients <- encounters_patients %>% 
  rename(
    PATIENT = PATIENT.x,
    START_ENCOUNTER = START_ENCOUNTER.x,
    STOP_ENCOUNTER = STOP_ENCOUNTER.x
  )
# make sure that we are ordered by PATIENT and Start encounter date
encounters_patients <- encounters_patients %>% 
  arrange(PATIENT, START_ENCOUNTER)

# Add final columns for readmission ---------------------------------------

# 90 day readmission
encounters_patients <- encounters_patients %>% 
  mutate(
    READMISSION_90_DAY_IND = case_when(
      as.numeric(lead(START_ENCOUNTER) - START_ENCOUNTER) <= 90 & lead(PATIENT) == PATIENT ~ 1,
      TRUE ~ 0
    )
  )

# 30 day readmission
encounters_patients <- encounters_patients %>% 
  mutate(
    READMISSION_30_DAY_IND = case_when(
      as.numeric(lead(START_ENCOUNTER) - START_ENCOUNTER) <= 30 & lead(PATIENT) == PATIENT ~ 1,
      TRUE ~ 0
    )
  )

# Add column for the first readmission date NA if no readmission
encounters_patients <- encounters_patients %>% 
  mutate(
    FIRST_READMISSION_DATE = case_when(
      READMISSION_90_DAY_IND == 1 ~ as.Date(lead(START_ENCOUNTER)),
      TRUE ~ NA_Date_
    )
  )

# to finish, select only the columns needed for this project
encounters_patients <- encounters_patients %>% 
  select(
    PATIENT, Id, START_ENCOUNTER, AGE, DEATH_AT_VISIT,
    COUNT_CURRENT_MEDS, CURRENT_OPIOID_IND, READMISSION_90_DAY_IND,
    READMISSION_30_DAY_IND, FIRST_READMISSION_DATE
  )

# rename columns to exactly what the project asked for
encounters_patients <- encounters_patients %>% 
  rename(
    PATIENT_ID = PATIENT,
    ENCOUNTER_ID = Id,
    HOSPITAL_ENCOUNTER_DATE = START_ENCOUNTER,
    AGE_AT_VISIT = AGE,
    DEATH_AT_VISIT_IND = DEATH_AT_VISIT
  )

# death at visit ind should be na if 0
encounters_patients <- encounters_patients %>% 
  mutate(
    DEATH_AT_VISIT_IND = case_when(
      DEATH_AT_VISIT_IND == 0 ~ NA_character_,
      TRUE ~ as.character(DEATH_AT_VISIT_IND)
    )
  )

# create csv for export
write.csv(x = encounters_patients, file = "Jared_Kelley")
