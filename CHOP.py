import pandas as pd
import numpy as np

# set width to view tables
from numpy import NaN

desired_width = 320
pd.set_option('display.width', desired_width)
np.set_printoptions(linewidth=desired_width)
pd.set_option('display.max_columns', 10)

# define our 5 tables
allergies = pd.read_csv('allergies')
encounters = pd.read_csv('encounters')
medications = pd.read_csv('medications')
patients = pd.read_csv('patients')
procedures = pd.read_csv('procedures')

# view the different types of encounters to get a better idea of drug ODs
count_encounters = encounters['REASONDESCRIPTION'].value_counts()

# now we see that there are 2,448 drug overdose encounters in the dataset
# now filter out only drug overdose occurrences
OD_encounters = encounters[encounters['REASONCODE'] == 55680006]

# Change START & STOP to dates
OD_encounters['START'] = pd.to_datetime(OD_encounters['START']).dt.date
OD_encounters['STOP'] = pd.to_datetime(OD_encounters['STOP']).dt.date

# select only start dates >= 1999-07-15
# set variable for the minimum date
mindate = pd.to_datetime("1999-7-15").date()

# use mindate to create filter
OD_encounters = OD_encounters[OD_encounters['START'] >= mindate]

# drop columns we don't need
OD_encounters = OD_encounters[['Id', 'START', 'STOP', 'PATIENT', 'REASONDESCRIPTION']]

# set up patients table with only what we need
patients_merge = patients[['Id', 'BIRTHDATE', 'DEATHDATE']]

# join OD encounters with patients table by PATIENT
encounters_patients = pd.merge(OD_encounters, patients_merge, how="left", left_on="PATIENT", right_on="Id")

# convert BIRHDATE & DEATHDATE as date
encounters_patients['BIRTHDATE'] = pd.to_datetime(encounters_patients['BIRTHDATE']).dt.date
encounters_patients['DEATHDATE'] = pd.to_datetime(encounters_patients['DEATHDATE']).dt.date

# add an age at time of encounter column
encounters_patients['AGE_AT_VISIT'] = encounters_patients['START'] - encounters_patients['BIRTHDATE']
encounters_patients['AGE_AT_VISIT'] = np.floor(encounters_patients['AGE_AT_VISIT'] / np.timedelta64(1, 'Y'))

# now filter out and patients not between 18 and 35 at time of visit
encounters_patients = encounters_patients[
    (encounters_patients['AGE_AT_VISIT'] >= 18) & (encounters_patients['AGE_AT_VISIT'] <= 35)]

# create dummy variable if death date is = stop
encounters_patients.loc[encounters_patients['DEATHDATE'] == encounters_patients['STOP'], 'DEATH_AT_VISIT_IND'] = 1
encounters_patients.loc[encounters_patients['DEATHDATE'] != encounters_patients['STOP'], 'DEATH_AT_VISIT_IND'] = NaN

# filter out NAs just to check
no_NAs_encounters_patients = encounters_patients[encounters_patients['DEATH_AT_VISIT_IND'] == 1]

# now working with medications data
# create opioid indicator
medications['CURRENT_OPIOID_IND'] = np.where(
    (medications['CODE'] != 316049) & (medications['CODE'] != 429503) & (medications['CODE'] != 406022), 0, 1
)
# select only columns we need
medications = medications[['START', 'STOP', 'PATIENT', 'ENCOUNTER', 'CODE', 'DESCRIPTION', 'CURRENT_OPIOID_IND']]

# make start and stop as dates
medications['START'] = pd.to_datetime(medications['START']).dt.date
medications['STOP'] = pd.to_datetime(medications['STOP']).dt.date

# make stop NaT into current year date
medications['STOP'] = medications['STOP'].fillna('2021-12-31')

# join encounters and medications data together
medications_encounters_patients = pd.merge(medications, encounters_patients, how='inner', on='PATIENT')

# select only the columns we need
medications_encounters_patients = medications_encounters_patients[['PATIENT', 'START_x', 'STOP_x', 'START_y', 'STOP_y',
                                                                   'CURRENT_OPIOID_IND', 'CODE', 'DESCRIPTION']]

# make 4 date columns dates
medications_encounters_patients['START_y'] = pd.to_datetime(medications_encounters_patients['START_y']).dt.date
medications_encounters_patients['START_x'] = pd.to_datetime(medications_encounters_patients['START_x']).dt.date
medications_encounters_patients['STOP_y'] = pd.to_datetime(medications_encounters_patients['STOP_y']).dt.date
medications_encounters_patients['STOP_x'] = pd.to_datetime(medications_encounters_patients['STOP_x']).dt.date

# add additional column that counts 1 if the encounter date is within medication dates
medications_encounters_patients['COUNT_CURRENT_MEDS'] = np.where(
    (medications_encounters_patients['START_y'] >= medications_encounters_patients['START_x']) &
    (medications_encounters_patients['START_y'] <= medications_encounters_patients['STOP_x']), 1, 0
)

# update opioid column to only count if date is within medication dates
medications_encounters_patients['CURRENT_OPIOID_IND'] = np.where(
    (medications_encounters_patients['CURRENT_OPIOID_IND'] == 1) &
    (medications_encounters_patients['START_y'] >= medications_encounters_patients['START_x']) &
    (medications_encounters_patients['START_y'] <= medications_encounters_patients['STOP_x']), 1, 0
)

# summarise full table by grouping by patient and start and stop encounters
meds_summary = medications_encounters_patients.groupby(['PATIENT', 'START_y', 'STOP_y'], as_index=False).\
    agg({'COUNT_CURRENT_MEDS': 'sum', 'CURRENT_OPIOID_IND': 'sum'})

# combine patient and start in order to have a key to join back to encounters table
meds_summary['KEY'] = meds_summary['PATIENT'] + meds_summary['START_y'].astype(str)

# complete the same process for the encounters & patients df
encounters_patients['KEY'] = encounters_patients['PATIENT'] + encounters_patients['START'].astype(str)

# merge encounters & patients with medications summary
encounters_patients = pd.merge(encounters_patients, meds_summary, how='left', on='KEY')

# check to make sure 10 encounters that did not have meds have NAs
check_encounters = encounters_patients.sort_values(by=['COUNT_CURRENT_MEDS'])

# drop unneeded columns
encounters_patients = encounters_patients[['Id_x', 'START', 'STOP', 'PATIENT_x',
                                           'BIRTHDATE', 'DEATHDATE', 'AGE_AT_VISIT', 'DEATH_AT_VISIT_IND',
                                           'REASONDESCRIPTION', 'COUNT_CURRENT_MEDS', 'CURRENT_OPIOID_IND']]

# make NaNs zero
encounters_patients['COUNT_CURRENT_MEDS'] = encounters_patients['COUNT_CURRENT_MEDS'].fillna(0)
encounters_patients['CURRENT_OPIOID_IND'] = encounters_patients['CURRENT_OPIOID_IND'].fillna(0)

# make sure we're sorted by patient and start date
encounters_patients = encounters_patients.sort_values(by=['PATIENT_x', 'START'])

# add 90 day readmission column
encounters_patients['READMISSION_90_DAY_IND'] = np.where(
    ((encounters_patients['START'].shift(-1) - encounters_patients['START']).dt.days <= 90) &
    (encounters_patients['PATIENT_x'].shift(-1) == encounters_patients['PATIENT_x']), 1, 0
)

# now add 30 day readmission column
encounters_patients['READMISSION_30_DAY_IND'] = np.where(
    ((encounters_patients['START'].shift(-1) - encounters_patients['START']).dt.days <= 30) &
    (encounters_patients['PATIENT_x'].shift(-1) == encounters_patients['PATIENT_x']), 1, 0
)

# now add date of first readmission
encounters_patients['FIRST_READMISSION_DATE'] = np.where(
    (encounters_patients['READMISSION_90_DAY_IND'] == 1), encounters_patients['START'].shift(-1), pd.NaT
)

# check 90 day values
CHECK_90_DAYS = encounters_patients[encounters_patients['READMISSION_90_DAY_IND'] == 1]

# rename columns
encounters_patients = encounters_patients.rename(columns={'Id_x': 'ENCOUNTER_ID', 'START': 'HOSPITAL_ENCOUNTER_DATE',
                                                          'PATIENT_x': 'PATIENT_ID'})

# select only the columns we need
encounters_patients = encounters_patients[['PATIENT_ID', 'ENCOUNTER_ID', 'HOSPITAL_ENCOUNTER_DATE', 'AGE_AT_VISIT',
                                           'DEATH_AT_VISIT_IND', 'COUNT_CURRENT_MEDS', 'CURRENT_OPIOID_IND',
                                           'READMISSION_90_DAY_IND', 'READMISSION_30_DAY_IND',
                                           'FIRST_READMISSION_DATE']]

encounters_patients.to_csv(r'C:\Users\jared\OneDrive\Desktop\Programming\Python\CHOP\Jared_Kelley.csv', index=False)
print(encounters_patients)
