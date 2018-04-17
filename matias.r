# Loading Data
load("H1B_Raw.RData") # raw data stored as h1b_raw
load("H1B.RData") # "cleaned" data stored as h1b

# There are 603,833 rows x 31 variables in the "cleaned" dataset
str(h1b)

# Many variables contain NAs - Regression will IGNORE these rows. Need to assume what to do.
# See the list at the bottom for quicker debugging
# For example, this variable has 92k NAs!
summary(h1b$EMPLOYER_COUNTRY) 

# 50+7 states include:
# 1. WashingtonDC DC
# 2. Federated States of Micronesia FM
# 3. Guam	GU
# 4. Northern Mariana Islands MP
# 5. Palau	PW
# 6. Puerto Rico	PR
# 7. Virgin Islands VI

# New variables
table(h1b$RESULT) # If VISA_STATUS is denied, this column in 0. Otherwise 1
summary(h1b$PW_STD) # Standardized Prevailing Wage
summary(h1b$WAGE_STD) # Standardized Wage offered


#########################################
# Can be safely deleted/ignored; useful infor debugging only
#########################################
table(h1b$CASE_NUMBER) # OK, not used
table(h1b$CASE_STATUS) # OK
table(h1b$CASE_SUBMITTED) # OK
table(h1b$DECISION_DATE) # OK
table(h1b$EMPLOYER_NAME) # OK
table(h1b$EMPLOYER_CITY) # OK
table(h1b$EMPLOYER_STATE) # OK, 50+7 states
table(h1b$EMPLOYER_POSTAL_CODE) # Zip codes  OK
summary(h1b$EMPLOYER_COUNTRY) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$AGENT_REPRESENTING_EMPLOYER) # Contains NAs - DECIDE HOW TO TREAT 
table(h1b$AGENT_ATTORNEY_NAME) # OK 
table(h1b$JOB_TITLE) # OK
table(h1b$SOC_CODE) # OK
table(h1b$NAICS_CODE) # OK
table(h1b$TOTAL_WORKERS) # OK
table(h1b$NEW_EMPLOYMENT) # OK
table(h1b$CONTINUED_EMPLOYMENT) # OK
table(h1b$CHANGE_PREVIOUS_EMPLOYMENT) # OK
table(h1b$NEW_CONCURRENT_EMPLOYMENT) # OK
table(h1b$CHANGE_EMPLOYER) # OK
table(h1b$AMENDED_PETITION) # OK
summary(h1b$FULL_TIME_POSITION) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$H1B_DEPENDENT) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$WILLFUL_VIOLATOR) # Contains NAs - DECIDE HOW TO TREAT 
summary(h1b$LABOR_CON_AGREE) # Contains NAs - DECIDE HOW TO TREAT 
table(h1b$WORKSITE_CITY) # Text OK
table(h1b$WORKSITE_STATE) # OK, 50+7 states
table(h1b$WORKSITE_POSTAL_CODE) # ZIP codes OK
min(h1b$PW_STD) # OK
min(h1b$WAGE_STD) # OK
table(h1b$RESULT) # OK