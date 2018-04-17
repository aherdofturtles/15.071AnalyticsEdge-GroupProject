library(dplyr)
rm(list = ls()) # Clear environment


#################
# Reading Raw Data from Excel File (commented out after saved locally)
#################
# install.packages("read.xl")
# library(readxl)

# Read xlsx downloaded from https://www.foreignlaborcert.doleta.gov/performancedata.cfm
# 2017 data is from Oct-17 to Sep-17 and consists 624,650 rows, 52 variables
# h1b_raw = read_excel("~/GitHub/15.071AnalyticsEdge-GroupProject/H-1B_Disclosure_Data_FY17.xlsx")

# Saving Data
# save(h1b_raw, file = "H1B_Raw.RData")

# Loading Data
load("H1B_Raw.RData")

# Selecting only the relevant columns
h1b = h1b_raw %>% 
  
  select(CASE_NUMBER, # Not actually used in regression, but used as a helpful reference
         CASE_STATUS,
         CASE_SUBMITTED,
         DECISION_DATE,
         VISA_CLASS,
#         EMPLOYMENT_START_DATE
#         EMPLOYMENT_END_DATE
         EMPLOYER_NAME,
#         EMPLOYER_BUSINESS_DBA
#         EMPLOYER_ADDRESS
         EMPLOYER_CITY,
         EMPLOYER_STATE,
         EMPLOYER_POSTAL_CODE,
         EMPLOYER_COUNTRY,
#         EMPLOYER_PROVINCE
#         EMPLOYER_PHONE
#         EMPLOYER_PHONE_EXT
         AGENT_REPRESENTING_EMPLOYER,
         AGENT_ATTORNEY_NAME,
#         AGENT_ATTORNEY_CITY
#         AGENT_ATTORNEY_STATE
         JOB_TITLE,
         SOC_CODE,
#         SOC_NAME,
         NAICS_CODE,
         TOTAL_WORKERS,
         NEW_EMPLOYMENT,
         CONTINUED_EMPLOYMENT,
         CHANGE_PREVIOUS_EMPLOYMENT,
         NEW_CONCURRENT_EMPLOYMENT,
         CHANGE_EMPLOYER,
         AMENDED_PETITION,
         FULL_TIME_POSITION,
         PREVAILING_WAGE,
         PW_UNIT_OF_PAY,
#         PW_WAGE_LEVEL Variables
#         PW_SOURCE Variables
#         PW_SOURCE_YEAR Year
#         PW_SOURCE_OTHER
         WAGE_RATE_OF_PAY_FROM,
         WAGE_RATE_OF_PAY_TO,
         WAGE_UNIT_OF_PAY,
         H1B_DEPENDENT,
         WILLFUL_VIOLATOR,
#         SUPPORT_H1B
         LABOR_CON_AGREE,
#         PUBLIC_DISCLOSURE_LOCATION
         WORKSITE_CITY,
#         WORKSITE_COUNTY
         WORKSITE_STATE,
         WORKSITE_POSTAL_CODE
#         ORIGINAL_CERT_DATE  
  ) %>% # 624,650 rows x 34 columns
  
  # Remove NAs
  filter(!is.na(PW_UNIT_OF_PAY) &
           !is.na(WAGE_UNIT_OF_PAY) &
           !is.na(WAGE_RATE_OF_PAY_TO) &
           !is.na(WAGE_RATE_OF_PAY_FROM)
  ) %>% # 624598 rows
  
  # Special Filters
  filter(VISA_CLASS == "H1-B" |  # Focus only on H-1B
         CASE_STATUS != "WITHDRAWN"  # These are neither certified not rejected 
  ) #603,833 rows
  
# Convert columns into correct data types
h1b$CASE_STATUS = as.factor(h1b$CASE_STATUS)
h1b$CASE_SUBMITTED = as.Date(strptime(h1b$CASE_SUBMITTED, "%Y-%m-%d"))
h1b$DECISION_DATE = as.Date(strptime(h1b$DECISION_DATE, "%Y-%m-%d"))
h1b$EMPLOYER_CITY = as.factor(h1b$EMPLOYER_CITY)
h1b$EMPLOYER_STATE = as.factor(h1b$EMPLOYER_STATE)
h1b$EMPLOYER_POSTAL_CODE = as.factor(h1b$EMPLOYER_POSTAL_CODE)
h1b$EMPLOYER_COUNTRY = as.factor(h1b$EMPLOYER_COUNTRY)
h1b$AGENT_REPRESENTING_EMPLOYER = as.factor(h1b$AGENT_REPRESENTING_EMPLOYER)
h1b$SOC_CODE = as.factor(h1b$SOC_CODE)
h1b$NAICS_CODE = as.factor(h1b$NAICS_CODE)
h1b$FULL_TIME_POSITION = as.factor(h1b$FULL_TIME_POSITION)
h1b$PW_UNIT_OF_PAY = as.factor(h1b$PW_UNIT_OF_PAY)
h1b$WAGE_UNIT_OF_PAY = as.factor(h1b$WAGE_UNIT_OF_PAY)
h1b$H1B_DEPENDENT = as.factor(h1b$H1B_DEPENDENT)
h1b$WILLFUL_VIOLATOR = as.factor(h1b$WILLFUL_VIOLATOR)
h1b$LABOR_CON_AGREE = as.factor(h1b$LABOR_CON_AGREE)
h1b$WORKSITE_CITY = as.factor(h1b$WORKSITE_CITY)
h1b$WORKSITE_STATE = as.factor(h1b$WORKSITE_STATE)
h1b$WORKSITE_POSTAL_CODE = as.factor(h1b$WORKSITE_POSTAL_CODE)

# Standardizing Prevailing Wage & Wage
Unit_to_Yearly = function(wage,wage_unit) {
  return(ifelse(wage_unit == "Year", wage, 
                ifelse(wage_unit == "Hour", 2080*wage,
                       ifelse(wage_unit == "Week", 52*wage,
                              ifelse(wage_unit == "Month", 12*wage,
                                     26*wage))))) } # Bi-weekly
h1b$PW_STD = Unit_to_Yearly(h1b$PREVAILING_WAGE, h1b$PW_UNIT_OF_PAY)

# Average Wage Rate calculation
Avg_Wage = function(wage_to,wage_from) {return(ifelse(wage_to == 0, wage_from, (wage_to+wage_from)/2))}
h1b$WAGE_STD = Unit_to_Yearly(Avg_Wage(h1b$WAGE_RATE_OF_PAY_TO, h1b$WAGE_RATE_OF_PAY_FROM),h1b$WAGE_UNIT_OF_PAY)

# Certified = 0, Rejected = 1
Certification = function(status) {return(ifelse(status == "DENIED", 0, 1))}
h1b$RESULT = Certification(h1b$CASE_STATUS)
h1b$RESULT= as.factor(h1b$RESULT)

# Deselect certain columns to reduce size
h1b = h1b %>% 
  
  select(-VISA_CLASS, # All visas are H-1B
         -PREVAILING_WAGE, # Replaced with PW_STD
         -PW_UNIT_OF_PAY, # Replaced with PW_STD
         -WAGE_RATE_OF_PAY_FROM, # Replaced with WAGE_STD
         -WAGE_RATE_OF_PAY_TO, # Replaced with WAGE_STD
         -WAGE_UNIT_OF_PAY
  ) # 603,833 rows x 31 columns

# Save workspaces in same file
save(h1b, file = "H1B.RData")
