rm(list = ls()) # Clear environment

install.packages("read.xl")
library(readxl)
library(dplyr)

# Read xlsx downloaded from https://www.foreignlaborcert.doleta.gov/performancedata.cfm
h1b_raw = read_excel("~/GitHub/15.071AnalyticsEdge-GroupProject/H-1B_Disclosure_Data_FY17.xlsx") #Oct 2016 to Sep 2017; 624,650 rows

# Selecting only the relevant columns
h1b = h1b_raw %>% 
  
  select(CASE_NUMBER,
         CASE_STATUS,
         CASE_SUBMITTED,
         DECISION_DATE,
         VISA_CLASS,
#         EMPLOYMENT_START_DATE Beginning date of employment.
#         EMPLOYMENT_END_DATE Ending date of employment.
         EMPLOYER_NAME,
#         EMPLOYER_BUSINESS_DBA Trade Name or dba name of employer submitting labor condition
#         EMPLOYER_ADDRESS
         EMPLOYER_CITY,
         EMPLOYER_STATE,
         EMPLOYER_POSTAL_CODE,
         EMPLOYER_COUNTRY,
#         EMPLOYER_PROVINCE
#         EMPLOYER_PHONE
#         EMPLOYER_PHONE_EXT
         AGENT_REPRESENTING_EMPLOYER, # Y = Employer is represented by an Agent/Attorney
         AGENT_ATTORNEY_NAME,
#         AGENT_ATTORNEY_CITY City information for the Agent or Attorney filing an H-1B application on
#         AGENT_ATTORNEY_STATE State information for the Agent or Attorney filing an H-1B application on
         JOB_TITLE,
         SOC_CODE,
#         SOC_NAME,
         NAICS_CODE, # Industry code as classified by the North American Industrial Classification System
         TOTAL_WORKERS, # Total number of foreign workers requested by the Employer(s).
         NEW_EMPLOYMENT, # Worker(s) will begin employment for new employer,
         CONTINUED_EMPLOYMENT, # Worker(s) will be continuing employment with same employer, 
         CHANGE_PREVIOUS_EMPLOYMENT, # Worker(s) will be continuing employment with same employer with same duties
         NEW_CONCURRENT_EMPLOYMENT, # Worker(s) will begin employment with additional employer I-29
         CHANGE_EMPLOYER, # Worker(s) will begin employment for new employer, using the same classification currently held
         AMENDED_PETITION, # Worker(s) will be continuing employment with same employer with material change to job duties
         FULL_TIME_POSITION, #Y = Full Time Position; N = Part Time Position.
         PREVAILING_WAGE, # Prevailing Wage for the job being requested for temporary labor condition.
         PW_UNIT_OF_PAY, # Daily (DAI), Hourly (HR), Bi-weekly (BI),Weekly (WK),Monthly (MTH),Yearly (YR)
#         PW_WAGE_LEVEL Variables include "I", "II", "III", "IV" or "N/A."
#         PW_SOURCE Variables include "OES", "CBA", "DBA", "SCA" or "Other".
#         PW_SOURCE_YEAR Year the Prevailing Wage Source was Issued.
#         PW_SOURCE_OTHER, If "Other Wage Source", provide the source of wage.
         WAGE_RATE_OF_PAY_FROM, # Employer's proposed wage rate.
         WAGE_RATE_OF_PAY_TO, # Maximum proposed wage rate.
         WAGE_UNIT_OF_PAY, # "Hour", "Week", "Bi-Weekly", "Month", "Year"
         H1B_DEPENDENT, # Y = Employer is H-1B Dependent; 1-25 FT employees = 8 H1B, 26-50 = 13 H1B, >50 =15% H1B workers.
         WILLFUL_VIOLATOR, # Y = Employer has been previously found to be a Willful Violator;
#         SUPPORT_H1B Y = Employer will use the temporary LCA only to support H-1B petitions of exempt H-1B worker(s); 
         LABOR_CON_AGREE, #Y = Employer agrees to the responses to the Labor Condition Statements
#         PUBLIC_DISCLOSURE_LOCATION Variables include "Place of Business" or "Place of Employment."
         WORKSITE_CITY, # City information of the foreign worker's intended area of employment.
#         WORKSITE_COUNTY # County information of the foreign worker's intended area of employment.
         WORKSITE_STATE, #State information of the foreign worker's intended area of employment.
         WORKSITE_POSTAL_CODE # Zip Code information of the foreign worker's intended area of employment.
#         ORIGINAL_CERT_DATE  
  ) %>%
  
  filter(VISA_CLASS == "H1-B" | # We focus only on H-1B
           CASE_STATUS != "WITHDRAWN" # These are neither certified not rejected 
  ) # 603,878 rows

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

## Need to do the following:
# Standardize Prevailing Wage and Wage of Unit Pay
# Calculate Average Wage
# Calculate Certified or Not
# Spell check?  Maybe not
# Remove columns not used (PW Unit,, Wage Unit, To Wage, From Wage, Visa Type)
# Keep Case Status!

# Then display Raw Data Wrangled
str(h1b)


## Keep in mind: 
# Employer name contains certain words, is a certain employer
# Lawyer name contains certain words, is a certain lawyer
# Job title  name contains certain words, is a certain job title