#install.package("dplyr")
#install.packages("readxl")
library(readxl)
#Read Admission Dataset
library(dplyr)

ic2004 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2004.csv")
ic2005 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2005.csv")
ic2006 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2006.csv")
ic2007 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2007.csv")
ic2008 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2008_rv.csv")
ic2009 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2009_rv.csv")
ic2010 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2010_rv.csv")
ic2011 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2011_rv.csv")
ic2012 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2012_rv.csv")
ic2013 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/ic2013_rv.csv")
ic2014 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Admissions and SAT/adm2014_rv.csv")

UNITID = c(
  232186,
  134097,
  139940,
  151351,
  151111,
  171100,
  199193,
  106397,
  201885,
  129020,
  134130,
  225511,
  145637,
  153658,
  155317,
  157085,
  157289,
  163286,
  166629,
  220862,
  174066,
  178402,
  181464,
  182281,
  182290,
  199120,
  207500,
  217484,
  218663,
  137351,
  221759,
  230764,
  240453,
  172644,
  172699
)
unitids <- UNITID
# Combine datasets into a list
data_list <- list(
  ic2004, ic2005, ic2006, ic2007,
  ic2008, ic2009, ic2010, ic2011,
  ic2012, ic2013, ic2014)

# Add the corresponding year to each dataset
years <- 2004:2014

for (i in seq_along(data_list)) {
  data_list[[i]]$year <- years[i]
}

# Variables needed for admissions and SAT
variables <- c("UNITID","SATVR25","SATMT25","ACTCM25","APPLCN","ADMSSN","ENRLFT","ENRLPT")

# Select variables and combine years
admissions_data <- bind_rows(
  lapply(data_list, function(df) { df %>%
      select(year, any_of(variables)) %>%
      mutate(
        across(
          -year,
          ~ suppressWarnings(as.numeric(as.character(.)))))}))

# Check results
str(admissions_data)
dim(admissions_data)
head(admissions_data)
#View(admissions_data)



###########################################################################################
#calculate Average SAT using the 25th-percentile SAT Verbal and Math scores
############################################################################################

#Select the 35 universities
admissions_35 <- admissions_data %>%
  filter(UNITID %in% unitids) %>%
  arrange(UNITID, year)

#Convert SAT and ACT variables to numeric
variables <- c("SATVR25", "SATMT25", "ACTCM25")

stopifnot(all(variables %in% names(admissions_35)))

admissions_35 <- admissions_35 %>% mutate(across(all_of(variables), ~ suppressWarnings(as.numeric(as.character(.)))))

# STEP 4: Calculate SAT 25th-percentile combined score
admissions_35 <- admissions_35 %>%
  mutate(SAT_observed = if_else(SATVR25 >= 200 & SATVR25 <= 800 & SATMT25 >= 200 & SATMT25 <= 800,
      SATVR25 + SATMT25,
      NA_real_))

# STEP 5: ACT-to-SAT concordance table (historical)
act_to_sat <- c(
  "11" = 500,  "12" = 560,  "13" = 620,
  "14" = 680,  "15" = 740,  "16" = 780,
  "17" = 830,  "18" = 870,  "19" = 910,
  "20" = 950,  "21" = 990,  "22" = 1030,
  "23" = 1070, "24" = 1110, "25" = 1140,
  "26" = 1180, "27" = 1220, "28" = 1260,
  "29" = 1300, "30" = 1340, "31" = 1380,
  "32" = 1420, "33" = 1470, "34" = 1520,
  "35" = 1580, "36" = 1600)

# STEP 6: Convert ACT to its SAT equivalent
admissions_35 <- admissions_35 %>%
  mutate(
    SAT_from_ACT = unname(
      act_to_sat[as.character(ACTCM25)]
    ),
    
    # Use original SAT first; ACT only if SAT is missing
    SAT = coalesce(SAT_observed, SAT_from_ACT)
  )

# STEP 7: Check SAT scores
admissions_35 %>%
  select(UNITID, year, SATVR25, SATMT25,
         ACTCM25, SAT) %>%
  head(20) %>%
  print()

# STEP 8: Check remaining missing SAT values
sum(is.na(admissions_35$SAT))

# Count how many missing SAT values were replaced by ACT
sum(is.na(admissions_35$SAT_observed) & !is.na(admissions_35$SAT_from_ACT))
#View(admissions_35)

###########################################################################################
#Check GMU's GMU's Average_SAT, %_Admitted, and % Yield in 2004-2006 (Pre treatment)
############################################################################################
GMU_averages_SAT_Adm_Yield <- admissions_35 %>%
  filter(UNITID == 232186, year %in% 2004:2006) %>%
  mutate(
    pct_admitted = ADMSSN / APPLCN,
    pct_yield = (ENRLFT + ENRLPT) / ADMSSN
  ) %>%
  summarise(
    Average_SAT = mean(SAT, na.rm = TRUE),
    Percent_Admitted = mean(pct_admitted, na.rm = TRUE),
    Percent_Yield = mean(pct_yield, na.rm = TRUE)
  )

print(GMU_averages_SAT_Adm_Yield)
summary(admissions_35 )

##################Total Price################################
ic2004ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2004_ay.csv")
ic2005ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2005_ay.csv")
ic2006ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2006_ay.csv")
ic2007ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2007_ay.csv")
ic2008ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2008_ay.csv")
ic2009ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2009_ay.csv")
ic2010ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2010_ay.csv")
ic2011ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2011_ay.csv")
ic2012ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2012_ay.csv")
ic2013ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2013_ay.csv")
ic2014ay <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Institutional Charactoristics /ic2014_ay.csv")


#View(ic2004ay)
head(ic2004ay$CINSON)
years <- 2004:2014
#price_vars <- c("CHG2AY3", "CHG4AY3", "CHG5AY3", "CHG6AY3")


# Combine datasets into a list
data_list <- list(
  ic2004ay, ic2005ay, ic2006ay, ic2007ay,
  ic2008ay, ic2009ay, ic2010ay, ic2011ay,
  ic2012ay, ic2013ay, ic2014ay)


for (i in seq_along(data_list)) {
  data_list[[i]]$year <- years[i]
}

# Variables needed 
variables <- c("UNITID","CINSON")

# Select variables and combine years
institution_data <- bind_rows(
  lapply(data_list, function(df) { df %>%
      select(year, any_of(variables)) %>%
      mutate(
        across(
          -year,
          ~ suppressWarnings(as.numeric(as.character(.)))))}))

# Check results
str(institution_data)

#View(institution_data)

# Filter the 35 institutions
institution_35 <- institution_data %>%
  filter(UNITID %in% unitids) %>%
  arrange(UNITID, year)

# Check results
n_distinct(institution_35$UNITID)

# View dataframe
#View(institution_35)

institution_35 <- institution_35 %>%
  mutate(
    Total_Price = CINSON
  )

#View(institution_35)

GMU_avg_Total_price <-  institution_35 %>%
  filter(
    UNITID == 232186,
    year >= 2004,
    year <= 2006
  ) %>%
  summarise(
    Average_Total_Price = mean(
      Total_Price,
      na.rm = TRUE
    )
  )

print(GMU_avg_Total_price)



#Read Fall Enrollment Dataset for underrepresented Minority ratio
ef2004 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2004a_rv.csv")
ef2005 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2005a_rv.csv")
ef2006 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2006a_rv.csv")
ef2007 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2007a_rv.csv")
ef2008 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2008a_rv.csv")
ef2009 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2009a_rv.csv")
ef2010 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2010a_rv.csv")
ef2011 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2011a_rv.csv")
ef2012 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2012a_rv.csv")
ef2013 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2013a_rv.csv")
ef2014 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2014a_rv.csv")
ef2015 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Fall Enrollment — Race and ethnicity (EF)/ef2015a_rv.csv")
names(ef2014)
str(ef2014)
library(dplyr)
library(dplyr)

years <- 2004:2014

ef_list <- list(
  ef2004, ef2005, ef2006, ef2007,
  ef2008, ef2009, ef2010, ef2011,
  ef2012, ef2013, ef2014
)

for (i in seq_along(ef_list)) {
  ef_list[[i]]$year <- years[i]
}

variables <- c(
  "UNITID", "EFALEVEL",
  "EFRACE18", "EFRACE19", "EFRACE21", "EFRACE24",
  "EFBKAAT", "EFAIANT", "EFHISPT", "EFNHPIT",
  "EFTOTLT"
)

ef_data <- bind_rows(
  lapply(ef_list, function(df) {
    df %>%
      select(year, any_of(variables)) %>%
      mutate(
        across(
          -year,
          ~ suppressWarnings(as.numeric(as.character(.)))
        )
      )
  })
)

library(dplyr)

# Function to retrieve numeric IPEDS variables
get_num <- function(data, variable) {
  
  if (!variable %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }
  
  x <- suppressWarnings(
    as.numeric(as.character(data[[variable]]))
  )
  
  x[x < 0] <- NA_real_
  
  return(x)
}

# Function to combine historical and modern counts
sum_reported <- function(old, new) {
  if_else(
    is.na(old) & is.na(new),
    NA_real_,
    coalesce(old, 0) + coalesce(new, 0)
  )
}

# STEP 1: Select the correct student cohort
ef_cohort <- ef_35 %>%
  filter(EFALEVEL == 4) %>%
  arrange(UNITID, year)

# Verify the cohort
stopifnot(
  nrow(ef_cohort) == 385,
  all(ef_cohort$EFALEVEL == 4),
  !anyDuplicated(ef_cohort[c("UNITID", "year")])
)

# STEP 2: Extract racial enrollment variables
ef_race <- ef_cohort %>%
  transmute(
    UNITID,
    year,
    EFALEVEL,
    
    # Historical variables
    EFRACE18 = get_num(ef_cohort, "EFRACE18"),
    EFRACE19 = get_num(ef_cohort, "EFRACE19"),
    EFRACE21 = get_num(ef_cohort, "EFRACE21"),
    EFRACE24 = get_num(ef_cohort, "EFRACE24"),
    
    # Modern variables
    EFBKAAT = get_num(ef_cohort, "EFBKAAT"),
    EFAIANT = get_num(ef_cohort, "EFAIANT"),
    EFHISPT = get_num(ef_cohort, "EFHISPT"),
    EFNHPIT = get_num(ef_cohort, "EFNHPIT"),
    EFTOTLT = get_num(ef_cohort, "EFTOTLT")
  )

# STEP 3: Harmonize historical and modern racial categories
ef_35_urm <- ef_race %>%
  mutate(
    
    reporting_system = case_when(
      year <= 2007 ~ "Historical",
      year %in% 2008:2009 ~ "Transition",
      year >= 2010 ~ "Modern"
    ),
    
    # Black students
    black = case_when(
      year <= 2007 ~ EFRACE18,
      year %in% 2008:2009 ~ sum_reported(EFRACE18, EFBKAAT),
      year >= 2010 ~ EFBKAAT
    ),
    
    # Hispanic students
    hispanic = case_when(
      year <= 2007 ~ EFRACE21,
      year %in% 2008:2009 ~ sum_reported(EFRACE21, EFHISPT),
      year >= 2010 ~ EFHISPT
    ),
    
    # Native American students
    native_american = case_when(
      year <= 2007 ~ EFRACE19,
      year %in% 2008:2009 ~ sum_reported(EFRACE19, EFAIANT),
      year >= 2010 ~ EFAIANT
    ),
    
    pacific_islander = EFNHPIT,
    
    # Total first-year cohort
    total_cohort = coalesce(
      if_else(EFTOTLT > 0, EFTOTLT, NA_real_),
      if_else(EFRACE24 > 0, EFRACE24, NA_real_)
    ),
    
    # Calculate enrollment shares
    black_share = black / total_cohort,
    hispanic_share = hispanic / total_cohort,
    
    # Three-group URM
    urm_count_3groups = black + hispanic + native_american,
    
    urm_share_3groups = urm_count_3groups / total_cohort,
    
    # Alternative URM definition
    urm_count = case_when(
      year <= 2007 ~ urm_count_3groups,
      year %in% 2008:2009 &
        is.na(EFBKAAT) &
        is.na(EFHISPT) &
        is.na(EFAIANT) &
        is.na(EFNHPIT) ~ urm_count_3groups,
      TRUE ~ urm_count_3groups + pacific_islander
    ),
    
    urm_share = urm_count / total_cohort
    
  ) %>%
  arrange(UNITID, year)

# STEP 4: Check results
str(ef_35_urm)
summary(ef_35_urm)
#View(ef_35_urm)











# Extract first-year cohort (full-time and part-time)
first_year_35 <- ef_35 %>%
  mutate(
    UNITID = as.numeric(as.character(UNITID)),
    year = as.integer(year),
    EFALEVEL = as.numeric(as.character(EFALEVEL)),
    
    first_year_cohort = coalesce(
      get_num(ef_35, "EFTOTLT"),
      get_num(ef_35, "EFRACE24")
    )
  ) %>%
  filter(EFALEVEL == 4) %>%
  select(
    UNITID,
    year,
    first_year_cohort
  ) %>%
  arrange(UNITID, year)

# Check results
#View(first_year_35)
str(first_year_35$first_year_cohort)
# Verify one observation per institution-year
stopifnot(
  !anyDuplicated(first_year_35[c("UNITID", "year")])
)

GMU_First_Year <- first_year_35 %>%
  filter(
    UNITID == 232186,
    year %in% 2004:2006
  ) %>%
  select(
    year,
    first_year_cohort
  )

print(GMU_First_Year)

GMU_avg_First_Year <- mean(GMU_First_Year$first_year_cohort)

GMU_avg_First_Year



####################State Appropriation #####################
f0304 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0304_f1a.csv")
f0405 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0405_f1a.csv")
f0506 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0506_f1a.csv")
f0607 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0607_f1a.csv")
f0708 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0708_f1a.csv")
f0809 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0809_f1a.csv")
f0910 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f0910_f1a.csv")
f1011 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f1011_f1a.csv")
f1112 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f1112_f1a.csv")
f1213 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f1213_f1a.csv")
f1314 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f1314_f1a.csv")
f1415 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Public finance/f1415_f1a.csv")
View(f1415)


efia2004 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2004.csv")
efia2005 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2005.csv")
efia2006 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2006.csv")
efia2007 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2007.csv")
efia2008 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2008.csv")
efia2009 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2009.csv")
efia2010 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2010.csv")
efia2011 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2011.csv")
efia2012 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2012.csv")
efia2013 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2013.csv")
efia2014 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2014.csv")
efia2015 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Predictor variables — Admissions, costs, and institutional finance/Instructional activity/efia2015.csv")
names(efia2014 )
years <- 2004:2014
finance_list <- list(
f0405, f0506, f0607,
  f0708, f0809, f0910, f1011,
  f1112, f1213, f1314
)

# Select necessary variables and add year
finance_data <- bind_rows(
  lapply(seq_along(finance_list), function(i) {
    
    df <- finance_list[[i]]
    
    df %>%
      select(UNITID, F1B11) %>%
      mutate(
        year = years[i],
        UNITID = as.numeric(UNITID),
        F1B11 = as.numeric(F1B11)
      )
  })
)

# Keep 35 institutions
finance_35 <- finance_data %>%
  filter(UNITID %in% unitids) %>%
  rename(state_appropriations = F1B11) %>%
  arrange(UNITID, year)

#View(finance_35)

efia_list <- list(
  efia2004, efia2005, efia2006, efia2007,
  efia2008, efia2009, efia2010, efia2011,
  efia2012, efia2013, efia2014)

names(efia2006)

for (i in seq_along(efia_list)) {
  efia_list[[i]]$year <- years[i]
}

# Variables needed 
variables <- c("UNITID","EFTEUG")

# Select variables and combine years
efia_data <- bind_rows(
  lapply(efia_list, function(df) {
    df %>%
      select(year, any_of(variables)) %>%
      mutate(
        across(
          -year,
          ~ suppressWarnings(as.numeric(as.character(.)))
        )
      )
  })
) %>%
  rename(total_fte = EFTEUG)

str(efia_data)

# Filter the 35 institutions
efia_35 <- efia_data %>%
  filter(UNITID %in% unitids) %>%
  arrange(UNITID, year)

str(efia_35)

# Merge datasets
state_appropriations_35 <- finance_35 %>%
  left_join(
    efia_35,
    by = c("UNITID", "year")
  ) %>%
  mutate(
    state_appropriations_per_fte = if_else(
      total_fte > 0 & state_appropriations >= 0,
      state_appropriations / total_fte,
      NA_real_
    )
  ) %>%
  select(
    UNITID,
    year,
    state_appropriations,
    total_fte,
    state_appropriations_per_fte
  ) %>%
  arrange(UNITID, year)

str(state_appropriations_35)

GMU_appropriations <- state_appropriations_35 %>%
  filter(
    UNITID == 232186,
    year %in% 2004:2006
  )


GMU_avg_appropriations <- mean(
  GMU_appropriations$state_appropriations_per_fte
)
GMU_avg_appropriations



#################Institutional Grant################
sfa0304 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0304.csv")
sfa0405 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0405.csv")
sfa0506 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0506.csv")
sfa0607 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0607.csv")
sfa0708 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0708.csv")
sfa0809 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0809.csv")
sfa0910 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa0910.csv")
sfa1011 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa1011.csv")
sfa1112 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa1112.csv")
sfa1213 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa1213.csv")
sfa1314 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa1314.csv")
sfa1415 <- read.csv("/Users/RekaFujii4/Desktop/Replication files/Financial Aid — Pell Grants and institutional grants (SFA)/sfa1415.csv")
names(sfa1314)
sfa_list <- list(
  sfa0405, sfa0506, sfa0607,
  sfa0708, sfa0809, sfa0910, sfa1011,
  sfa1112, sfa1213, sfa1314, sfa1415
)

years <- 2004:2015
pell_variables <- c(
  "UPGRNTP", "UPGRNTN", "SCUGRAD",
  "PGRNT_P", "PGRNT_N", "SCUGFFN",
  "FGRNT_P",
  
  # Institutional grant aid variables
  "IGRNT_N",
  "IGRNT_P",
  "IGRNT_T",
  "IGRNT_A"
)

years <- 2004:2014

stopifnot(length(sfa_list) == length(years))

sfa_data <- bind_rows(
  lapply(seq_along(sfa_list), function(i) {
    
    sfa_list[[i]] %>%
      select(UNITID, any_of(pell_variables)) %>%
      mutate(
        year = years[i],
        across(
          everything(),
          ~ suppressWarnings(as.numeric(as.character(.)))
        )
      )
  })
)

names(sfa_data)

sfa_35 <- sfa_data %>%
  mutate(
    across(
      -c(UNITID, year),
      ~ if_else(.x >= 0, .x, NA_real_)
    )
  ) %>%
  mutate(
    pell_share = case_when(
      year %in% 2004:2008 ~ FGRNT_P / 100,
      year %in% 2009:2014 ~ UPGRNTP / 100,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(UNITID %in% unitids) %>%
  arrange(UNITID, year)

names(sfa_35)
str(sfa_35)

instgrant_35 <- sfa_35 %>%
  transmute(
    UNITID,
    year,
    institutional_grant_recipients = IGRNT_N,
    institutional_grant_share = IGRNT_P / 100,
    institutional_grant_total = IGRNT_T,
    `avg. instgrant` = IGRNT_A
  ) %>%
  arrange(UNITID, year)

str(instgrant_35)
summary(instgrant_35)
#View(instgrant_35)

##########Combine all the datasets################

# Create a balanced panel dataset
cleaned_data <- expand_grid(
  UNITID = unitids,
  year = 2004:2014
)

# Merge all datasets
cleaned_data <- cleaned_data %>%
  left_join(admissions_35,
            by = c("UNITID", "year")) %>%
  left_join(institution_35,
            by = c("UNITID", "year")) %>%
  left_join(ef_35_urm,
            by = c("UNITID", "year")) %>%
  left_join(first_year_35,
            by = c("UNITID", "year")) %>%
  left_join(finance_35,
            by = c("UNITID", "year")) %>%
  left_join(efia_35,
            by = c("UNITID", "year")) %>%
  left_join(
    state_appropriations_35 %>%
      select(UNITID, year, state_appropriations_per_fte),
    by = c("UNITID", "year")
  ) %>%
  arrange(UNITID, year)

# Calculate additional predictor variables
cleaned_data <- cleaned_data %>%
  mutate(
    pct_admitted = if_else(
      APPLCN > 0,
      ADMSSN / APPLCN,
      NA_real_
    ),
    
    pct_yield = if_else(
      ADMSSN > 0,
      (ENRLFT + ENRLPT) / ADMSSN,
      NA_real_
    )
  )

# Check the merged dataset
str(cleaned_data)
dim(cleaned_data)
summary(cleaned_data)
#View(cleaned_data)

institution_lookup <- tibble(
  UNITID = c(
    232186,134097,139940,151351,151111,171100,199193,
    106397,201885,129020,134130,225511,145637,153658,
    155317,157085,157289,163286,166629,220862,174066,
    178402,181464,182281,182290,199120,207500,217484,
    218663,137351,221759,230764,240453,172644,172699
  ),
  institution_name = c(
    "George Mason University",
    "Florida State University",
    "Georgia State University",
    "Indiana University-Bloomington",
    "Indiana University-Purdue University Indianapolis",
    "Michigan State University",
    "North Carolina State University",
    "University of Arkansas",
    "University of Cincinnati",
    "University of Connecticut",
    "University of Florida",
    "University of Houston",
    "University of Illinois Urbana-Champaign",
    "University of Iowa",
    "University of Kansas",
    "University of Kentucky",
    "University of Louisville",
    "University of Maryland-College Park",
    "University of Massachusetts Amherst",
    "University of Memphis",
    "University of Minnesota-Twin Cities",
    "University of Missouri-Kansas City",
    "University of Nebraska-Lincoln",
    "University of Nevada-Las Vegas",
    "University of Nevada-Reno",
    "University of North Carolina at Chapel Hill",
    "University of Oklahoma",
    "University of Rhode Island",
    "University of South Carolina",
    "University of South Florida",
    "University of Tennessee-Knoxville",
    "University of Utah",
    "University of Wisconsin-Milwaukee",
    "Wayne State University",
    "Western Michigan University"
  )
)


# --------------------------------------------------
# STEP 1: Verify the corrected URM dataset
# --------------------------------------------------

stopifnot(
  nrow(ef_35_urm) == 385,
  all(ef_35_urm$EFALEVEL == 4),
  !anyDuplicated(ef_35_urm[c("UNITID", "year")])
)

# Check missing racial enrollment outcomes
colSums(is.na(
  ef_35_urm %>%
    select(black_share, hispanic_share, urm_share)
))


# --------------------------------------------------
# STEP 2: Extract full-time fall enrollment
# --------------------------------------------------

ftenrollment_35 <- ef_35 %>%
  mutate(
    Ftenrollment = coalesce(
      get_num(ef_35, "EFTOTLT"),
      get_num(ef_35, "EFRACE24")
    )
  ) %>%
  filter(EFALEVEL == 21) %>%
  select(UNITID, year, Ftenrollment) %>%
  arrange(UNITID, year)

stopifnot(
  nrow(ftenrollment_35) == 385,
  !anyDuplicated(ftenrollment_35[c("UNITID", "year")])
)


# --------------------------------------------------
# STEP 3: Create a balanced panel dataset
# --------------------------------------------------

cleaned_data <- expand_grid(
  UNITID = unitids,
  year = 2004:2014
)


# --------------------------------------------------
# STEP 4: Merge all datasets
# --------------------------------------------------

cleaned_data <- cleaned_data %>%
  
  left_join(
    admissions_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    institution_35,
    by = c("UNITID", "year")
  ) %>%
  
  # Corrected racial enrollment dataset
  left_join(
    ef_35_urm,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    first_year_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    finance_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    efia_35,
    by = c("UNITID", "year")
  ) %>%
  
  # Corrected full-time fall enrollment
  left_join(
    ftenrollment_35,
    by = c("UNITID", "year")
  ) %>%
  
  arrange(UNITID, year)


# --------------------------------------------------
# STEP 5: Calculate predictor variables
# --------------------------------------------------

cleaned_data <- cleaned_data %>%
  mutate(
    
    # Admission rate
    pct_admitted = if_else(
      APPLCN > 0,
      ADMSSN / APPLCN,
      NA_real_
    ),
    
    # Yield rate
    pct_yield = if_else(
      ADMSSN > 0,
      (ENRLFT + ENRLPT) / ADMSSN,
      NA_real_
    ),
    
    # State appropriations per full-time student
    state_appropriations_per_fte = if_else(
      Ftenrollment > 0 & state_appropriations >= 0,
      state_appropriations / Ftenrollment,
      NA_real_
    )
  )

# Check the merged dataset
str(cleaned_data)
dim(cleaned_data)
summary(cleaned_data)


# --------------------------------------------------
# STEP 6: Create final_data
# --------------------------------------------------

final_data <- cleaned_data %>%
  select(-any_of(c("institution", "institution_name"))) %>%
  left_join(
    institution_lookup,
    by = "UNITID"
  ) %>%
  transmute(
    institution = institution_name,
    UNITID,
    year,
    
    # Predictor variables
    Ftenrollment,
    applications = APPLCN,
    admitted = pct_admitted,
    yield = pct_yield,
    appropriations = state_appropriations,
    
    `appropriations per FTE` = 
      state_appropriations_per_fte,
    
    `appropriations by 1000` = 
      state_appropriations / 1000,
    
    price = Total_Price,
    SAT,
    firstyearclass = first_year_cohort,
    
    # Corrected outcome variables
    perhisp = hispanic_share,
    perblack = black_share,
    perums = urm_share
    
  ) %>%
  arrange(UNITID, year)


# --------------------------------------------------
# STEP 7: Verify final_data
# --------------------------------------------------

str(final_data)
dim(final_data)
summary(final_data)

#View(final_data)
cleaned_data <- expand_grid(
  UNITID = unitids,
  year = 2004:2014
)

# Import Pell recipient data
master_OPEID <- read_xlsx("/Users/RekaFujii4/Desktop/Replication Presentation/Pell_Recipients_35_Universities_2004_2014_corrected_names.xlsx")

# Select necessary variables
pell_recipients <- master_OPEID %>%
  transmute(
    institution = University,
    year = as.integer(Year),
    recipients = as.numeric(Recipients)
  )


# Add Pell recipients and calculate Pell share
final_data <- final_data %>%
  select(-any_of(c("pell", "recipients"))) %>%
  left_join(
    pell_recipients,
    by = c("institution", "year")
  ) %>%
  mutate(
    pell = if_else(
      Ftenrollment > 0,
      recipients / Ftenrollment,
      NA_real_
    )
  ) %>%
  relocate(pell, recipients, .after = year) %>%
  arrange(UNITID, year)

# Check results
str(final_data)
dim(final_data)
summary(final_data)

GMU_avg_Pell <- final_data %>%
  filter(
    UNITID == 232186,
    year %in% 2004:2006
  ) %>%
  summarise(
    Average_Pell = mean(pell)
  )

print(GMU_avg_Pell)


cleaned_data <- cleaned_data %>%
  
  left_join(
    admissions_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    institution_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    ef_35_urm,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    first_year_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    finance_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    efia_35,
    by = c("UNITID", "year")
  ) %>%
  
  left_join(
    ftenrollment_35,
    by = c("UNITID", "year")
  ) %>%
  
  # NEW: Institutional grant aid
  left_join(
    instgrant_35,
    by = c("UNITID", "year")
  ) %>%
  
  arrange(UNITID, year)

# Verify the merged dataset
str(cleaned_data)
dim(cleaned_data)
summary(cleaned_data)

cleaned_data <- cleaned_data %>%
  mutate(
    
    # Admission rate
    pct_admitted = if_else(
      APPLCN > 0,
      ADMSSN / APPLCN,
      NA_real_
    ),
    
    # Yield rate
    pct_yield = if_else(
      ADMSSN > 0,
      (ENRLFT + ENRLPT) / ADMSSN,
      NA_real_
    ),
    
    # State appropriations per full-time student
    state_appropriations_per_fte = if_else(
      Ftenrollment > 0 & state_appropriations >= 0,
      state_appropriations / Ftenrollment,
      NA_real_
    )
  )

final_data <- cleaned_data %>%
  select(-any_of(c("institution", "institution_name"))) %>%
  left_join(
    institution_lookup,
    by = "UNITID"
  ) %>%
  transmute(
    institution = institution_name,
    UNITID,
    year,
    
    # Predictor variables
    Ftenrollment,
    applications = APPLCN,
    admitted = pct_admitted,
    yield = pct_yield,
    appropriations = state_appropriations,
    
    `appropriations per FTE` =
      state_appropriations_per_fte,
    
    `appropriations by 1000` =
      state_appropriations / 1000,
    
    price = Total_Price,
    SAT,
    
    # NEW: Institutional grant aid
    `avg. instgrant`,
    
    firstyearclass = first_year_cohort,
    
    # Outcome variables
    perhisp = hispanic_share,
    perblack = black_share,
    perums = urm_share
    
  ) %>%
  arrange(UNITID, year)

str(final_data)
dim(final_data)
summary(final_data)

# Export the final cleaned dataset
write.csv(
  final_data,
  file = "/Users/RekaFujii4/Desktop/Replication Presentation/cleaned_data.csv",
  row.names = FALSE,
  na = ""
)