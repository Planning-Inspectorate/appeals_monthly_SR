# ==============================================================================
# PINS_monthly_SR_workings.R
# ------------------------------------------------------------------------------
# script to do background workings for PINS_monthly_statistical_release.Rmd,
# which produces Rmarkdown document/html
# ------------------------------------------------------------------------------
#
# version control:
# ----------------
# Jo Gerulaitis, 30/08/2022, initial script
#
# ==============================================================================

# read in libraries
# (may need to be installed using install.packages() first time this script is run)
library(data.table)
library(dplyr)
library(janitor)
library(lubridate)
library(hash)

# ==============================================================================
# USER INPUTS (FOR TESTING/DEVELOPMENT ONLY, COMMENT OUT IF RUNNING rmd)
# ==============================================================================

# # date statistical release will be published
# publication_date <- "21/07/2022"
# appeals_data <- "C:/Users/GerulaitisJoanna/OneDrive - Planning Inspectorate/Admin/Data_Analysis/to_june_2022_appeals.csv"

# ==============================================================================

# convert publication_date to date format
publ_date <- as.Date(publication_date, "%d/%m/%Y")

# derive end of reporting period from publication date
# (first date of publ_date month, -1 day to make last day of previous month)
end_date <- floor_date(publ_date, "month") - days(1)

# derive start of reporting period (assumes one year reporting period)
start_date <- floor_date(publ_date, "month") - years(1)

# derive latest month/year in format Month Year (e.g. June 2022)
end_m_y <- format(as.Date(publication_date, "%d/%m/%Y") - months(1), "%B %Y")

# derive previous (latest - 1) month/year in format Month Year (e.g. May 2022)
prev_m_y <- format(as.Date(publication_date, "%d/%m/%Y") - months(2), "%B %Y")

# derive start month/year in format Month Year (e.g. July 2021)
start_m_y <- format(as.Date(publication_date, "%d/%m/%Y") - years(1), "%B %Y")

# read in appeals data and convert column names to lower case / remove special characters and spaces
appeals <- fread(appeals_data) %>%
  
  # convert column names to lower case and replace spaces/special characters with underscores
  clean_names() %>%
  
  # filter out invalid appeals (those with negative valid_to_decision_weeks or no procedure type)
  filter(!is.na(procedure_code) & procedure_code != "" & valid_to_decision_weeks >= 0) %>%
  
  # make sure decision dates are in date format (not character/string)
  mutate(decision_date = as.Date(decision_date, format = "%d/%m/%Y"))

# ------------------------------------------------------------------------------
# time in weeks to decision
# ------------------------------------------------------------------------------

# extract data needed for time to decide cases table
time_to_decide_cases <- appeals %>%
  select(case_number, decision_date, procedure_code, valid_to_decision_weeks) 

# latest year
# ------------------------------------------------------------------------------

# filter to cases decided in latest year
time_to_decide_cases_latest_year <- time_to_decide_cases %>%
  filter(decision_date <= end_date & decision_date >= start_date)

# take median by procedure_code
time_to_decide_cases_latest_year_median <- time_to_decide_cases_latest_year %>%
  group_by(procedure_code) %>%
  summarise(median_decision_time_wks = median(valid_to_decision_weeks))

# append median for all cases
time_to_decide_cases_latest_year_median_tot <- c("all_cases", median(time_to_decide_cases_latest_year$valid_to_decision_weeks))

# combine median by case and median for all cases
time_to_decide_cases_latest_year_median_comb <- rbind(time_to_decide_cases_latest_year_median, time_to_decide_cases_latest_year_median_tot)

# latest month
# ------------------------------------------------------------------------------

# filter to cases decided in latest month
time_to_decide_cases_latest_month <- time_to_decide_cases %>%
  
  # decision_date >= first day of month of end_date
  filter(decision_date >= floor_date(end_date, "month"))

# take median by procedure_code
time_to_decide_cases_latest_month_median <- time_to_decide_cases_latest_month %>%
  group_by(procedure_code) %>%
  summarise(median_decision_time_wks = median(valid_to_decision_weeks))

# create a row with the median for all cases
time_to_decide_cases_latest_month_median_tot <- c("all_cases", median(time_to_decide_cases_latest_month$valid_to_decision_weeks))

# combine median by case and median for all cases by row-binding them together
time_to_decide_cases_latest_month_median_comb <- rbind(time_to_decide_cases_latest_month_median, time_to_decide_cases_latest_month_median_tot)

# latest year and latest month combined
# ------------------------------------------------------------------------------

time_to_decide_cases_latest <- time_to_decide_cases_latest_year_median_comb %>%
  
  # join latest year and latest month medians into one table
  left_join(time_to_decide_cases_latest_month_median_comb, by = "procedure_code") %>%
  
  # rename columns for ease of reference
  rename(last_12_months = median_decision_time_wks.x, latest_month = median_decision_time_wks.y) %>%
  
  # create a column to allow to be sorted in the correct order (WR, HRG, INQ, total)
  mutate(sort_col = case_when(
    procedure_code == "WR" ~ 1,
    procedure_code == "HRG" ~ 2,
    procedure_code == "INQ" ~ 3,
    TRUE ~ 4
  )) %>%
  
  # sort by new sort_col
  arrange(sort_col) %>%
  
  # round decision time in weeks to nearest whole week
  mutate(across(c(last_12_months, latest_month), function(x) round_half_up(as.numeric(x)))) %>%
  
  # drop sort col now they are in correct order
  select(-sort_col)

# reformat table for publication
# ------------------------------------------------------------------------------

# rename procedure codes for publication
time_to_decide_cases_latest_table <- time_to_decide_cases_latest %>%
  mutate(procedure_code = case_when(
    procedure_code == "WR" ~ "Written Representations",
    procedure_code == "HRG" ~ "Hearings",
    procedure_code == "INQ" ~ "Inquiries",
    TRUE ~ "All Cases"
  )) %>%
  
  # append "weeks" to decision times
  mutate(last_12_months = paste0(last_12_months, " weeks"), latest_month = paste0(latest_month, " weeks")) %>%
  
  # rename columns for publication
  rename("Procedure type" = procedure_code, "Last 12 Months" = last_12_months, "{end_m_y}" := latest_month)
 

# ------------------------------------------------------------------------------
# decisions by month
# ------------------------------------------------------------------------------

decisions_by_month <- appeals %>%
  mutate(month = month(decision_date)) %>%
  group_by(month) %>%
  summarise(decision_count = n(), decision_time = round_half_up(median(valid_to_decision_weeks)))

# get median decision time for end of period month
end_m_median_decision_time <- decisions_by_month %>%
  filter(month == month(end_date)) %>%
  select(decision_time) %>%
  as.numeric()

# get median decision time for end of period month - 1 (i.e. previous month)
prev_m_median_decision_time <- decisions_by_month %>%
  filter(month == (month(end_date) - 1)) %>%
  select(decision_time) %>%
  as.numeric()

# difference between latest and previous month median decision time
month_change_decision_time <- end_m_median_decision_time - prev_m_median_decision_time
month_change_decision_time_text <- case_when(
  month_change_decision_time > 1 ~ paste0(abs(month_change_decision_time), " weeks higher than "),
  month_change_decision_time < -1 ~ paste0(abs(month_change_decision_time), " weeks lower than "),
  month_change_decision_time > 0 ~ paste0(abs(month_change_decision_time), " week higher than "),
  month_change_decision_time < 0 ~ paste0(abs(month_change_decision_time), " week lower than "),
  TRUE ~ " unchanged since "
)

# get median decision time for start of period month
start_m_median_decision_time <- decisions_by_month %>%
  filter(month == month(start_date)) %>%
  select(decision_time) %>%
  as.numeric()

# get lowest median for past year
min_median_decision_time <- min(decisions_by_month$decision_time)

# get highest median for past year
max_median_decision_time <- max(decisions_by_month$decision_time)

# ------------------------------------------------------------------------------
# decisions by month and appeal category (Planning, Enforcement, Specialist)
# ------------------------------------------------------------------------------

# decisions by appeal type and month
decisions_by_appeal_type_month <- appeals %>%
  mutate(month = month(decision_date)) %>%
  group_by(appeal_type_category, month) %>%
  summarise(decision_count = n(), decision_time = median(valid_to_decision_weeks))

decisions_by_appeal_type_month_planning <- decisions_by_appeal_type_month %>% filter(appeal_type_category == "Planning")
decisions_by_appeal_type_month_enforcement <- decisions_by_appeal_type_month %>% filter(appeal_type_category == "Enforcement")
decisions_by_appeal_type_month_specialist <- decisions_by_appeal_type_month %>% filter(appeal_type_category == "Specialist")

enforcement_vs_specialist <- decisions_by_appeal_type_month_enforcement %>%
  left_join(decisions_by_appeal_type_month_specialist, by = "month") %>%
  mutate(enforce_longer_specialist = ifelse(decision_time.x > decision_time.y, 1, 0))

number_of_months_enforcement_longer_specialist <- sum(enforcement_vs_specialist$enforce_longer_specialist)

decisions_by_appeal_type <- appeals %>%
  group_by(appeal_type_category) %>%
  summarise(decision_time = median(valid_to_decision_weeks))
  
median_decision_time_planning <- round_half_up(decisions_by_appeal_type %>% filter(appeal_type_category == "Planning") %>% select(decision_time) %>% as.numeric())
median_decision_time_enforcement <- round_half_up(decisions_by_appeal_type %>% filter(appeal_type_category == "Enforcement") %>% select(decision_time) %>% as.numeric())
median_decision_time_specialist <- round_half_up(decisions_by_appeal_type %>% filter(appeal_type_category == "Specialist") %>% select(decision_time) %>% as.numeric())

median_decision_time_planning_month <- decisions_by_appeal_type_month %>% filter(appeal_type_category == "Planning") %>% select(decision_time)
min_median_decision_time_planning <- min(median_decision_time_planning_month$decision_time)
max_median_decision_time_planning <- max(median_decision_time_planning_month$decision_time)

decisions_inquiry_planning_rosewell <- appeals %>% filter(is_rosewell == 1, procedure_code == "INQ", appeal_type_category == "Planning") %>% select(valid_to_decision_weeks)

decisions_inquiry <- appeals %>% filter(procedure_code == "INQ") %>%
  group_by(is_rosewell, appeal_type_category) %>%
  summarise(decision_time = median(valid_to_decision_weeks))

decisions_inquiry_planning_rosewell_median <- round_half_up(median(decisions_inquiry_planning_rosewell$valid_to_decision_weeks))
decisions_inquiry_min_type <- decisions_inquiry[which(decisions_inquiry$decision_time == min(decisions_inquiry$decision_time)),]

# decisions by month
decisions_count <- appeals %>%
  mutate(month = month(decision_date)) %>%
  group_by(month) %>%
  summarise(decision_count = n())

# total decisions in year
decisions_in_year <- sum(decisions_count$decision_count)

# decisions in latest month
decisions_in_latest_month <- decisions_count %>%
  filter(month == month(end_date)) %>%
  select(decision_count) %>%
  as.numeric()

# arrange months by number of decisions in that month (ascending)
decisions_in_latest_month_sorted <- decisions_count %>%
  arrange(decision_count)

# get rank of latest month's number of decisions compared to last 12 months
decisions_in_latest_month_rank <- which(decisions_in_latest_month_sorted$decision_count == decisions_in_latest_month)

# create hash/dictionary to assign word description to this rank
decisions_in_latest_month_rank_result <- hash()

# populate hash
decisions_in_latest_month_rank_result[["1"]] <- "lowest"
decisions_in_latest_month_rank_result[["2"]] <- "second lowest"
decisions_in_latest_month_rank_result[["3"]] <- "third lowest"
decisions_in_latest_month_rank_result[["4"]] <- "fourth lowest"
decisions_in_latest_month_rank_result[["5"]] <- "fifth lowest"
decisions_in_latest_month_rank_result[["6"]] <- "sixth lowest"
decisions_in_latest_month_rank_result[["7"]] <- "sixth highest"
decisions_in_latest_month_rank_result[["8"]] <- "fifth highest"
decisions_in_latest_month_rank_result[["9"]] <- "fourth highest"
decisions_in_latest_month_rank_result[["10"]] <- "third highest"
decisions_in_latest_month_rank_result[["11"]] <- "second highest"
decisions_in_latest_month_rank_result[["12"]] <- "highest"

# decisions by month and procedure code
decisions_count_procedure_code <- appeals %>%
  mutate(month = month(decision_date)) %>%
  group_by(procedure_code, month) %>%
  summarise(decision_count = n())

decisions_count_written <- decisions_count_procedure_code %>% filter(procedure_code == "WR")
decisions_count_hearing <- decisions_count_procedure_code %>% filter(procedure_code == "HRG")
decisions_count_inquiry <- decisions_count_procedure_code %>% filter(procedure_code == "INQ")

# decision count by procedure code latest month
decisions_count_written_latest_month <- decisions_count_written %>% filter(month == month(end_date)) %>% ungroup %>% select(decision_count) %>% as.integer()
decisions_count_hearing_latest_month <- decisions_count_hearing %>% filter(month == month(end_date)) %>% ungroup %>% select(decision_count) %>% as.integer()
decisions_count_inquiry_latest_month <- decisions_count_inquiry %>% filter(month == month(end_date)) %>% ungroup %>% select(decision_count) %>% as.integer()

#decision count by procedure code last 12 months
decisions_count_written_year <- sum(decisions_count_written$decision_count)
decisions_count_hearing_year <- sum(decisions_count_hearing$decision_count)
decisions_count_inquiry_year <- sum(decisions_count_inquiry$decision_count)

decisions_count_inquiry_year_min <- min(decisions_count_inquiry$decision_count)
decisions_count_inquiry_year_max <- max(decisions_count_inquiry$decision_count)