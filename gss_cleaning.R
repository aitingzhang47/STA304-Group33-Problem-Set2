#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
  ## 1. Go to: http://www.chass.utoronto.ca/
  ## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
  ## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
  ## 4. Continue in English (you're welcome to use the French, but we probably can't
  ## help you too much).
  ## 5. Crtl F GSS, click
  ## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
  ## wave. In particular the General Social Survey on social identity (cycle 27), 
  ## 2013 has some variables on voter participation if you're into that sort of 
  ## thing. You're welcome to pick any year but this code applies to 2017.
  ## 7. Click download
  ## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
  ## 9. Can select all variables by clicking button next to green colored "All". Then continue.
  ## 10. Create the files, download and save
# Check: 
  ## You WILL need to change the raw data name. Search for .csv - line 41
  ## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAG0Ofp3.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID,
         agegr10,
         sex,
         marstat,
         hsdsizec,
         whw120gr,
         spa_01,
         iur_01,
         heh_01,
         smg_01,
         prv,
         srh_110) %>% 
  mutate_at(vars(agegr10:srh_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:srh_110),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agegr10,
         sex = sex,
         marital_status = marstat,
         household_size = hsdsizec,
         work_perweek = whw120gr,
         regular_sports = spa_01,
         frequency_technology = iur_01,
         eating_habits = heh_01,
         stress_level = smg_01,
         province = prv,
         health_condition=srh_110) 




#### Clean up ####
gss <- gss %>% 
  mutate_at(vars(age:health_condition), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) 

gss <- as.data.frame(apply(gss,2,function(x)gsub('\\s+', '',x)))

gss <- gss %>% 
  rowwise() %>% 
  mutate(health_condition = str_remove(string = health_condition, pattern = "\\ .*")) %>% 
  mutate(health_condition = case_when(
    health_condition=="Excellent" ~ 5,
    health_condition=="Verygood" ~ 4,
    health_condition=="Good" ~ 3,
    health_condition=="Fair" ~ 2,
    health_condition=="Poor" ~ 1,
  )) 

gss <- gss %>% 
  rowwise() %>% 
  mutate(eating_habits = str_remove(string = eating_habits, pattern = "\\ .*")) %>% 
  mutate(eating_habits = case_when(
    eating_habits=="Excellent" ~ 5,
    eating_habits=="Verygood" ~ 4,
    eating_habits=="Good" ~ 3,
    eating_habits=="Fair" ~ 2,
    eating_habits=="Poor" ~ 1,
  )) 


gss <- gss %>% 
  rowwise() %>% 
  mutate(stress_level = str_remove(string = stress_level, pattern = "\\ .*")) %>% 
  mutate(stress_level = case_when(
    stress_level=="Notatallstressful" ~ 5,
    stress_level=="Notverystressful" ~ 4,
    stress_level=="Abitstressful" ~ 3,
    stress_level=="Quiteabitstressful" ~ 2,
    stress_level=="Extremelystressful" ~ 1,
  )) 

gss <- gss %>% 
  rowwise() %>% 
  mutate(frequency_technology = str_remove(string = frequency_technology, pattern = "\\ .*")) %>% 
  mutate(frequency_technology = case_when(
    frequency_technology=="Daily" ~ 5,
    frequency_technology=="Afewtimesaweek" ~ 4,
    frequency_technology=="Onceaweek" ~ 3,
    frequency_technology=="Afewtimesamonth" ~ 2,
    frequency_technology=="Didnotuseinthepastmonth" ~ 1,
  )) 

gss <- gss %>% 
  mutate_at(vars(regular_sports), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  rowwise() %>% 
  mutate(work_perweek = str_remove(string = work_perweek, pattern = "\\ .*")) %>% 
  mutate(work_perweek = case_when(
    work_perweek=="Don'tknow" ~ as.numeric(NA),
    work_perweek=="41hoursandabove" ~ 4,
    work_perweek=="30to40hours" ~ 3,
    work_perweek=="16to29hours" ~ 2,
    work_perweek==">0to15hours" ~ 1
  )) 

gss <- gss %>% 
  rowwise() %>% 
  mutate(household_size = str_remove(string = household_size, pattern = "\\ .*")) %>% 
  mutate(household_size = case_when(
    household_size=="Sixormorepersonhousehold" ~ 6,
    household_size=="Fivepersonhousehold" ~ 5,
    household_size=="Fourpersonhousehold" ~ 4,
    household_size=="Threepersonhousehold" ~ 3,
    household_size=="Twopersonhousehold" ~ 2,
    household_size=="Onepersonhousehold" ~ 1
  )) 

gss <- gss %>% 
  mutate_at(vars(sex), .funs = funs(case_when(
    .=="Male"~1,
    .=="Female"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  rowwise() %>% 
  mutate(age = str_remove(string = age, pattern = "\\ .*")) %>% 
  mutate(age = case_when(
    age=="75yearsandover" ~ 7,
    age=="65to74years" ~ 6,
    age=="55to64years" ~ 5,
    age=="45to54years" ~ 4,
    age=="35to44years" ~ 3,
    age=="25to34years" ~ 2,
    age=="15to24years" ~ 1
  )) 

gss <- gss %>% select(caseid, age, sex, household_size, work_perweek, regular_sports, frequency_technology, eating_habits, stress_level, province, health_condition)

write_csv(gss, "gss.csv")






