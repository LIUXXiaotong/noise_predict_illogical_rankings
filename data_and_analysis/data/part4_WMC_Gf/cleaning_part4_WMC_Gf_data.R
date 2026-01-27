# R preparation ----------------
# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
} 
library("pacman")
p_load(tidyverse, jsonlite, here)

`%notin%` <- Negate(`%in%`)

##read in original .txt file and remove personally identifiable information
data0 <- read_file("raw_wmc_Gf_results.txt") |>  # Read the text file from JATOS ...
  str_split('\n') |>  first()  |>    # ... split it into lines ...
  discard(function(x) x == '') |>    # ... filter empty rows ...
  map_dfr(fromJSON, flatten=T)  |> 
  group_by(prolific_ID_url) |> 
  fill(prolific_ID, meta.userAgent, .direction = "updown") |> 
  ungroup() 

data0 <- data0 |>  
  group_by(prolific_ID) |>  
  mutate(count_rows = n()) |>  
  filter(count_rows > 1450 ) |> #exclude those who did not finish
  ungroup()

mapping <- read_csv("mapping.csv")
mapping$prolific_id <- as.character(mapping$prolific_id)

data0 <- data0 |> 
 left_join(mapping, by = c("prolific_ID_url" = "prolific_id")) |> 
 rename(id = anon_id) |>
 relocate(id, .before = everything()) |> 
 select(-prolific_ID_url, -prolific_ID, -session_ID, -study_ID, 
        -timestamp, -localTime, -date_exp, -meta.location) |> 
 select(-starts_with("...")) 

save(data0, file = "part4_WMC_Gf.RData")
load("part4_WMC_Gf.RData")

### participants' comments --------------------------------------------------
data0$id |> unique() |> length() 
data0$comments |>  unique()
#nothing in particular to pay attention to 

# participant exclusion ---------------------------------------------------
#one participant used auto translation add-ins when 
#performing the experiment. As a result, their feedback were not correctly given during the experiment.
#one participant performed OSPAN three times. 
data_exclude <- data0 |>  
  filter(grepl("font style|Leave empty|Leave blank", answer, ignore.case = TRUE) | count_rows > 2000)

exclusion_wave4_IDs <- data_exclude$id |>  unique() |>  as.data.frame()
colnames(exclusion_wave4_IDs) <- "id"
write.csv(exclusion_wave4_IDs, "exclusion-part4-IDs.csv", row.names = FALSE)

# clean the data from formal test trials  ---------------------------------
# OSPAN data ------------------
df_OSPAN <- data0 |> 
  filter(grepl("ORT", sender)) |> 
  select(id, sender_id, span_length, trial, span, answer, correctRecall, splength,
         correctResponse, response, correct, percentageTrue, countfalse_here, sender) |> 
  filter (sender == "ORT_feedback") |>  
  group_by(id) |> 
  mutate(trial_count = n())  # one participants performed the ospan three times, and we exclude this participant

df_OSPAN$answer <- sapply(df_OSPAN$answer, function(x){
  x <- gsub("&nbsp;", "", x)
  x <- gsub("Leer lassen", "0", x)
  y <- paste0(x, collapse = "")
  return(y)
})

df_OSPAN$span <- sapply(df_OSPAN$span, function(x){
  y <- paste0(x, collapse = "")
  return(y)
})

 
df_OSPAN_cleaned <- df_OSPAN |>  
  select(id, sender_id, trial, span_length, span, answer, correctRecall, 
         countfalse_here, percentageTrue) |> 
  mutate(trial = as.numeric(trial),
         raw_correct = as.numeric(correctRecall),
         splength = as.numeric(span_length),
         all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |>  
  group_by(id) |> 
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /42) |>  #span length varies from 3 to 9
  select(id, splength, OSPAN_span = span, 
         OSPAN_answer = answer,
         OSPAN_partial_credit = raw_correct,
         OSPAN_all_or_nothing = all_or_nothing,
         OSPAN_processing = pcTrue) 

write.csv(df_OSPAN_cleaned, "df_part4_OSPAN.csv", row.names = FALSE)

# SSPAN data --------------------------------------------------------------
df_SSPAN <- data0 |> 
  filter(grepl("SRT", sender)) |> 
  select(id, sender_id, trial, span_length, splength, correctResponse, 
         response, correct, span, answer, percentageTrue, countfalse_here, correctRecall, sender) |>  
  filter (sender == "SRT_feedback") |>  
  group_by(id) |> 
  mutate(trial_count = n()) 

clean_string <- function(x) {
  x <- gsub("&nbsp;", "", x)
  x <- gsub("blank", "0", x)
  x <- gsub("10", "A", x)
  x <- gsub("11", "B", x)
  x <- gsub("12", "C", x)
  x <- gsub("13", "D", x)
  x <- gsub("14", "E", x)
  x <- gsub("15", "F", x)
  x <- gsub("16", "G", x)
  x <- gsub("matrix|\\.svg", "", x)
  return(x)
}

df_SSPAN$span <- sapply(df_SSPAN$span, clean_string)
df_SSPAN$span <- sapply(df_SSPAN$span, function(x){
  if(identical(x, character(0))){
    return(NA)
  } else
    y <- paste0(x, collapse = "")
  return(y)
})

df_SSPAN$answer <- sapply(df_SSPAN$answer, function(x){
  if(identical(x, character(0))){
    return(NA)
  } else
    y <- paste0(x, collapse = "")
  return(y)
})
df_SSPAN$answer <- sapply(df_SSPAN$answer, clean_string)

df_SSPAN_cleaned <- df_SSPAN |> 
  select(id, sender_id, trial, span_length, span, answer, correctRecall, 
         countfalse_here, percentageTrue) |> 
  mutate(trial = as.numeric(trial),
         raw_correct = as.numeric(correctRecall),
         splength = as.numeric(span_length),
         all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |> 
  group_by(id) |> 
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /27) |>  #span length varies from 2 to 7
  select(id, splength,
         SSPAN_span = span, 
         SSPAN_answer = answer, 
         SSPAN_partial_credit = raw_correct,
         SSPAN_all_or_nothing = all_or_nothing,
         SSPAN_processing = pcTrue) 

write.csv(df_SSPAN_cleaned, "df_part4_SSPAN.csv", row.names = FALSE)

# RSPAN data ---------------
df_RSPAN <- data0 |> 
  filter(grepl("RRT", sender)) |> 
  select(id, sender_id, trial, span_length, splength, correctResponse, response, correct, span, 
         answer, percentageTrue, countfalse_here, correctRecall, sender, meta.userAgent) |>  
  filter (sender == "RRT_feedback") |>  
  group_by(id) |> 
  mutate(trial_count = n()) 

clean_string_rspan <- function(x) {
  x <- gsub("&nbsp;", "", x)
  x <- gsub("arrowblank", "0", x)
  x <- gsub("10", "A", x)
  x <- gsub("11", "B", x)
  x <- gsub("12", "C", x)
  x <- gsub("13", "D", x)
  x <- gsub("14", "E", x)
  x <- gsub("15", "F", x)
  x <- gsub("16", "G", x)
  x <- gsub("arrow|\\.png", "", x)
  if(identical(x, character(0))){
    return(NA)
  } else
    y <- paste0(x, collapse = "")
  return(y)
}

df_RSPAN$answer <- sapply(df_RSPAN$answer, clean_string_rspan)
df_RSPAN$span <- sapply(df_RSPAN$span, clean_string_rspan)

df_RSPAN_cleaned <- df_RSPAN |> 
  select(id, sender_id, trial, span_length, span, answer, correctRecall, 
         countfalse_here, percentageTrue, meta.userAgent) |> 
  mutate(trial = as.numeric(trial),
         raw_correct = as.numeric(correctRecall),
         splength = as.numeric(span_length),
         all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |> 
  group_by(id) |> 
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /27) |>  #span length varies from 2 to 7
  select(id, splength,
         RSPAN_span = span, 
         RSPAN_answer = answer, 
         RSPAN_partial_credit = raw_correct,
         RSPAN_all_or_nothing = all_or_nothing,
         RSPAN_processing = pcTrue)

write.csv(df_RSPAN_cleaned, "df_part4_RSPAN.csv", row.names = FALSE)


# Hagen Matrices Test -----------------------------------------------------
data_HMT <- data0 |>  
  filter(sender %in% c("HTM-S_Instr1",
                       "HTM-S_Instr2",
                       "Ex1_1",
                       "Feed_corr",
                       "Feed_incor",
                       "Ex1_2",
                       "Ex2_1",
                       "Feed_corr",
                       "Feed_incor",
                       "Ex2_2",
                       "HMT-S_Seq_Ex",
                       "HTM-S_Instr2",
                       "HMT-S_Input",
                       "HMT_S_ISI",
                       "HMT-S_Sequence",
                       "HMT-S_Loop",
                       "HMT-S_Seq",
                       "HTM-S_Ende",
                       "HMT-S_GrandSequence")) |> 
  select(id, HMTresponded, sender, responses: corrAnsw) |>  
  drop_na() |>  
  mutate(responses_correct = ifelse(responses == corrAnsw, TRUE, FALSE)) |> 
  group_by(id) |> 
  mutate(no_correct = sum(responses_correct)) |> 
  select(id, HMTresponded, sender, responses, corrAnsw, responses_correct, no_correct) |> 
  select(id, no_correct) |>  unique()

write.csv(data_HMT, "df_HMT.csv", row.names = FALSE)

