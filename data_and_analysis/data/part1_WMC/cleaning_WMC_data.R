# R preparation -----------------------------------------------------------
#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

###package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
} 
library("pacman")
p_load(tidyverse, jsonlite, here)

`%notin%` <- Negate(`%in%`)

### read in original .txt file and remove personally identifiable information
#data0 <- read_file("raw_wmc_results.txt") |> # Read the text file from JATOS ...
#   str_split('\n') |> first()  |>    # ... split it into lines ...
#   discard(function(x) x == '') |>   # ... filter empty rows ...
#   map_dfr(fromJSON, flatten=T)  |> 
#  fill(url.srid, .direction = "down") |> 
#  group_by(url.srid) |> 
#  fill(prolific_ID, .direction = "updown") |> 
#  ungroup() |> 
#  mutate(prolific_ID = sub("@email\\.prolific\\.co$", "", prolific_ID))
#
############# replace prolific IDs with random strings 
#set.seed(1)
##
#data0$prolific_ID <- as.character(data0$prolific_ID)
##
###unique original IDs
#uids <- unique(data0$prolific_ID)
##
###generate random-looking new IDs (e.g., "ID_A1B2C3D4E5")
#rand_string <- function(n = 10) {
#  paste(sample(c(LETTERS, 0:9), n, replace = TRUE), collapse = "")
#}
#new_ids <- paste0("ID_", vapply(seq_along(uids), function(i) rand_string(10), character(1)))
##
### make an old->new map and apply it to every row
#map <- setNames(new_ids, uids)
#data0$id <- unname(map[data0$prolific_ID])
##
### 4) mapping table (useful to store separately)
#mapping <- data.frame(prolific_id = uids, anon_id = new_ids, stringsAsFactors = FALSE)
##
#write.csv(mapping, "mapping.csv", row.names = FALSE)
##
#data0 <- data0 |> 
#  select(-prolific_ID, -prolific_ID_url, -session_ID, -study_ID, -timestamp, -meta.location) |> 
#  select(-starts_with("...")) 
#
#data0 <- data0 |> 
#  relocate(id, .before = everything()) 
#
#save(data0, file = "part1_WMC.RData")

load("part1_WMC.RData")

################################################
data0$comments |> unique()
#participants indicated errors in programming:
#[22] “I didn't quite understand the mirrored letters; either I was too stupid or sometimes 
#the letters were marked incorrectly.”
#[23] “The last part with the arrows and letters didn't work at all. 
#The questions about the letters were skipped and marked as failed. Everything was automatically shown in fast forward.”
#[31] “I often didn't understand why I got the tasks with normal or mirrored letters wrong 
#– I don't know if it was me or if the instructions were too confusing.”
#[34] “The letter in the last step was very difficult for me to identify. 
#Therefore, I'm sorry that the last round didn't go well.”
# [36] “I didn't quite understand exercise 3.”

#particiapnts indicated difficulties:
#[7] “There was a glitch on my screen during part 3, so I didn't see the beginning 
#of one of the tasks and unfortunately made a big mistake. I apologize for that.” 
#[10] “If my result in the third part is too low, I'm happy to repeat it. \nBest regards”
#[14] “I was often unable to select an answer to the yes/no questions, which was then 
#counted as an error.”
#[15] “In the part with the red squares, the symmetrical images disappeared so quickly that 
#I could hardly press ‘continue’ (and they were therefore counted as wrong).”

#participants gave recommendation: 
#[24] “If you reveal in the first task that the average reaction time is being recorded 
#during the exercise and will be used for the subsequent tasks, 
#it is quite possible that test subjects will deliberately react slowly in the following 
#tasks in the exercise block in order to give themselves more time in the actual block.”

################################################################

 #participant exclusion 1: 
 #I detected there are some participants who used auto translation add-ins when 
 #performing the experiment. As a result, their feedback were not correctly given during the experiment. So I excluded these participants 
 data_exclude1 <- data0 |> 
   filter(grepl("font style|Leave empty|Leave blank", answer, ignore.case = TRUE))
 
 exclusion_IDs1 <- data_exclude1 |> 
   select(id) |> unique()
 
 #participant exclusion 2: 
 #three participants reported that they used help (e.g., pen and paper) for memory items.
 #So, I excluded these participants
 data0$cheating_memory |> unique()
 data0$help_memory_type |> unique() 
 data0$quality |> unique()
 data0$cheating_distractor |> unique()
 
 data_exclude2 <- data0 |> 
   filter(cheating_memory == "cheating_memory_yes") |> 
   select(id, cheating_memory, help_memory_type)
 
 exclusion_IDs2 <- data_exclude2 |> 
   select(id) |> unique()
 
 exclusion_IDs <- rbind(exclusion_IDs1, exclusion_IDs2) |> 
   unique()
 
 write.csv( exclusion_IDs, "exclusion_part1_IDs.csv", row.names = FALSE)

# clean the data from formal test trials  ---------------------------------
# OSPAN trials ------------------------------------------------------------
df_OSPAN <- data0 |>
  filter( sender == "Real_mathEquarigt" | sender == "Screen_letter" |
            sender == "Screen_feedback") |>
  filter(grepl("^8_0_4_3", sender_id))  #only selected data from formal test trials 
 
 
#string manipulation
df_OSPAN$span <- sapply(df_OSPAN$span, paste, collapse = "")
df_OSPAN$answer <- sapply(df_OSPAN$answer, paste, collapse = "")
df_OSPAN$answer <- gsub("&nbsp;", "", df_OSPAN$answer)
df_OSPAN$answer <- gsub("Leer lassen", "0", df_OSPAN$answer)

df_OSPAN_cleaned <- df_OSPAN[, !apply(is.na(df_OSPAN), 2, all)] |> #remove columns that contain only NAs
  select(id, correctResponse, response, correct, truearr, span, 
         answer, percentageTrue, countfalse_here, correctRecall, sender,
         splength) |>
  group_by(id) |>
  filter (sender == "Screen_feedback") |> 
  select(id, splength, span, answer, correctRecall, countfalse_here, percentageTrue) |>
  mutate(raw_correct = as.numeric(correctRecall),
         splength = as.numeric(splength),
         all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |> 
  group_by(id) |>
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /25) |>
  select(id, splength, OSPAN_span = span, 
         OSPAN_answer = answer, OSPAN_partial_credit = raw_correct,  
         OSPAN_all_or_nothing = all_or_nothing, 
         OSPAN_processing = pcTrue) 

# SSPAN trials ------------------------------------------------------------
df_SSPAN <- data0  |>
  filter( sender == "Real_symm_pic" | sender == "SymmResponse" |
            sender == "Screen_showsquare" | sender == "MatrixResponse" | 
            sender == "BothFeedback") |>
  filter(grepl("^8_3_3_3", sender_id)) #SSPAN formal test trials

#string manipulation
#recode the variable, matrix1 -> 1, matrix2 -> 2, matrix3 -> 3, ......, matrix11 -> A, matrix12 -> B, 
#matrix13 -> C, matrix14 -> D, matrix15 -> E, matrix16 -> F

#span
df_SSPAN$span <- sapply(df_SSPAN$span, paste, collapse = "")
df_SSPAN$span <- gsub("10", "A", df_SSPAN$span)
df_SSPAN$span <- gsub("11", "B", df_SSPAN$span)
df_SSPAN$span <- gsub("12", "C", df_SSPAN$span)
df_SSPAN$span <- gsub("13", "D", df_SSPAN$span)
df_SSPAN$span <- gsub("14", "E", df_SSPAN$span)
df_SSPAN$span <- gsub("15", "F", df_SSPAN$span)
df_SSPAN$span <- gsub("16", "G", df_SSPAN$span)
df_SSPAN$span <- gsub("matrix|\\.svg", "", df_SSPAN$span)

#recall
df_SSPAN$answer <- sapply(df_SSPAN$answer, paste, collapse = "")
df_SSPAN$answer <- gsub("&nbsp;", "", df_SSPAN$answer)
df_SSPAN$answer <- gsub("blank", "0", df_SSPAN$answer)

df_SSPAN$answer <- gsub("10", "A", df_SSPAN$answer)
df_SSPAN$answer <- gsub("11", "B", df_SSPAN$answer)
df_SSPAN$answer <- gsub("12", "C", df_SSPAN$answer)
df_SSPAN$answer <- gsub("13", "D", df_SSPAN$answer)
df_SSPAN$answer <- gsub("14", "E", df_SSPAN$answer)
df_SSPAN$answer <- gsub("15", "F", df_SSPAN$answer)
df_SSPAN$answer <- gsub("16", "G", df_SSPAN$answer)
df_SSPAN$answer <- gsub("matrix|\\.svg", "", df_SSPAN$answer)

df_SSPAN_cleaned <- df_SSPAN[, !apply(is.na(df_SSPAN), 2, all)] |>
  select(id, correctResponse, response, correct, truearr, span, 
         answer, percentageTrue, countfalse_here, correctRecall, sender,
         splength) |>
  group_by(id) |>
  filter (sender == "BothFeedback") |> 
  select(id, splength, span, answer, correctRecall, countfalse_here, percentageTrue) |>
  mutate(raw_correct = as.numeric(correctRecall),
         splength = as.numeric(splength),
         all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |> 
  group_by(id) |>
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /14) |>
  select(id, splength, SSPAN_span = span, 
         SSPAN_answer = answer, SSPAN_partial_credit = raw_correct,
         SSPAN_all_or_nothing = all_or_nothing, 
         SSPAN_processing = pcTrue) 

# RSPAN trials ------------------------------------------------------------

df_RSPAN <- data0  |>
  filter( sender == "Screen_showarrow" | sender == "Screen_letter" |
            sender == "LetterResponse" | sender == "Screen_arrow_response" | 
            sender == "Screen_arrow_feedback") |>
  filter(grepl("^8_6_3_3", sender_id)) #RSPAN formal test trials

df_RSPAN$span <- sapply(df_RSPAN$span, paste, collapse = "")

df_RSPAN$span <- gsub("10", "A", df_RSPAN$span)
df_RSPAN$span <- gsub("11", "B", df_RSPAN$span)
df_RSPAN$span <- gsub("12", "C", df_RSPAN$span)
df_RSPAN$span <- gsub("13", "D", df_RSPAN$span)
df_RSPAN$span <- gsub("14", "E", df_RSPAN$span)
df_RSPAN$span <- gsub("15", "F", df_RSPAN$span)
df_RSPAN$span <- gsub("16", "G", df_RSPAN$span)
df_RSPAN$span <- gsub("arrow|\\.png", "", df_RSPAN$span)

#recall
df_RSPAN$answer <- sapply(df_RSPAN$answer, paste, collapse = "")
df_RSPAN$answer <- gsub("&nbsp;", "", df_RSPAN$answer)
df_RSPAN$answer <- gsub("arrowblank", "0", df_RSPAN$answer)
df_RSPAN$answer <- gsub("10", "A", df_RSPAN$answer)
df_RSPAN$answer <- gsub("11", "B", df_RSPAN$answer)
df_RSPAN$answer <- gsub("12", "C", df_RSPAN$answer)
df_RSPAN$answer <- gsub("13", "D", df_RSPAN$answer)
df_RSPAN$answer <- gsub("14", "E", df_RSPAN$answer)
df_RSPAN$answer <- gsub("15", "F", df_RSPAN$answer)
df_RSPAN$answer <- gsub("16", "G", df_RSPAN$answer)
df_RSPAN$answer <- gsub("arrow|\\.png", "", df_RSPAN$answer)

df_RSPAN_cleaned <- df_RSPAN[, !apply(is.na(df_RSPAN), 2, all)] |>
  select(id, correctResponse, response, correct, truearr, span, 
         answer, percentageTrue, countfalse_here, correctRecall, sender,
         splength) |>
  group_by(id) |>
  filter (sender == "Screen_arrow_feedback") |> 
  select(id, splength, span, answer, correctRecall, countfalse_here, percentageTrue) |>
  mutate(raw_correct = as.numeric(correctRecall),
          splength = as.numeric(splength),
          all_or_nothing = ifelse(splength == raw_correct, 1, 0)) |> 
  group_by(id) |>
  mutate(processing_error = sum(countfalse_here, na.rm = TRUE),
         pcTrue = 1 - processing_error /14) |>
  select(id, splength, RSPAN_span = span, 
         RSPAN_answer = answer, RSPAN_partial_credit = raw_correct, 
         RSPAN_all_or_nothing = all_or_nothing, 
         RSPAN_processing = pcTrue) 

#########
write.csv(df_OSPAN_cleaned, "df_OSPAN.csv", row.names = FALSE)
write.csv(df_SSPAN_cleaned, "df_SSPAN.csv", row.names = FALSE)
write.csv(df_RSPAN_cleaned, "df_RSPAN.csv", row.names = FALSE)

