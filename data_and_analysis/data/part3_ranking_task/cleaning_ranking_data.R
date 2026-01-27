# R preparation -----------------------------------------------------------
#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
} 
library("pacman") 
p_load(tidyverse, jsonlite, dplyr, stringdist, Hmisc, stringr)

#function
`%notin%` <- Negate(`%in%`)

# read in original .txt file and remove personally identifiable information
# parse JSON into a data.frame
#data0 <- read_file('raw_ranking_results.txt') |> # Read the text file from JATOS ...
#  str_split('\n') |> first() |>   # ... split it into lines ...
#  discard(function(x) x == '') |>   # ... filter empty rows ...
#  map_dfr(fromJSON, flatten=T) # ... parse JSON into a data.frame

#mapping <- read_csv("mapping.csv")
#data0$prolific_ID_url <- as.character(data0$prolific_ID_url)
#mapping$prolific_id <- as.character(mapping$prolific_id)

#data0 <- data0 |> 
#  left_join(mapping, by = c("prolific_ID_url" = "prolific_id")) |> 
#  rename(id = anon_id) |>
#  relocate(id, .before = everything()) |> 
#  select(-prolific_ID_url, -session_ID, -study_ID, -timestamp, -meta.location) |> 
#  select(-starts_with("...")) 

#save(data0, file = "part3_ranking_task.RData")

load("part3_ranking_task.RData")

# functions  --------------------------------------------------------------
encode_each_item <- function(x) {
  if (identical(x, "Eine zufällig ausgewählte Person in Deutschland spielt täglich Volleyball.")) {
    y <- "impl2_pos"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wird im Lauf ihres Lebens an Malaria erkranken.")){
    y <- "impl6_pos"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland kann NICHT in mehr als vier Sprachen sprechen.")){
    y <- "impl4_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wurde NICHT in einem Krankenhaus geboren.")){
    y <- "plau6_neg"
    
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland im Alter von 20 bis 25 Jahren studiert an einer Universität oder Hochschule.")){
    y <- "indiff2_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland geht täglich mehr als 100 Schritte.")){
    y <- "plau4_pos"
    
  }  else if (identical(x, "In einem zufällig ausgewählten Krankenhaus in Deutschland wird das nächste Neugeborene KEIN Mädchen sein.")){
    y <- "check1_inbetween"
    
  }  else if (identical(x, "Eine zufällig ausgewählte deutsche Person ist Mitglied der christlichen Kirche.")){
    y <- "indiff12_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte über 18-jährige Person in Deutschland hat KEINEN Bürojob.")){
    y <- "indiff6_neg"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag im Jahr wird die Temperatur in Deutschland über 15 °C liegen.")){
    y <- "indiff11_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland über 30 Jahre ist NICHT verheiratet.")){
    y <- "indiff1_neg"
    
  }  else if (identical(x, "In einem zufällig ausgewählten Jahr wird es in Deutschland im Juni NICHT schneien.")){
    y <- "impl3_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wohnt NICHT in Bayern, Baden-Württemberg oder Nordrhein-Westfalen.")){
    y <- "indiff3_neg"
    
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland spielt NICHT täglich Volleyball.")){
    y <- "impl2_neg"
    
  } else if (identical(x, "In einem zufällig ausgewählten deutschen Haushalt findet man mindestens eine Waschmaschine.")){
    y <- "plau3_pos"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland lebt NICHT im Saarland.")){
    y <- "impl5_neg"
    
  }  else if (identical(x, "Ein zufällig ausgewählter deutscher Erwachsener kann KEIN Fahrrad fahren.")){
    y <- "plau2_neg"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag in Hamburg wird es Regen geben.")){
    y <- "indiff4_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland besitzt KEIN Gerät, das sich mit dem Internet verbinden kann.")){
    y <- "plau5_neg"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag im Jahr 2020 war Berlin die Hauptstadt Deutschlands.")){
    y <- "check_place1"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland ist Fan eines Fußballvereins.")){
    y <- "indiff9_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland lebt NICHT in einer Großstadt.")){
    y <- "indiff7_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wird NICHT irgendwann an einer Herz-Kreislauf-Erkrankung sterben.")){
    y <- "indiff5_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland über 30 Jahre ist verheiratet.")){
    y <- "indiff1_pos"
    
  }  else if (identical(x, "In einem zufällig ausgewählten Jahr wird es in Deutschland im Juni schneien.")){
    y <- "impl3_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wohnt in Bayern, Baden-Württemberg oder Nordrhein-Westfalen.")){
    y <- "indiff3_pos"
    
    
  } else if (str_detect(x, "ausgewählte Person in Deutschland hat NICHT mehr als")){
    y <- "impl1_neg"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wird im Lauf ihres Lebens NICHT an Malaria erkranken.")){
    y <- "impl6_neg"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland lebt im Saarland.")){
    y <- "impl5_pos"
    
  }  else if (identical(x, "Ein zufällig ausgewählter deutscher Erwachsener kann Fahrrad fahren.")){
    y <- "plau2_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland im Alter von 20 bis 25 Jahren studiert NICHT an einer Universität oder Hochschule.")){
    y <- "indiff2_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland geht täglich NICHT mehr als 100 Schritte.")){
    y <- "plau4_neg"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag im Jahr 2020 war Berlin NICHT die Hauptstadt Deutschlands.")){
    y <- "check_placelast"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland ist NICHT Fan eines Fußballvereins.")){
    y <- "indiff9_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland lebt in einer Großstadt.")){
    y <- "indiff7_pos"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag im Jahr wird die Temperatur in Deutschland NICHT über 15 °C liegen.")){
    y <- "indiff11_neg"
    
  }  else if (identical(x, "Ein zufällig ausgewählter Baum in Deutschland ist ein Laubbaum.")){
    y <- "indiff8_pos"
    
  }  else if (str_detect(x, "KEIN Englisch")){
    y <- "plau1_neg"
    
  }  else if (identical(x, "Ein zufällig ausgewähltes Auto auf der Straße in Deutschland wurde NICHT in Deutschland hergestellt.")){
    y <- "indiff10_neg"
    
    
  } else if (str_detect(x, "ausgewählte Person in Deutschland hat mehr als")){
    y <- "impl1_pos"
    
    
    
  } else if (identical(x, "In einem zufällig ausgewählten deutschen Haushalt findet man KEINE Waschmaschine.")){
    y <- "plau3_neg"
    
  } else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland kann in mehr als vier Sprachen sprechen.")){
    y <- "impl4_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wurde in einem Krankenhaus geboren.")){
    y <- "plau6_pos"
    
  }  else if (identical(x, "An einem zufällig ausgewählten Tag in Hamburg wird es KEINEN Regen geben.")){
    y <- "indiff4_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland besitzt mindestens ein Gerät, das sich mit dem Internet verbinden kann.")){
    y <- "plau5_pos"
    
  }  else if (identical(x, "In einem zufällig ausgewählten Krankenhaus in Deutschland wird das nächste Neugeborene ein Mädchen sein.")){
    y <- "check_inbetween2"
    
  }  else if (identical(x, "Eine zufällig ausgewählte deutsche Person ist NICHT Mitglied der christlichen Kirche.")){
    y <- "indiff12_neg"
    
  }  else if (identical(x, "Eine zufällig ausgewählte über 18-jährige Person in Deutschland hat einen Bürojob.")){
    y <- "indiff6_pos"
    
  }  else if (identical(x, "Eine zufällig ausgewählte Person in Deutschland wird irgendwann an einer Herz-Kreislauf-Erkrankung sterben.")){
    y <- "indiff5_pos"
    
  }  else if (identical(x, "Ein zufällig ausgewählter Baum in Deutschland ist KEIN Laubbaum.")){
    y <- "indiff8_neg"
    
  }  else if (str_detect(x, "spricht Englisch")){
    y <- "plau1_pos"
    
  }  else if (identical(x, "Ein zufällig ausgewähltes Auto auf der Straße in Deutschland wurde in Deutschland hergestellt.")){
    y <- "indiff10_pos"
    
    
  } else {
    y <- ""
    
  }  
  
  return(y)
}

#if there are tied events. These two events will be stored in a list under the column rank_1 or rank_2, ranK_3, rank_4 etc.
#unlist these lists first
encode_each_item_2 <- function(x) {
  
  events <- sapply(x,  function(x){x |> str_split('\n')}) |> unlist() |> unique()
  
  
  eventsindex <- lapply(events, encode_each_item) |> unlist() |> as.character()
  
  return(eventsindex )
}

#function to decide if participants are presented with a "middle" or "edge" event set
judge_con <- function(eveTopleft, eveTopright, eveDownleft, eveDownright){
  
  no_of_tr <- c(str_detect(eveTopleft, "diff"), str_detect(eveTopright, "diff"), str_detect(eveDownleft, "diff"), str_detect(eveDownright, "diff"))
  
  
  if (sum(no_of_tr, na.rm = TRUE) == 4 ) {
    con <- "middle"
  } else if ( sum(no_of_tr, na.rm = TRUE) == 0 ){
    con <- "edge"
  } else if (sum(no_of_tr, na.rm = TRUE) == 2 ){
    con <- "mixed"
  } else {
    con <- ""
  }
  
  return(con)
}

#function to label events that are presented in a same set with the labels "A" "a", "B" "b"
judge_eveTopleft <- function(eveTopleft){
  if ( grepl("_pos", eveTopleft, fixed=TRUE) ) {
    eve = "A"
  } else if ( grepl("_neg", eveTopleft, fixed=TRUE) ) {
    eve = "a"
  }
  
  return(eve)
}

judge_eveTopright <- function(eveTopleft, eveTopright){
  
  if( stringdist(eveTopleft, eveTopright) == 3 && grepl("_pos", eveTopright, fixed=TRUE) ){
    eve = "A"
  } else if ( stringdist(eveTopleft, eveTopright) == 3 && grepl("_neg", eveTopright, fixed=TRUE) ) {
    eve = "a"
  } else if ( stringdist(eveTopleft, eveTopright) != 3 && grepl("_pos", eveTopright, fixed=TRUE) ){
    eve = "B"
  } else if ( stringdist(eveTopleft, eveTopright) != 3 && grepl("_neg", eveTopright, fixed=TRUE)  ){
    eve = "b"
  }
  
}

judge_eveDownleft <- function(eveTopleft, eveDownleft){
  
  if( stringdist(eveTopleft, eveDownleft) == 3 && grepl("_pos", eveDownleft, fixed=TRUE) ){
    eve = "A"
  } else if ( stringdist(eveTopleft, eveDownleft) == 3 && grepl("_neg", eveDownleft, fixed=TRUE) ) {
    eve = "a"
  } else if ( stringdist(eveTopleft, eveDownleft) != 3 && grepl("_pos", eveDownleft, fixed=TRUE) ){
    eve = "B"
  } else if ( stringdist(eveTopleft, eveDownleft) != 3 && grepl("_neg", eveDownleft, fixed=TRUE)  ){
    eve = "b"
  }
  
}

judge_eveDownright <- function(eveTopleft, eveDownright){
  
  if( stringdist(eveTopleft, eveDownright) == 3 && grepl("_pos", eveDownright, fixed=TRUE) ){
    eve = "A"
  } else if ( stringdist(eveTopleft, eveDownright) == 3 && grepl("_neg", eveDownright, fixed=TRUE) ) {
    eve = "a"
  } else if ( stringdist(eveTopleft, eveDownright) != 3 && grepl("_pos", eveDownright, fixed=TRUE) ){
    eve = "B"
  } else if ( stringdist(eveTopleft, eveDownright) != 3 && grepl("_neg", eveDownright, fixed=TRUE)  ){
    eve = "b"
  }
  
}

#function to change the naming of events in "eveTopleft_type": "eveDownright_type", i.e., changing indiff_pos10 to pos10_indiff
encode_each_item_three <- function(x) {
  if (identical(x, "impl_pos2")) {
    y <- "impl2_pos"
    
  } else if (identical(x, "impl_pos6")){
    y <- "impl6_pos"
    
  } else if (identical(x, "impl_neg4")){
    y <- "impl4_neg"
    
  }  else if (identical(x, "plau_neg6")){
    y <- "plau6_neg"
    
    
  }  else if (identical(x, "indiff_pos2")){
    y <- "indiff2_pos"
    
  }  else if (identical(x, "plau_pos4")){
    y <- "plau4_pos"
    
  }  else if (identical(x, "indiff_pos12")){
    y <- "indiff12_pos"
    
  }  else if (identical(x, "indiff_neg6")){
    y <- "indiff6_neg"
    
  }  else if (identical(x, "indiff_pos11")){
    y <- "indiff11_pos"
    
  }  else if (identical(x,  "indiff_neg1")){
    y <- "indiff1_neg"
    
  }  else if (identical(x, "impl_neg3")){
    y <- "impl3_neg"
    
  }  else if (identical(x, "indiff_neg3")){
    y <- "indiff3_neg"
    
  }  else if (identical(x, "impl_neg2")){
    y <- "impl2_neg"
    
  } else if (identical(x, "plau_pos3")){
    y <- "plau3_pos"
    
  } else if (identical(x, "impl_neg5")){
    y <- "impl5_neg"
    
  }  else if (identical(x, "plau_neg2")){
    y <- "plau2_neg"
    
  }  else if (identical(x, "indiff_pos4")){
    y <- "indiff4_pos"
    
  }  else if (identical(x, "plau_neg5")){
    y <- "plau5_neg"
    
  }  else if (identical(x, "indiff_pos9")){
    y <- "indiff9_pos"
    
  }  else if (identical(x, "indiff_neg7")){
    y <- "indiff7_neg"
    
  }  else if (identical(x, "indiff_neg5")){
    y <- "indiff5_neg"
    
  }  else if (identical(x, "indiff_pos1")){
    y <- "indiff1_pos"
    
  }  else if (identical(x, "impl_pos3")){
    y <- "impl3_pos"
    
  }  else if (identical(x, "indiff_pos3")){
    y <- "indiff3_pos"
    
  } else if (str_detect(x, "impl_neg1")){
    y <- "impl1_neg"
    
  } else if (identical(x, "impl_neg6")){
    y <- "impl6_neg"
    
  } else if (identical(x, "impl_pos5")){
    y <- "impl5_pos"
    
  }  else if (identical(x, "plau_pos2")){
    y <- "plau2_pos"
    
  }  else if (identical(x, "indiff_neg2")){
    y <- "indiff2_neg"
    
  }  else if (identical(x, "plau_neg4")){
    y <- "plau4_neg"
    
  }  else if (identical(x, "indiff_neg9")){
    y <- "indiff9_neg"
    
  }  else if (identical(x, "indiff_pos7")){
    y <- "indiff7_pos"
    
  }  else if (identical(x, "indiff_neg11")){
    y <- "indiff11_neg"
    
  }  else if (identical(x, "indiff_pos8")){
    y <- "indiff8_pos"
    
  }  else if (str_detect(x, "plau_neg1")){
    y <- "plau1_neg"
    
  }  else if (identical(x, "indiff_neg10")){
    y <- "indiff10_neg"
    
    
  } else if (str_detect(x, "impl_pos1")){
    y <- "impl1_pos"
    
    
    
  } else if (identical(x, "plau_neg3")){
    y <- "plau3_neg"
    
  } else if (identical(x, "impl_pos4")){
    y <- "impl4_pos"
    
  }  else if (identical(x, "plau_pos6")){
    y <- "plau6_pos"
    
  }  else if (identical(x, "indiff_neg4")){
    y <- "indiff4_neg"
    
  }  else if (identical(x, "plau_pos5")){
    y <- "plau5_pos"
    
  }  else if (identical(x, "indiff_neg12")){
    y <- "indiff12_neg"
    
  }  else if (identical(x, "indiff_pos6")){
    y <- "indiff6_pos"
    
  }  else if (identical(x, "indiff_pos5")){
    y <- "indiff5_pos"
    
  }  else if (identical(x, "indiff_neg8")){
    y <- "indiff8_neg"
    
  }  else if (str_detect(x, "plau_pos1")){
    y <- "plau1_pos"
    
  }  else if (identical(x, "indiff_pos10")){
    y <- "indiff10_pos"
    
    
  } else {
    y <- ""
    
  }  
  
  return(y)
}

# data cleaning  ----------------------------------------------------------
# select useful rows and columns 
data <- data0  |>  
  filter(sender %in% c("Welcome", "study screen", "Form_ruleDetecting",
                       "Demographic control","Post-study questionnaire")) |>
  select(id,
         sender,
         f00, 
         rank_Order_one: rank_Order_four,
         quality, comments,
         age: level_education,
         `rank A-B-nichtB-nichtA`: `rank nicht-A-A-nicht-B-B`, 
         duration:time_switch, eveTopleft:eveDownright_type, meta.timeZone,
         meta.language)

# attention check and comments 
data$quality |> unique()
data$comments |> unique()
#take away: rule-detection question could be better
#[1] NA 
#[2] “The study was very entertaining.” 
#[3] “” 
#[4] “/” 
#[5] “Was exciting! This time it was faster than the other two parts! :)” 
#[6] “Cool survey. Very nice tasks. These were a lot of fun to work on. More of the same please.” 
#[7] “The survey worked very well technically. It was really fun to think about the probabilities. A progress bar would have been nice.” 
#[8] “I didn't understand the last task.” 
#[9] “With the final question with the bullet points of \”A, B, not-A, not-B\” I was initially unsure whether this was perhaps a hidden attention test. I tried to answer the question based on my logic / my best guess. As feedback, I would like to positively emphasize (as I did last time) how detailed you wrote the instructions in this study. Overall, everything went well and there were no problems. I wish you much success in your research and thank you for allowing me to participate!"
#[10] ‘I would be interested to know what proportion of participants recognized the logical structure and after how many questions the realization occurred.’ 
#[11] ”Unfortunately, the question about the possible logical constellations of the answer options A, not A, B, not B was not quite unambiguously posed. I therefore assumed that the probabilities for A and B are different. If A and B are allowed to have the same probability, then AABB or BBAA is also possible and if A & B both have 50% probability, then all answer options are possible.” 
#[12] ”I didn't think about logic enough for the first few questions... So the first arrangements don't make much mathematical sense.” 
#[13] “I am doing a part-time course alongside my job, so I have a part-time job as well as studying.” 
#[14] “Good luck.” 
#[15] “That was a lot of fun. Thanks for that.” 
#[16] “--/--” 
#[17] “No.”


# Data cleaning, step 1: re-label the events with the label that indicates event type (indiff vs. plau vs. implau) and negations (pos/neg)  ----------------------------------------------------------
#re-label the events
data$rank_1<- sapply(data$rank_Order_one,  encode_each_item_2) 
data$rank_2 <- sapply(data$rank_Order_two,  encode_each_item_2)
data$rank_3 <- sapply(data$rank_Order_three,  encode_each_item_2)
data$rank_4 <- sapply(data$rank_Order_four,  encode_each_item_2)

data$rank_1[sapply(data$rank_1, function(x){identical(x, character(0))})] <- "_"
data$rank_2[sapply(data$rank_2, function(x){identical(x, character(0))})] <- "_"
data$rank_3[sapply(data$rank_3, function(x){identical(x, character(0))})] <- "_"
data$rank_4[sapply(data$rank_4, function(x){identical(x, character(0))})] <- "_"

data_rankings <- data |> 
  mutate(rank_combined = paste(data$rank_1, data$rank_2, rank_3, rank_4, sep="_")) |> 
  select(id, f00, rank_1: rank_4, eveTopleft_type: eveDownright_type) |> drop_na() 

# code the within-subjects condition, if participants see edge or middle event sets
input_list_for_condition <- data_rankings |> select(id, eveTopleft = eveTopleft_type, 
                                                     eveTopright = eveTopright_type, 
                                                     eveDownleft = eveDownleft_type, 
                                                     eveDownright = eveDownright_type)
input_list_for_condition$id <- NULL

data_rankings <- data_rankings |>
  mutate(condition = pmap_chr(input_list_for_condition, judge_con)) |> 
  select(id, f00, condition, rank_1:rank_4,
         eveTopleft = eveTopleft_type,
         eveTopright = eveTopright_type,
         eveDownleft = eveDownleft_type,
         eveDownright = eveDownright_type)

data_rankings <- data_rankings |>
  mutate(eveTopleft = sapply(eveTopleft, encode_each_item_three),
         eveTopright = sapply(eveTopright, encode_each_item_three),
         eveDownleft = sapply(eveDownleft, encode_each_item_three),
         eveDownright = sapply(eveDownright, encode_each_item_three))


# participant exclusion ---------------------------------------------------
data_comprehension_check <- data0 |>
  filter(f00 == 13) |> 
  select(id, rank_Order_one, rank_Order_two,
         rank_Order_three, rank_Order_four) 

# Function to check if a string pattern exists in a list column
check_pattern_in_list <- function(list_col, pattern) {
  sapply(list_col, function(x) any(grepl(pattern, x, fixed = FALSE)))
}

# Check for "randomly selected" in each column
contains_random_selected <- data.frame(
  rank_Order_one = check_pattern_in_list(data_comprehension_check$rank_Order_one, "randomly selected"),
  rank_Order_two = check_pattern_in_list(data_comprehension_check$rank_Order_two, "randomly selected"),
  rank_Order_three = check_pattern_in_list(data_comprehension_check$rank_Order_three, "randomly selected"),
  rank_Order_four = check_pattern_in_list(data_comprehension_check$rank_Order_four, "randomly selected")
)

# Summary of which rows have the pattern in any column
data_comprehension_check$has_random_selection <- rowSums(contains_random_selected) > 0

data_comprehension_check_english <- data_comprehension_check |>
  filter(has_random_selection == TRUE)
#8 participants translated German to English when performing the study 

#exclude participants who did not pass the attention check question
data_exclu <- data_rankings |> filter(f00 == "13") |>
  filter((rank_4 != "check_placelast" & rank_3 != "check_placelast") | (rank_1 != "check_place1") ) |> 
  select(id) #8+8 participant

write.csv(data_exclu, "exclusion_part3_IDs.csv", row.names = FALSE)

#data_rankings <- data_rankings |> filter(ID %notin% data_exclu$ID) 

data_rankings <- data_rankings |>
  filter(f00 != "13")

# Data cleaning, step 2: relabel the events in a single trial with the label "A" "a" "B" "b" --------
data_rankings <- data_rankings |> 
  mutate(place_1 = sapply(eveTopleft, judge_eveTopleft))

input_list_eveTopright <- data_rankings |> select(id, eveTopleft, eveTopright)
input_list_eveTopright$id <- NULL

data_rankings <- data_rankings |> 
  mutate( place_2 = pmap_chr(input_list_eveTopright, judge_eveTopright) )

input_list_eveDownleft <- data_rankings |> select(id, eveTopleft, eveDownleft)
input_list_eveDownleft$id <- NULL

data_rankings <- data_rankings |> 
  mutate(place_3 = pmap_chr(input_list_eveDownleft, judge_eveDownleft))

input_list_eveDownright <- data_rankings |> select(id, eveTopleft, eveDownright)
input_list_eveDownright$id <- NULL

data_rankings <- data_rankings |> 
  mutate(place_4 = pmap_chr(input_list_eveDownright, judge_eveDownright))

data_rankings <- data_rankings |> 
  mutate(presentation_order = paste0(place_1, "_", place_2, "_", place_3, "_", place_4)) |> 
  select(-place_1, -place_2, -place_3, -place_4)

data_rankings |> group_by(presentation_order) |>
  count()


# Data cleaning step 3: Categorize the rankings into logical, interlaced-illogical, and stacked-illogical categories.  --------
# First we need to add an DV telling us if there are ties in participant's responses or not 

judge_if_ties <- function(rank_1, rank_2, rank_3, rank_4) {
  
  rank_1 <- !identical(rank_1, "_")
  rank_2 <- !identical(rank_2, "_") 
  rank_3 <- !identical(rank_3, "_")
  rank_4 <- !identical(rank_4, "_")
  
  if (all(rank_1, rank_2, rank_3, rank_4)) {
    if_ties <- 4
  } else if (all(rank_1, rank_2, rank_3) & !rank_4) {
    if_ties <- 3
  } else if (all(rank_1, rank_2) & !rank_3 & !rank_4) {
    if_ties <- 2
  } else if (rank_1 & !rank_2 & !rank_3 & !rank_4) {
    if_ties <- 1
  } else {
    if_ties <- "error"
  }
  
  return(if_ties)
  
} 
#if there are no ties, and four events receive four orders, then if ties = 4
# if there is one tie, and four events receive three orders, then if ties = 3
# so on and so forth
input_list_ties <- data_rankings |> dplyr::select(id, rank_1, rank_2, rank_3, rank_4) 
input_list_ties$id <- NULL
data_rankings <- data_rankings |> 
  mutate(if_ties = pmap(input_list_ties , judge_if_ties) )

#judge if the provided ranking is logical or not
judge_rank <- function(rank_1,rank_2,rank_3,rank_4,if_ties) {
  
  
  if ( if_ties == 3  && is.na( stringdist(rank_2[1], rank_2[2])) == FALSE ) {
    place_with_ties <- 2 
  } else if ( if_ties == 3 && is.na( stringdist(rank_1[1], rank_1[2])) == FALSE){
    place_with_ties <- 1
  } else if ( if_ties == 3 && is.na( stringdist(rank_3[1], rank_3[2])) == FALSE){
    place_with_ties <- 3
  } else {
    place_with_ties <- 0
  }
  
  
  if ( if_ties == 2 && length(rank_1) == 2 && length(rank_2) == 2){
    two_fields <- "even"
  } else if( if_ties == 2 && ( length(rank_1) != 2 | length(rank_2) != 2) ){
    two_fields <- "uneven"
  } else{
    two_fields <- "not_relevant"
  }
  
  
  if(if_ties == 4 && stringdist(rank_1, rank_4) == 3 && stringdist(rank_2, rank_3) == 3 ){
    return(0)
  } else if (if_ties == 4 && (stringdist(rank_1, rank_4) != 3 | stringdist(rank_2, rank_3) != 3 ) ){
    return(1)
  } else if( if_ties == 3 && place_with_ties == 2 && stringdist(rank_1, rank_3) == 3 && stringdist(rank_2[1], rank_2[2]) == 3){
    return(0)
  } else if( if_ties == 3 && place_with_ties == 2 && stringdist(rank_1, rank_3) != 3 && stringdist(rank_2[1], rank_2[2]) != 3 ){
    return(1)
  } else if( if_ties == 3 && place_with_ties == 3 ){
    return(1)
  } else if( if_ties == 3 && place_with_ties == 1){
    return(1)
  } else if ( if_ties == 2 && two_fields == "even" && stringdist(rank_1[1], rank_1[2]) != 3 && stringdist(rank_2[1], rank_2[2]) != 3 ){
    return(0)
  } else if (if_ties == 2 && two_fields == "even" && stringdist(rank_1[1], rank_1[2]) == 3 && stringdist(rank_2[1], rank_2[2]) == 3) {
    return(1)
  } else if (if_ties == 2 && two_fields == "uneven") {
    return(1)
  } else if (if_ties == 1) {
    return(0)
  } else {
    return(-1)
  }
  
}

input_list <- data_rankings |> dplyr::select(id, rank_1, rank_2, rank_3, rank_4, if_ties)
input_list$id <- NULL

data_rankings$logical_pass <- pmap_dbl(input_list, judge_rank)
# 0 = logical, 1 = illogical 

#Next, we determine the type of erroneous ranking - is it interlaced illogical ranking, stacked illogical ranking, or another type of illogical ranking?
# decide on the error type 
# we do it separately if each "if_ties" situation
# 1. full ordering (if_ties = 4)

judge_error_type_four_ties <- function(rank_1, rank_2, rank_3, rank_4, logical_pass) {
  
  if (logical_pass == 0) {
    error_type = NA
  } else {
    
    if(stringdist(rank_1, rank_2) == 3 & stringdist(rank_3, rank_4) == 3){
      error_type = 1 #stacked illogical rankings
    } else {
      error_type = 0  #interlaced illogical rankings
    }
    
  }
  
  return(error_type)
}

data_rankings_four_ties <- data_rankings |> filter(if_ties == 4) 

input_list_four_ties <- data_rankings_four_ties |> select(id, rank_1, rank_2, rank_3, rank_4, logical_pass) 

input_list_four_ties$id <- NULL

data_rankings_four_ties$error_type <- pmap_int(input_list_four_ties, judge_error_type_four_ties) 

# 1. partial ordering (if_ties = 3)
data_rankings_three_ties <- data_rankings |> filter(if_ties == 3) 

judge_A_three_where <- function(rank_1, rank_2, rank_3, rank_4) {
  
  if ( is.na( stringdist(rank_2[1], rank_2[2])) == FALSE ) {
    place_with_ties = 2 
  } else if ( is.na( stringdist(rank_1[1], rank_1[2])) == FALSE){
    place_with_ties = 1
  } else if ( is.na( stringdist(rank_3[1], rank_3[2])) == FALSE){
    place_with_ties = 3
  } else {
    place_with_ties = 0
  }
  return(place_with_ties)
}

input_list_three_where <- data_rankings_three_ties |> select(id, rank_1, rank_2, rank_3, rank_4) 

input_list_three_where$id <- NULL

data_rankings_three_ties$ties_where <- pmap_int(input_list_three_where, judge_A_three_where) 
data_rankings_three_where_one <- data_rankings_three_ties |>
  filter(ties_where == 1)

judge_error_three_where_one <- function(rank_1, logical_pass) {
  
  type <- NA
  
  if (logical_pass == 1 && stringdist(rank_1[1], rank_1[2]) == 3) {
    type = 1
  } else if ( logical_pass == 1 && stringdist(rank_1[1], rank_1[2]) != 3 ){
    type = 0
  }
  return(type)
}

input_list_three_where_one <- data_rankings_three_where_one |> select(id, rank_1, logical_pass) 

input_list_three_where_one$id <- NULL
data_rankings_three_where_one$error_type <- pmap_int(input_list_three_where_one, judge_error_three_where_one) 

data_rankings_three_where_two <- data_rankings_three_ties |>
  filter(ties_where == 2) 

judge_error_three_where_two <- function(rank_2, logical_pass){
  
  type <- NA
  
  if(logical_pass == 1 && stringdist(rank_2[1], rank_2[2]) != 3){
    type = 2 #other illogical rankings
  } 
  return(type)
  
}

input_list_three_where_two <- data_rankings_three_where_two |> select(id, rank_2, logical_pass) 

input_list_three_where_two$id <- NULL

data_rankings_three_where_two$error_type <- pmap_int(input_list_three_where_two, 
                                                     judge_error_three_where_two) 

data_rankings_three_where_three <- data_rankings_three_ties |>
  filter(ties_where == 3) 

judge_error_three_where_three <- function(rank_3, logical_pass) {
  
  type <- NA
  
  if (logical_pass == 1 && stringdist(rank_3[1], rank_3[2]) == 3 ) {
    type = 1 #stacked illogical rankings
  } else if (logical_pass == 1 && stringdist(rank_3[1], rank_3[2]) != 3) {
    type = 0 #interlaced illogical rankings
  }
  
  return(type)
  
}

input_list_three_where_three <- data_rankings_three_where_three |> select(id, rank_3, logical_pass)

input_list_three_where_three$id <- NULL

data_rankings_three_where_three$error_type <- pmap_int(input_list_three_where_three, 
                                                       judge_error_three_where_three)

data_rankings_three <- 
  rbind(data_rankings_three_where_one, data_rankings_three_where_two, 
        data_rankings_three_where_three) |> select(-ties_where)

### 2. partial ordering, using two fields (if_ties = 2)
data_rankings_two <- data_rankings |> filter(if_ties == 2) 

judge_error_two <- function(rank_1, rank_2, logical_pass) {
  
  type <- NA
  
  if (logical_pass == 1 && length(rank_1) == 2 && length(rank_2) == 2) {
    type = 1
  } else if (logical_pass == 1 && length(rank_1) != 2 &&  length(rank_2) != 2 ){
    type = 2
  } else if (logical_pass == 0) {
    type = NA
  }
  
  return(type)
}

input_list_two <- data_rankings_two |> select(id, rank_1, rank_2, logical_pass)
input_list_two$id <- NULL
data_rankings_two$error_type <- pmap_int(input_list_two, judge_error_two)

### 2. partial ordering, using one field (if_ties = 1)
data_rankings_one <- data_rankings |> filter(if_ties == 1) 

data_rankings_one$error_type <- NA

data_rankings_updated <- rbind(data_rankings_four_ties, data_rankings_three, 
                               data_rankings_two, data_rankings_one) 
#1 #stacked illogical rankings
#0  #interlaced illogical rankings
#2 #other illogical rankings
clean_rank <- function (rank) {
  if (length(rank) == 1) {
    cleaned_str = toString(rank[1])
  } else if (length(rank) == 2) {
    cleaned_str = paste(rank[1], "&", rank[2])
  }  else if (length(rank) == 3) {
    cleaned_str = paste(rank[1], "&", rank[2], "&", rank[3])
  }  else if (length(rank) == 4) {
    cleaned_str = paste(rank[1], "&", rank[2], "&", rank[3], "&", rank[4])
  }
} 

data_rankings_updated <- data_rankings_updated |> 
  mutate(rank_1 = sapply(rank_1, clean_rank),
         rank_2 = sapply(rank_2, clean_rank),
         rank_3 = sapply(rank_3, clean_rank),
         rank_4 = sapply(rank_4, clean_rank)) 

data_rankings_updated$if_ties <- as.numeric(unlist(data_rankings_updated$if_ties))

# add variable to indicate if the answer to "logical rule" question correct or not --------
data_logic <- data |> 
  filter(id %in% data_rankings$id) |> 
  select(id, `rank A-B-nichtB-nichtA` : `rank nicht-A-A-nicht-B-B`) |> 
  drop_na() |>
  group_by(id) |> 
  mutate(get_logic = 
           ifelse(`rank A-B-nichtB-nichtA` == TRUE &&
                    `rank A-nicht-A-nicht-B-B` == FALSE && 
                    `rank A-B-nicht-A-nicht-B` == FALSE &&
                    `rank B-A-nicht-A-nicht-B` == TRUE && 
                    `rank B-A-nicht-B-nicht-A` == FALSE &&
                    `rank nicht-A-A-nicht-B-B` == FALSE &&
                    `rank nicht-A-nicht-B-B-A` == TRUE,
                  TRUE, FALSE))

# Right Join (keep all df2 ids, fill NA where df1 has missing values)
merge_df <- data_rankings_updated |> right_join(data_logic, by = "id") 

write.csv(merge_df, "result_ranking.csv", row.names = FALSE)


# demographic information -------------------------------------------------
data_demo <- data |> 
filter(id %in% data_rankings$id) |> 
  select(id, age, gender, language, level_education, profession, subject) |>
  drop_na()

write.csv(data_demo, "data_demo.csv", row.names = FALSE)

