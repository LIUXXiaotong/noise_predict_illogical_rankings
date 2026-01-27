# R preparation -----------------------------------------------------------
#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
} 
library("pacman")
p_load(tidyverse, jsonlite, psych, ggridges, knitr, kableExtra, corrplot, gridExtra)

##read in original .txt file and remove personally identifiable information 
#data0 <- read_file('raw_noise_results.txt') |> # Read the text file from JATOS ...
#  str_split('\n') |> first() |>   # ... split it into lines ...
#  discard(function(x) x == '') |>   # ... filter empty rows ...
#  map_dfr(fromJSON, flatten=T) # ... parse JSON into a data.frame
#
#mapping <- read_csv("mapping.csv")
#
#data0$prolific_ID_url <- as.character(data0$prolific_ID_url)
#mapping$prolific_id <- as.character(mapping$prolific_id)
#
#data0 <- data0 |> 
#  left_join(mapping, by = c("prolific_ID_url" = "prolific_id")) |> 
#  rename(id = anon_id) |>
#  relocate(id, .before = everything()) |> 
#  select(-prolific_ID_url, -session_ID, -study_ID, -timestamp, -meta.location) |> 
#  select(-starts_with("...")) 
#save(data0, file = "part2_estimation_task.RData")
load("part2_estimation_task.RData")

data0$quality |> unique()
data0$comments |> unique()

# String manipulation, re-labeling the probabilistic events  --------------

## first count the number of labels assigned to a given event, by counting the number of commas 
data <- data0 |>
  mutate(count_commas = str_count(labelling, ",")) 

#select useful rows and columns for data analysis 
data <- data |> select(id, sender, answer, labelling, count_commas) |>
  filter(sender == "show_events") 

#split the data set according to the number of labels (commas) assigned to the probabilistic event 
data_count_commas_0 <- data |> filter(count_commas == 0) |> 
  pivot_wider(names_from = labelling,
              values_from = answer)

data_count_commas_1 <- data |> filter(count_commas == 1) |> 
  mutate(answer2 = answer)  |>
  separate(labelling, into = c("labelling1", "labelling2"), sep = ",") |> 
  pivot_wider(names_from = labelling1,
              values_from = answer)  |> 
  pivot_wider(names_from = labelling2,
              values_from = answer2) |>
  group_by(id) |>
  fill(4:9, .direction = "updown") |> unique()

data_count_commas_2 <- data |> filter(count_commas == 2) |> 
  mutate(answer2 = answer,
         answer3 = answer) |>
  separate(labelling, into = c("labelling1", "labelling2", "labelling3"), sep = ",") |> 
  pivot_wider(names_from = labelling1,
              values_from = answer)  |> 
  pivot_wider(names_from = labelling2,
              values_from = answer2) |>
  pivot_wider(names_from = labelling3,
              values_from = answer3) |>
  group_by(id) |> 
  fill(4: 12, .direction = "updown")  |> unique()

#re-join the data
data_join <- inner_join(data_count_commas_0 |> select(-count_commas), 
                        data_count_commas_1 |> 
                          select(-count_commas, -sender), by = "id") |>
  inner_join(data_count_commas_2 |> 
               select(-count_commas, -sender), by = "id")

#glimpse(data_join) #variable type `3aORb` is  <chr>
# Convert the columns/variables that represent participants' answers to numeric variables 
data_join[,3:50] <- lapply(data_join[,3:50], function(x) as.numeric(as.character(x)))

# data exclusion ----------------------------------------------------------
#check the response time and how participants use the probability scale 
data_exclusion <- data0 |> 
  select(id, sender, answer, labelling, duration) |> 
  filter(sender == "show_events") |>
  group_by(id) |>
  mutate(no_trials = n(),
         unique_answer_count = n_distinct(answer)) |>
  #select(ID, unique_answer_count) |> 
  unique()   |> 
  filter(unique_answer_count <= 3) |> 
  select(id, unique_answer_count) |> #participants gave only a few data points (either "100" or "90") to queries/probabilistic events. 
  unique()

write.csv(data_exclusion, "exclusion_part2_IDs.csv", row.names = FALSE)

# Calculating the noise level "d" approximated using probabilistic identities  ---------------------------------------

#In this analysis, we use each participant's average values for expressions Z1E . . . Z6E to estimate a value of d, 
#the rate of random variation in recall for that participant. For each participant, we can compute 6 estimates for that participant's value of d, 
#by taking that participant's average value for each expression Z1E, . . . , Z6E (and dividing the averages for Z5E and Z6E by 2). 
data_join <- data_join |>
  mutate(z1_eventset1 = (`1a` + `1bNnota` - `1aORb`)/100,
         z1_eventset2 = (`2a` + `2bNnota` - `2aORb`)/100,
         z1_eventset3 = (`3a` + `3bNnota` - `3aORb`)/100,
         z1_eventset4 = (`4a` + `4bNnota` - `4aORb`)/100,
         z1_eventset5 = (`5a` + `5bNnota` - `5aORb`)/100,
         z1_eventset6 = (`6a` + `6bNnota` - `6aORb`)/100,
         z1_eventset7 = (`7a` + `7bNnota` - `7aORb`)/100,
         z1_eventset8 = (`8a` + `8bNnota` - `8aORb`)/100,
         z1_d = (z1_eventset1 + z1_eventset2 + z1_eventset3 + z1_eventset4 + 
                   z1_eventset5 + z1_eventset6 + z1_eventset7 + z1_eventset8)/8,
         z2_eventset1 = (`1b` + `1aNnotb` - `1aORb`)/100,
         z2_eventset2 = (`2b` + `2aNnotb` - `2aORb`)/100,
         z2_eventset3 = (`3b` + `3aNnotb` - `3aORb`)/100,
         z2_eventset4 = (`4b` + `4aNnotb` - `4aORb`)/100,
         z2_eventset5 = (`5b` + `5aNnotb` - `5aORb`)/100,
         z2_eventset6 = (`6b` + `6aNnotb` - `6aORb`)/100,
         z2_eventset7 = (`7b` + `7aNnotb` - `7aORb`)/100,
         z2_eventset8 = (`8b` + `8aNnotb` - `8aORb`)/100,
         z2_d = (z2_eventset1 + z2_eventset2 + z2_eventset3 + z2_eventset4 + 
                   z2_eventset5 + z2_eventset6 + z2_eventset7 + z2_eventset8)/8,
         z3_eventset1 = (`1aNnotb` + `1aNb` - `1a`)/100,
         z3_eventset2 = (`2aNnotb` + `2aNb` - `2a`)/100,
         z3_eventset3 = (`3aNnotb` + `3aNb` - `3a`)/100,
         z3_eventset4 = (`4aNnotb` + `4aNb` - `4a`)/100,
         z3_eventset5 = (`5aNnotb` + `5aNb` - `5a`)/100,
         z3_eventset6 = (`6aNnotb` + `6aNb` - `6a`)/100,
         z3_eventset7 = (`7aNnotb` + `7aNb` - `7a`)/100,
         z3_eventset8 = (`8aNnotb` + `8aNb` - `8a`)/100,
         z3_d = (z3_eventset1 + z3_eventset2 + z3_eventset3 + z3_eventset4 + 
                   z3_eventset5 + z3_eventset6 + z3_eventset7 + z3_eventset8)/8,
         z4_eventset1 = (`1bNnota` + `1aNb` - `1b`)/100,
         z4_eventset2 = (`2bNnota` + `2aNb` - `2b`)/100,
         z4_eventset3 = (`3bNnota` + `3aNb` - `3b`)/100,
         z4_eventset4 = (`4bNnota` + `4aNb` - `4b`)/100,
         z4_eventset5 = (`5bNnota` + `5aNb` - `5b`)/100,
         z4_eventset6 = (`6bNnota` + `6aNb` - `6b`)/100,
         z4_eventset7 = (`7bNnota` + `7aNb` - `7b`)/100,
         z4_eventset8 = (`8bNnota` + `8aNb` - `8b`)/100,
         z4_d = (z4_eventset1 + z4_eventset2 + z4_eventset3 + z4_eventset4 + 
                   z4_eventset5 + z4_eventset6 + z4_eventset7 + z4_eventset8)/8,
         z5_eventset1 = (`1aNnotb` + `1bNnota` + `1aNb` - `1aORb`)/2/100,
         z5_eventset2 = (`2aNnotb` + `2bNnota` + `2aNb` - `2aORb`)/2/100,
         z5_eventset3 = (`3aNnotb` + `3bNnota` + `3aNb` - `3aORb`)/2/100,
         z5_eventset4 = (`4aNnotb` + `4bNnota` + `4aNb` - `4aORb`)/2/100,
         z5_eventset5 = (`5aNnotb` + `5bNnota` + `5aNb` - `5aORb`)/2/100,
         z5_eventset6 = (`6aNnotb` + `6bNnota` + `6aNb` - `6aORb`)/2/100,
         z5_eventset7 = (`7aNnotb` + `7bNnota` + `7aNb` - `7aORb`)/2/100,
         z5_eventset8 = (`8aNnotb` + `8bNnota` + `8aNb` - `8aORb`)/2/100,
         z5_d = (z5_eventset1 + z5_eventset2 + z5_eventset3 + z5_eventset4 + 
                   z5_eventset5 + z5_eventset6 + z5_eventset7 + z5_eventset8)/8,
         z6_eventset1 = (`1aNnotb` + `1bNnota` + `1aNb` + `1aNb` - `1a` - `1b`)/2/100,
         z6_eventset2 = (`2aNnotb` + `2bNnota` + `2aNb` + `2aNb` - `2a` - `2b`)/2/100,
         z6_eventset3 = (`3aNnotb` + `3bNnota` + `3aNb` + `3aNb` - `3a` - `3b`)/2/100,
         z6_eventset4 = (`4aNnotb` + `4bNnota` + `4aNb` + `4aNb` - `4a` - `4b`)/2/100,
         z6_eventset5 = (`5aNnotb` + `5bNnota` + `5aNb` + `5aNb` - `5a` - `5b`)/2/100,
         z6_eventset6 = (`6aNnotb` + `6bNnota` + `6aNb` + `6aNb` - `6a` - `6b`)/2/100,
         z6_eventset7 = (`7aNnotb` + `7bNnota` + `7aNb` + `7aNb` - `7a` - `7b`)/2/100,
         z6_eventset8 = (`8aNnotb` + `8bNnota` + `8aNb` + `8aNb` - `8a` - `8b`)/2/100,
         z6_d = (z6_eventset1 + z6_eventset2 + z6_eventset3 + z6_eventset4 + 
                   z6_eventset5 + z6_eventset6 + z6_eventset7 + z6_eventset8)/8) #notice that I did not take any absolute values, 
#I just took the averages

#for each participant, calculate RMSD across the 6 estimates of d (simply SD, as the prediction is the mean)
data_d <- data_join |> 
  select(id, z1_eventset1, z1_eventset2, z1_eventset3, z1_eventset4, z1_eventset5, z1_eventset6, z1_eventset7, z1_eventset8, z1_d, 
         z2_eventset1, z2_eventset2, z2_eventset3, z2_eventset4, z2_eventset5, z2_eventset6, z2_eventset7, z2_eventset8, z2_d, 
         z3_eventset1, z3_eventset2, z3_eventset3, z3_eventset4, z3_eventset5, z3_eventset6, z3_eventset7, z3_eventset8, z3_d,
         z4_eventset1, z4_eventset2, z4_eventset3, z4_eventset4, z4_eventset5, z4_eventset6, z4_eventset7, z4_eventset8, z4_d, 
         z5_eventset1, z5_eventset2, z5_eventset3, z5_eventset4, z5_eventset5, z5_eventset6, z5_eventset7, z5_eventset8, z5_d, 
         z6_eventset1, z6_eventset2, z6_eventset3, z6_eventset4, z6_eventset5, z6_eventset6, z6_eventset7, z6_eventset8, z6_d)  |>
  rowwise() |>
  mutate(
    d_average = mean(c(z1_d, z2_d, z3_d, z4_d, z5_d, z6_d)),
    sd_d1=sd(c_across(c(z1_eventset1, z1_eventset2, z1_eventset3, z1_eventset4, z1_eventset5, z1_eventset6, z1_eventset7, z1_eventset8))),
    sd_d2=sd(c_across(c(z2_eventset1, z2_eventset2, z2_eventset3, z2_eventset4, z2_eventset5, z2_eventset6, z2_eventset7, z2_eventset8))),
    sd_d3=sd(c_across(c(z3_eventset1, z3_eventset2, z3_eventset3, z3_eventset4, z3_eventset5, z3_eventset6, z3_eventset7, z3_eventset8))),
    sd_d4=sd(c_across(c(z4_eventset1, z4_eventset2, z4_eventset3, z4_eventset4, z4_eventset5, z4_eventset6, z4_eventset7, z4_eventset8))),
    sd_d5=sd(c_across(c(z5_eventset1, z5_eventset2, z5_eventset3, z5_eventset4, z5_eventset5, z5_eventset6, z5_eventset7, z5_eventset8))),
    sd_d6=sd(c_across(c(z6_eventset1, z6_eventset2, z6_eventset3, z6_eventset4, z6_eventset5, z6_eventset6, z6_eventset7, z6_eventset8))),
    sd_d = sd(c_across(c(z1_d, z2_d, z3_d, z4_d, z5_d, z6_d)))
  ) |>
  select(id, d_average, sd_d,
         sd_d1, sd_d2, sd_d3, sd_d4, sd_d5, sd_d6,
         z1_d, z2_d, z3_d, z4_d, z5_d, z6_d) 

# -------------------------------------------------------------------------
#look at the distribution of RMSD of the estimates of "d" and the distribution of the average values of  d 
#only include participants whose RMSD d < average d ?


( hist_d_average <- ggplot(data_d, aes(x = d_average)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) + 
    geom_vline(aes(xintercept = mean(d_average)), 
               color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", x = mean(data_d$d_average) + 0.1,, y = 10, 
             label = paste("Mean =", round(mean(data_d$d_average), 3)),
             color = "red") +
    labs(title = "Distribution of Estimated d",
         x = "Value",
         y = "Count") +
    theme_minimal() )

( hist_sd_d <- ggplot(data_d, aes(x = sd_d)) +
    geom_histogram(bins = 50, fill = "coral", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(sd_d)), 
               color = "red", linetype = "dashed", linewidth = 1) + 
    annotate("text", x = mean(data_d$sd_d) + 10, y = 30, 
             label = paste("Mean =", round(mean(data_d$sd_d), 2)),
             color = "red") +
    labs(title = "Distribution of RMSD of Estimates",
         x = "Value",
         y = "Count") +
    xlim(-0.1, 1) + 
    theme_minimal() )

grid.arrange(hist_d_average, hist_sd_d, ncol = 1)

#use absolute values or not? 
#exclude participants whose RMSD of estimates > averaged value of d (prediction)
#not reliable predictions
#data_d <- data_d |> 
 # mutate(pass_z1 = ifelse(z1_d >= sd_d1, 1, 0),
  #       pass_z2 = ifelse(z2_d >= sd_d2, 1, 0),
   #      pass_z3 = ifelse(z3_d >= sd_d3, 1, 0),
    #     pass_z4 = ifelse(z4_d >= sd_d4, 1, 0),
     #    pass_z5 = ifelse(z5_d >= sd_d5, 1, 0),
      #   pass_z6 = ifelse(z6_d >= sd_d6, 1, 0),
       #  pass_sd = ifelse(abs(d_average) >= sd_d, 1, 0)) |>  #28 participants being excluded
  #filter(pass_sd == 1)  |> #28 participants being excluded 
  #select(-pass_z1, -pass_z2, -pass_z3, -pass_z4, -pass_z5, -pass_z6, -pass_sd) 


# conjunction and disjunction fallacy -------------------------------------
data_join_conj_disj <- data_join |> 
  mutate(conj_1 = ifelse(`1aNb` > `1a` |`1aNb` > `1b` | 
                           `1aNnotb` > `1a` | `1bNnota` > `1b`, 1, 0),
         disj_1 = ifelse(`1aORb` < `1a` |`1aORb` < `1b`, 1, 0),
         conj_2 = ifelse(`2aNb` > `2a` |`2aNb` > `2b`|
                           `2aNnotb` > `2a` | `2bNnota` > `2b`, 1, 0),
         disj_2 = ifelse(`2aORb` < `2a` |`2aORb` < `2b`, 1, 0),
         conj_3 = ifelse(`3aNb` > `3a` |`3aNb` > `3b`|
                           `3aNnotb` > `3a` | `3bNnota` > `3b`, 1, 0),
         disj_3 = ifelse(`3aORb` < `3a` |`3aORb` < `3b`, 1, 0),
         conj_4 = ifelse(`4aNb` > `4a` |`4aNb` > `4b`|
                           `4aNnotb` > `4a` | `4bNnota` > `4b`, 1, 0),
         disj_4 = ifelse(`4aORb` < `4a` |`4aORb` < `4b`, 1, 0),
         conj_5 = ifelse(`5aNb` > `5a` |`5aNb` > `5b`|
                           `5aNnotb` > `5a` | `5bNnota` > `5b`, 1, 0),
         disj_5 = ifelse(`5aORb` < `5a` |`5aORb` < `5b`, 1, 0),
         conj_6 = ifelse(`6aNb` > `6a` |`6aNb` > `6b`|
                           `6aNnotb` > `6a` | `6bNnota` > `6b`, 1, 0),
         disj_6 = ifelse(`6aORb` < `6a` |`6aORb` < `6b`, 1, 0),
         conj_7 = ifelse(`7aNb` > `7a` |`7aNb` > `7b`|
                           `7aNnotb` > `7a` | `7bNnota` > `7b`, 1, 0),
         disj_7 = ifelse(`7aORb` < `7a` |`7aORb` < `7b`, 1, 0),
         conj_8 = ifelse(`8aNb` > `8a` |`8aNb` > `8b`|
                           `8aNnotb` > `8a` | `8bNnota` > `8b`, 1, 0),
         disj_8 = ifelse(`8aORb` < `8a` |`8aORb` < `8b`, 1, 0)) |>
         #x1 = `1a` + `1b` - `1aNb` - `1aORb`,
         #x2 = `2a`+ `2b`- `2aNb`- `2aORb`,
         #x3 = `3a`+ `3b`- `3aNb`- `3aORb`,
         #x4 = `4a`+ `4b`- `4aNb`- `4aORb`,
         #x5 = `5a`+ `5b`- `5aNb`- `5aORb`,
         #x6 = `6a`+ `6b`- `6aNb`- `6aORb`,
         #x7 = `7a`+ `7b`- `7aNb`- `7aORb`,
         #x8 = `8a`+ `8b`- `8aNb`- `8aORb`,
         #y1 = `1a`+ `1bNnota`- `1b`- `1aNnotb`, 
         #y2 = `2a`+ `2bNnota`- `2b`- `2aNnotb`,
         #y3 = `3a`+ `3bNnota`- `3b`- `3aNnotb`,
         #y4 = `4a`+ `4bNnota`- `4b`- `4aNnotb`,
         #y5 = `5a`+ `5bNnota`- `5b`- `5aNnotb`,
         #y6 = `6a`+ `6bNnota`- `6b`- `6aNnotb`,
         #y7 = `7a`+ `7bNnota`- `7b`- `7aNnotb`,
         #y8 = `8a`+ `8bNnota`- `8b`- `8aNnotb`) |>
  select(id, `1a`, `1b`, `1aNb`, `1aORb`, `1aNnotb`,`1bNnota`, `1aNnotb`,`1bNnota`, conj_1, disj_1,
         `2a`, `2b`, `2aNb`, `2aORb`, `2aNnotb`,`2bNnota`, conj_2, disj_2,
         `3a`, `3b`, `3aNb`, `3aORb`, `3aNnotb`,`3bNnota`, conj_3, disj_3,
         `4a`, `4b`, `4aNb`, `4aORb`, `4aNnotb`,`4bNnota`, conj_4, disj_4,
         `5a`, `5b`, `5aNb`, `5aORb`, `5aNnotb`,`5bNnota`, conj_5, disj_5,
         `6a`, `6b`, `6aNb`, `6aORb`, `6aNnotb`,`6bNnota`, conj_6, disj_6,
         `7a`, `7b`, `7aNb`, `7aORb`, `7aNnotb`,`7bNnota`, conj_7, disj_7,
         `8a`, `8b`, `8aNb`, `8aORb`, `8aNnotb`,`8bNnota`, conj_8, disj_8) |>
          #x1 ,x2 ,x3 ,x4 ,x5 ,x6 ,x7, x8,
          #y1, y2, y3, y4, y5, y6, y7, y8) |> 
  mutate(number_conj = conj_1 + conj_2+ conj_3+ conj_4+ conj_5+ conj_6+ conj_7+ conj_8,
         number_disj = disj_1 + disj_2+ disj_3+ disj_4+ disj_5+ disj_6+ disj_7+ disj_8) |> 
         #mean_x = (x1 +x2 +x3 +x4 +x5 +x6 +x7 +x8) / 8,
         #mean_y = (y1 +y2 +y3 +y4 +y5 +y6 +y7 +y8) / 8)  |> 
  mutate(number_conj_disj = number_conj + number_disj)
         #mean_xy = mean_x + mean_y)

# combining the two data sets  --------------------------------------------

data_d <- data_d |> 
  select(id, z1_d, z2_d, z3_d, z4_d, z5_d, z6_d, d_average, sd_d)

d_conj_disj_combined <- data_join_conj_disj |>
  left_join(data_d, by = "id") 

write.csv(d_conj_disj_combined, "noise_conj_disj.csv", row.names = FALSE)
