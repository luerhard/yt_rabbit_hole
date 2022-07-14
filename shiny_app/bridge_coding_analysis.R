# ******* Post-Coding Complication and Data Cleaning ************

# This script imports the data used for coding and combines them into a single data frame. It then
# compiles all responses into a single object, determines the final
# code choice for each video, and adds this to the merged data
# frame. 
# Output: combined responses and compiled + coded + cleaned data.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(here)

here::i_am("README.md")

graph_name <- "adrenochrome"

graph_file <- paste0(graph_name, ".gml")

my_path <- here()
response_path <- here(my_path, "shiny_app/responses/")



# Data Import and Merge ###############################################

## Get original and auto abstract files
g <-  read_graph(here("data/clean/networks", graph_file), format = "gml")
data <- as_data_frame(g,"vertices")
data <- input_file %>%
  mutate(
    url = paste0("https://www.youtube.com/watch?v=", label),
    s.no. = label
  )


# Compile Responses ###########################################################

# Load responses (slow)
code_data <- map(list.files(response_path, full.names = T),
                 function(x) {
                     read_csv(x, col_names = T,
                              skip_empty_rows = T,
                              col_types = "ccd")
                 }
)
code_data <- bind_rows(code_data)

# Save responses as single data frame
saveRDS(code_data, here(my_path, "response_data.rds"))




# Merge Responses and Data ####################################################

## If 2 initial coders agreed, final outcome = their agreed choice
two_agree <- code_data %>%
  group_by(s.no.) %>%
  mutate(count = length(c(micro, topic))) %>%
  filter(count == 2) %>%
  # check that coding decisions agree
  mutate(unique_codes = n_distinct(c(micro, topic)))

table(two_agree$unique_codes) 

# final UID & code choice only
two_agree <- two_agree %>%
  distinct(s.no., .keep_all = T) %>% 
  select(-name, -count, -unique_codes) 

## If at least 3 agreed, final outcome = the choice 3 chose
three_agree <- code_data %>%
  group_by(s.no.) %>%
  mutate(count = length(c(micro, topic))) %>%
  filter(count == 3 & name != "Group") %>%
  # check that coding decisions agree
  mutate(unique_codes = n_distinct(c(micro, topic))) %>%
  # 2 code choices only 
  filter(unique_codes == 2) %>%
  # keep the choice at least 3 agree on
  count(code_choice) %>%
  filter(n == 3) %>%
  # delete extraneous variable
  select(-n)

length(unique(three_agree$s.no.)) 

## Bind final decisions together
final_choices <- bind_rows(two_agree,
                           three_agree) 

length(unique(final_choices$s.no.)) # all unique
length(unique(data$s.no.)) 

#################################################
##################################################
# To use only if we agree to code individually

## 1 coder = their agreed choice
one_agree <- code_data %>%
    group_by(s.no.) %>%
    mutate(count = length(c(micro, topic))) %>%
    filter(count == 1) %>%
    # check that coding decisions agree
    mutate(unique_codes = n_distinct(c(micro, topic)))

table(one_agree$unique_codes) # all the same

# final UID & code choice only
one_agree <- one_agree %>%
    distinct(s.no., .keep_all = T) %>%
    select(-name, -count, -unique_codes) 


## Bind final decisions together
final_choices2 <- bind_rows(one_agree) 

length(unique(final_choices2$s.no.)) 
length(unique(data$s.no.)) 

######################################
######################################

## Check missing UIDs
missing <- data$s.no.[!(data$s.no. %in% final_choices$s.no.)]
code_data[code_data$s.no. %in% missing,] %>% 
    arrange(s.no., micro, topic)


## Merge final decisions with main dataset
final_data <- left_join(data, final_choices, by = "s.no.")


## Some descriptive output
code_tbl1 <- final_data %>% group_by(title) %>% 
    summarise(n = n(),
              debunk = sum(micro=="Debunking", na.rm=T),
              neutral = sum(micro=="Neutral", na.rm=T),
              spread= sum(micro=="Spreading", na.rm=T),
              des_miss = sum(micro=="Description missing", na.rm=T),
              not_sure = sum(micro=="I'm not sure", na.rm=T)
    ) 
write.csv(code_tbl1, here(my_path, "codingchoice1_summary.csv"), row.names=F)

code_tbl2 <- final_data %>% group_by(title) %>% 
  summarise(n = n(),
            on_topic = sum(topic=="Yes", na.rm=T),
            off_topic = sum(topic=="No", na.rm=T),
            not_sure = sum(topic=="I'm not sure", na.rm=T)
  ) 
write.csv(code_tbl2, here(my_path, "codingchoice2_summary.csv"), row.names=F)



## Save full coded and merged data
saveRDS(final_data, here(my_path, "dataset_coded_clean.rds"))
