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


my_path <- "..\\yt"
response_path <- paste0(my_path, "/responses/")



# Data Import and Merge ###############################################

## Get original and auto abstract files
data <- readRDS(paste0(my_path, "/sample.rds"))



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
saveRDS(code_data, paste0(my_path, "response_data.rds"))




# Merge Responses and Data ####################################################

## If 2 initial coders agreed, final outcome = their agreed choice
two_agree <- code_data %>%
    group_by(s.no.) %>%
    mutate(count = length(c(micro, topic))) %>%
    filter(count == 1) %>%
    # check that coding decisions agree
    mutate(unique_codes = n_distinct(c(micro, topic)))

table(two_agree$unique_codes) # all the same

# final UID & code choice only
two_agree <- two_agree %>%
    distinct(s.no., .keep_all = T) %>%
    select(-name, -count, -unique_codes) 


## Bind final decisions together
final_choices <- bind_rows(two_agree) # 5876 records

length(unique(final_choices$s.no.)) # all unique
length(unique(data$s.no.)) # vs. 5880 in original

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
write.csv(code_tbl1, paste0(my_path, "codingchoice1_summary.csv"), row.names=F)

code_tbl2 <- final_data %>% group_by(title) %>% 
  summarise(n = n(),
            on_topic = sum(topic=="Yes", na.rm=T),
            off_topic = sum(topic=="No", na.rm=T),
            not_sure = sum(topic=="I'm not sure", na.rm=T)
  ) 
write.csv(code_tbl2, paste0(my_path, "codingchoice2_summary.csv"), row.names=F)



## Save full coded and merged data
saveRDS(final_data, paste0(my_path, "dataset_coded_clean.rds"))
