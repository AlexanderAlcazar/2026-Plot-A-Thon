#install necessary packages
#install.packages("janitor")
library(tidyverse)
library(janitor)
library(dplyr)
#----------------------------------Clean Data------------------------------------------
#load the original data set
gamer_data <- read.csv("C:/Users/alcaz/OneDrive/Desktop/R/Plot2026/data/Gamer Study Sample Data Raw.csv")

#list distinct values
distinct_list <- gamer_data %>%
  map(unique)

#clean column names to remove any trailing white space
gamer_data <- gamer_data %>% clean_names()

#check for duplicates
duplicates <- gamer_data %>%
  group_by(respondent_id) %>%
  filter(n() > 1)

#remove any duplicate rows with the same respondent id
gamer_data <- gamer_data %>%
  distinct(respondent_id, .keep_all = TRUE)

#list distinct values
distinct_list <- gamer_data %>%
  map(unique)

#convert 'yes' to 'TRUE'
#convert 'No' to 'False'
#convert is_player to logical values
gamer_data <- gamer_data %>%
  mutate(is_player = str_trim(is_player)) %>% 
  mutate(is_player = if_else(is_player == "yes", "TRUE", is_player)) %>% 
  mutate(is_player = if_else(is_player == "No", "FALSE", is_player)) %>% 
  mutate(is_player = as.logical(is_player))


#list distinct values
distinct_list <- gamer_data %>%
  map(unique)

#filter out under age players
gamer_data <- gamer_data %>%
  filter(age >= 18)

#fix inconsistent gender values 
gamer_data <- gamer_data %>%
  mutate(gender = str_trim(gender)) %>% 
  mutate(gender = if_else(gender == "femal", "Female", gender)) %>% 
  mutate(gender = if_else(gender == "MALE", "Male", gender)) %>% 
  mutate(gender = if_else(gender == "", NA, gender)) 

#convert integer values to boolean values 
gamer_data <- gamer_data %>% 
  mutate(plays_mobile = as.logical(plays_mobile)) %>% 
  mutate(plays_console = as.logical(plays_console)) %>% 
  mutate(plays_pc = as.logical(plays_pc)) %>% 
  mutate(plays_vr = as.logical(plays_vr))

gamer_data <- gamer_data %>% 
  mutate(motivation_relax_or_pass_time = as.logical(motivation_relax_or_pass_time)) %>% 
  mutate(motivation_have_fun = as.logical(motivation_have_fun))

gamer_data <- gamer_data %>% 
  mutate(race_ethnicity_primary = str_trim(race_ethnicity_primary)) %>% 
  mutate(race_ethnicity_primary = if_else(race_ethnicity_primary == "", NA, race_ethnicity_primary)) %>% 
  mutate(race_ethnicity_primary = if_else(race_ethnicity_primary == "Whtie", "White/Caucasian", race_ethnicity_primary)) %>% 
  mutate(lgbt_identification = if_else(lgbt_identification == "Prefer not to say", NA, lgbt_identification)) %>% 
  mutate(lgbt_identification = if_else(lgbt_identification == "", NA, lgbt_identification))

#list distinct values
distinct_list <- gamer_data %>%
  map(unique)

write_csv(gamer_data, "output/data_clean.csv")
#----------------------------------Analyze/Plot Data-----------------------------------
Data---------------------------------------
social_analysis <- gamer_data %>%
  select(
    has_disability,
    agree_games_introduce_new_friends_binary,
    games_help_stay_connected,
    plays_with_others_ever,
    has_communicated_online,
    met_close_friend_spouse_partner
  )

#prepare the data for plotting
social_counts <- social_analysis %>%
  #pivot columns into two: 'category' (the question) and 'response' (TRUE/FALSE)
  pivot_longer(
    cols = -has_disability, 
    names_to = "category", 
    values_to = "response"
  ) %>%
  #filter to only keep 'TRUE' responses so we are counting the "Yes" answers
  filter(response == TRUE) %>%
  #rename the categories to be human-readable for the poster
  mutate(category = case_when(
    category == "agree_games_introduce_new_friends_binary" ~ "Introduces New Friends",
    category == "games_help_stay_connected" ~ "Helps Stay Connected",
    category == "plays_with_others_ever" ~ "Plays with Others",
    category == "has_communicated_online" ~ "Communicates Online",
    category == "met_close_friend_spouse_partner" ~ "Met Close Friend/Partner",
    TRUE ~ category
  ))

#create the Vertical Count Bar Graph
graph1 <- ggplot(social_counts, aes(x = reorder(category, category, function(x) -length(x)), fill = has_disability)) +
  geom_bar(position = "stack") + # 'stack' shows total count, split by group
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#B0B0B0"), # Purple for Disabled, Grey for others
    labels = c("TRUE" = "Disabled", "FALSE" = "Non-Disabled"),
    name = "Player Group"
  ) +
  labs(
    title = "How Gaming Connects Us",
    subtitle = "Total number of players who agreed with each social indicator",
    x = "Social Category",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    #rotate the labels so they fit vertically
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

#calculate percentages
social_percent <- social_analysis %>%
  pivot_longer(
    cols = -has_disability, 
    names_to = "category", 
    values_to = "response"
  ) %>%
  group_by(has_disability, category) %>%
  #calculate mean and multiply by 100 to get percentage
  summarize(percent = mean(response, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(category = case_when(
    category == "agree_games_introduce_new_friends_binary" ~ "Introduces New Friends",
    category == "games_help_stay_connected" ~ "Helps Stay Connected",
    category == "plays_with_others_ever" ~ "Plays with Others",
    category == "has_communicated_online" ~ "Communicates Online",
    category == "met_close_friend_spouse_partner" ~ "Met Close Friend/Partner",
    TRUE ~ category
  ))

#create the percentage bar graph
graph2 <- ggplot(social_percent, aes(x = reorder(category, percent), y = percent, fill = has_disability)) +
  geom_bar(stat = "identity", position = "dodge") + # 'dodge' puts bars side-by-side
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#B0B0B0"),
    labels = c("TRUE" = "Disabled Players", "FALSE" = "Non-Disabled Players"),
    name = "Group"
  ) +
  labs(
    title = "Social Connectivity: Is There a Disability Divide?",
    subtitle = "Percentage of each group who experience these social benefits",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#export the count graph (graph 1)
ggsave(
  filename = "output/social_connectivity_counts.png", 
  plot = graph1, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)

#export the percentage graph (graph 2)
ggsave(
  filename = "output/social_connectivity_percentages.png", 
  plot = graph2, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)

accessibility_pie_data <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  group_by(thinks_games_very_accessible) %>%
  summarise(count = n()) %>%
  mutate(
    percent = count / sum(count) * 100,
    label = paste0(round(percent, 1), "%")
  )

#Create the Pie Chart
pie_graph <- ggplot(accessibility_pie_data, aes(x = "", y = percent, fill = thinks_games_very_accessible)) +
  geom_bar(stat = "identity", width = 1, color = "white") + # white border makes it look cleaner
  coord_polar("y", start = 0) +
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#FF4B4B"), # Purple vs Red
    labels = c("TRUE" = "Games are Accessible", "FALSE" = "Barriers Still Exist"),
    name = "Opinion"
  ) +
  #Add the percentage text directly onto the pie slices
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 6) +
  labs(
    title = "The Accessibility Divide",
    subtitle = "Opinion among players with disabilities on whether games are 'Very Accessible'",
    caption = "Data represents respondents who identified as having a disability"
  ) +
  theme_void() + #Removes background, axes, and gridlines for a clean infographic look
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
ggsave(
  filename = "output/accesibility_pie.png", 
  plot = pie_graph, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)

#Prepare data: Filter for disabled players and select type columns
disability_reasons <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  select(thinks_games_very_accessible, starts_with("disability_")) %>%
  # Pivot to 'long' format to compare types easily
  pivot_longer(cols = starts_with("disability_"), 
               names_to = "disability_type", 
               values_to = "has_type")

# 2. Calculate percentages of each type within the two groups
type_comparison <- disability_reasons %>%
  group_by(thinks_games_very_accessible, disability_type) %>%
  summarize(percent = mean(has_type, na.rm = TRUE) * 100) %>%
  # Clean up labels for the graph
  mutate(disability_type = str_remove(disability_type, "disability_"),
         disability_type = str_to_title(disability_type))

#Create a Vertical Bar Graph to show the difference
ggplot(type_comparison, aes(x = disability_type, y = percent, fill = thinks_games_very_accessible)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#FF4B4B"),
    labels = c("TRUE" = "Thinks Games are Accessible", "FALSE" = "Thinks Barriers Exist"),
    name = "Opinion"
  ) +
  labs(
    title = "The Source of the Split",
    subtitle = "Percentage of each disability type within the two opinion groups",
    x = "Disability Type",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 1. Create the comparison groups
disabled_comparison <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  mutate(group = if_else(disability_cognitive == TRUE | disability_auditory == TRUE, 
                         "Cognitive/Auditory", "Other Disabilities"))

# 2. Summarize the two key metrics that show the difference
comparison_summary <- disabled_comparison %>%
  group_by(group) %>%
  summarize(
    `New Friends` = mean(agree_games_introduce_new_friends_binary, na.rm = TRUE) * 100,
    `Staying Connected` = mean(games_help_stay_connected, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(-group, names_to = "Metric", values_to = "Percent")

# 3. Plot the Difference
ggplot(comparison_summary, aes(x = Metric, y = Percent, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Cognitive/Auditory" = "#5D3FD3", "Other Disabilities" = "#B0B0B0")) +
  labs(
    title = "Connection vs. Maintenance",
    subtitle = "Cognitive & Auditory players excel at making new friends but face hurdles in staying connected.",
    x = "", y = "Percentage (%)"
  ) +
  theme_minimal()
# 1. Calculate dynamic values for the Call to Action
# We isolate the community with disabilities to get their specific "Power Facts"
cta_metrics <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  summarize(
    # Percentage who agree games introduce new friends
    new_friends = mean(agree_games_introduce_new_friends_binary, na.rm = TRUE) * 100,
    
    # Percentage who rate 'Games bring people together' as a 4 or 5 (Joy/Social Impact)
    social_joy = mean(att_bring_people_together_likert >= 4, na.rm = TRUE) * 100,
    
    # Percentage who believe barriers still exist (thinks_games_very_accessible is FALSE)
    barriers = mean(thinks_games_very_accessible == FALSE, na.rm = TRUE) * 100
  )

# 2. Create the data frame using the values we just calculated
cta_data <- data.frame(
  Fact = c("Find New Friends", "Experience Social Joy", "Report Accessibility Barriers"),
  Value = c(cta_metrics$new_friends, cta_metrics$social_joy, cta_metrics$barriers),
  Color_Type = c("Positive", "Positive", "Action")
)

# 3. Create the final "Closing Statement" plot (No changes needed here, it uses cta_data)
cta_graph <- ggplot(cta_data, aes(x = reorder(Fact, Value), y = Value, fill = Color_Type)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#5D3FD3", "Action" = "#FF4B4B")) +
  # We use round() so the labels on the poster don't have too many decimals
  geom_text(aes(label = paste0(round(Value), "%")), hjust = 1.5, color = "white", fontface = "bold") +
  labs(
    title = "The Path Forward",
    subtitle = "We have the connection. Now we need the access.",
    x = "",
    y = "Percentage of Disabled Players (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank()
  )


# 4. Save the graph
ggsave(
  filename = "output/call_to_action_graph.png", 
  plot = cta_graph, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)

# 1. Isolate the importance features for disabled players
accessibility_needs <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  # Select only the importance columns
  select(starts_with("acc_importance_")) %>%
  # Pivot to long format for easy calculation
  pivot_longer(everything(), names_to = "feature", values_to = "is_important") %>%
  # Calculate the percentage of players who want each feature
  group_by(feature) %>%
  summarize(percent = mean(is_important, na.rm = TRUE) * 100) %>%
  # Clean up the names for your poster
  mutate(feature = str_remove(feature, "acc_importance_"),
         feature = str_replace_all(feature, "_", " "),
         feature = str_to_title(feature))

# 2. Create the "Priorities for Progress" Graph
road_map <- ggplot(accessibility_needs, aes(x = fct_reorder(feature, percent), y = percent)) +
  geom_col(fill = "#5D3FD3") + # Use your signature purple
  coord_flip() +
  labs(
    title = "A Roadmap for Improvement",
    subtitle = "Top accessibility features requested by players with disabilities",
    x = "",
    y = "Percentage of Community Identifying as 'Very Important' (%)"
  ) +
  theme_minimal()

ggsave(
  filename = "output/road_map.png", 
  plot = road_map, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)


graph1
graph2
pie_graph
cta_graph

