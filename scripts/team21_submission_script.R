library(tidyverse)
library(janitor)

# --- LOAD AND CLEAN DATA ---

# Pulling in the raw survey results
gamer_data <- read.csv("C:/Users/writing.tutors/Downloads/Gamer Study Sample Data Raw.csv")

# Standardize column names (lowercase, no spaces)
gamer_data <- gamer_data %>% clean_names()

# Cleaning up duplicates based on ID
gamer_data <- gamer_data %>%
  distinct(respondent_id, .keep_all = TRUE)

# Filtering for adults and fixing inconsistent strings
gamer_data <- gamer_data %>%
  filter(age >= 18) %>%
  mutate(
    # Converting is_player to actual TRUE/FALSE
    is_player = str_trim(is_player),
    is_player = if_else(is_player == "yes", "TRUE", is_player),
    is_player = if_else(is_player == "No", "FALSE", is_player),
    is_player = as.logical(is_player),
    
    # Fixing gender typos
    gender = str_trim(gender),
    gender = if_else(gender == "femal", "Female", gender),
    gender = if_else(gender == "MALE", "Male", gender),
    gender = if_else(gender == "", NA, gender),
    
    # Fixing race/ethnicity typos
    race_ethnicity_primary = str_trim(race_ethnicity_primary),
    race_ethnicity_primary = if_else(race_ethnicity_primary == "Whtie", "White/Caucasian", race_ethnicity_primary),
    race_ethnicity_primary = if_else(race_ethnicity_primary == "", NA, race_ethnicity_primary),
    
    # Cleaning LGBT column
    lgbt_identification = if_else(lgbt_identification %in% c("Prefer not to say", ""), NA, lgbt_identification)
  )

# Turning 1/0 values into logical TRUE/FALSE for easier plotting
gamer_data <- gamer_data %>% 
  mutate(across(c(starts_with("plays_"), starts_with("motivation_")), as.logical))


# --- SOCIAL CONNECTIVITY ANALYSIS ---

# Selecting metrics for social story
social_analysis <- gamer_data %>%
  select(
    has_disability,
    agree_games_introduce_new_friends_binary,
    games_help_stay_connected,
    plays_with_others_ever,
    has_communicated_online,
    met_close_friend_spouse_partner
  )

# Reshaping data to plot all metrics together
social_long <- social_analysis %>%
  pivot_longer(cols = -has_disability, names_to = "category", values_to = "response") %>%
  mutate(category = case_when(
    category == "agree_games_introduce_new_friends_binary" ~ "Introduces New Friends",
    category == "games_help_stay_connected" ~ "Helps Stay Connected",
    category == "plays_with_others_ever" ~ "Plays with Others",
    category == "has_communicated_online" ~ "Communicates Online",
    category == "met_close_friend_spouse_partner" ~ "Met Close Friend/Partner"
  ))


# Graph 1: Total counts of players agreeing with social metrics
graph1 <- social_long %>%
  filter(response == TRUE) %>%
  ggplot(aes(x = fct_infreq(category), fill = has_disability)) +
  geom_bar() +
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#eb9b34"),
    labels = c("TRUE" = "Disabled", "FALSE" = "Non-Disabled"),
    name = "Player Group"
  ) +
  labs(title = "How Gaming Connects Us", y = "Number of Respondents", x = "") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Graph 2: Side-by-side percentage comparison
social_perc <- social_long %>%
  group_by(has_disability, category) %>%
  summarize(percent = mean(response, na.rm = TRUE) * 100)

graph2 <- ggplot(social_perc, aes(x = fct_reorder(category, -percent), y = percent, fill = has_disability)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#eb9b34"),
    labels = c("TRUE" = "Disabled", "FALSE" = "Non-Disabled"),
    name = "Group"
  ) +
  labs(title = "Social Connectivity: A Universal Benefit?", y = "Percentage (%)", x = "") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")


# --- ACCESSIBILITY DEEP DIVE ---

# Looking at perception split among disabled players only
pie_data <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  group_by(thinks_games_very_accessible) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100, label = paste0(round(percent, 2), "%"))

# Create the Pie Chart
pie_graph <- ggplot(pie_data, aes(x = "", y = percent, fill = thinks_games_very_accessible)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) + # This is the line that makes it a circle
  scale_fill_manual(
    values = c("TRUE" = "#5D3FD3", "FALSE" = "#eb9b34"),
    labels = c("TRUE" = "Accessible", "FALSE" = "Barriers Exist"),
    name = "Opinion"
  ) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 7) +
  labs(title = "The Accessibility Divide") +
  theme_void(base_size = 18) + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

# Ranking what needs to improve (Roadmap)
roadmap_data <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  select(starts_with("acc_importance_")) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "important") %>%
  group_by(feature) %>%
  summarize(percent = mean(important, na.rm = TRUE) * 100) %>%
  mutate(feature = str_to_title(str_replace_all(str_remove(feature, "acc_importance_"), "_", " ")))

road_map <- ggplot(roadmap_data, aes(x = fct_reorder(feature, percent), y = percent)) +
  geom_col(fill = "#5D3FD3") +
  coord_flip() +
  labs(title = "A Roadmap for Improvement", x = "", y = "Importance (%)") +
  theme_minimal(base_size = 18)


# --- THE CALL TO ACTION ---

# Pulling dynamic stats for the conclusion
cta_stats <- gamer_data %>%
  filter(has_disability == TRUE) %>%
  summarize(
    new_friends = mean(agree_games_introduce_new_friends_binary, na.rm = TRUE) * 100,
    social_joy = mean(att_bring_people_together_likert >= 4, na.rm = TRUE) * 100,
    barriers = mean(thinks_games_very_accessible == FALSE, na.rm = TRUE) * 100
  )

cta_df <- data.frame(
  Fact = c("Find New Friends", "Experience Social Joy", "Report Barriers"),
  Value = c(cta_stats$new_friends, cta_stats$social_joy, cta_stats$barriers),
  Type = c("Positive", "Positive", "Action")
)

cta_graph <- ggplot(cta_df, aes(x = reorder(Fact, Value), y = Value, fill = Type)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#5D3FD3", "Action" = "#eb9b34")) +
  geom_text(aes(label = paste0(round(Value), "%")), hjust = 1.5, color = "white", fontface = "bold") +
  labs(title = "The Path Forward", x = "", y = "Percent of Disabled Players (%)") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none")


# --- EXPORTS ---

ggsave("output/social_counts.png", graph1, width = 10, height = 8)
ggsave("output/social_percentages.png", graph2, width = 10, height = 8)
ggsave("output/accessibility_pie.png", pie_graph, width = 8, height = 8)
ggsave("output/road_map.png", road_map, width = 10, height = 8)
ggsave("output/cta_summary.png", cta_graph, width = 10, height = 8)

# End of Script