library(tidyverse)
library(RSQLite)
library(readxl) 
library(scales)

SB_og <- read.csv('https://storage.googleapis.com/kagglesdsdata/datasets/2424110/4098789/super_bowls.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20241213%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20241213T013011Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=6582ca04493e78e3081221cc14b3b6a15e856ec4aa7a98a6b252e62971050da8766673b829dfc92732a9c9591ac2b1fc5daa729ab3968119baabdcfcdac98a5431f6de015b0bf22e47818e9d5d27843909dbc5fab0af01abd56225dca7cbb770174018acd4ad7eaba6c0effc3f99990052c86ddc061e1364a017de425dd733a7cd21f928b02f2c65aae6bf66056c3b90da351ff56c9ac9f89227e94de22cbcf7c55aea53dc410957e543d17ba732d9aa75f135a0bbf2b93e23329f4ffea017425dd306c660c9b5e7669bc606b65b1ec425703a8f4889d6567ad9af79c420f7c94459c59731f35ab694cbbaa10c447e177d14059c19b7282e3e751a3dd31e670f')
TV <- read.csv('https://storage.googleapis.com/kagglesdsdata/datasets/2424110/4098789/tv.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20241213%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20241213T013058Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=142dc9eda091f262616579a84eaf233f35f6f196c4a91ac0e70c87f11a317315a62a00c579d7ef5f7c6eee2d7f48ffe75f37f61d5826909371e6b633f177eceb6ea58672a9d1420c968e79f6b3e5908a6c3f53602646fe84f596b87de1b1bff67de39ab3f02b75cc455ba3d94e9ac5c0c65922149ade6f2c384855035cecc11fc53dc13650aedc62bb06154693336e5eb963b9b60208ceda1755a8349521058bc797f08e1b00be05b59ad1d9e7195816a897d7fba0615bab2da0bae54687103dd06e6ca291efff6ca7eb6f799c240af821d196cc0981d3842739b8ed4db708ad87554727b6a4e9cbd512561ba90936ed80dd5da6c51abd31ec53c46a9620268b')
conferences <- read.csv('https://gist.githubusercontent.com/cnizzardini/13d0a072adb35a0d5817/raw/f315c97c7677845668a9c26e9093d0d550533b00/nfl_teams.csv')
mvp <- read.csv('https://storage.googleapis.com/kagglesdsdata/datasets/587171/1059362/mvpsb.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20241215%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20241215T214442Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=8029714ff39b27ae1f077f0bf0a245004fd53a27a44a4f0196ea7288fed87b9d484e5cd46a66dfa9b69274f12edfd8f7215981b115a9efe9fbbd0b4fbc72fa3397075d6190f3e34a0c59134c878ab4db730b3679f759dd650ddf119036cf650628db42803a5b25e2d54f39205b497aa6e9389e58f67526bde786c1b806377af02848e06d8325e57c4a2fe1ef9116fa8d30f7091de28b5bd4da5d6f29180eca63c4e57c82c72c4dd679ca96aa867ec8c41d4690bd1cf1926fbf13d2e4cb123131324a4670acc3daceca648470ebb23e4e7f010277c976df0e6bb47957fe0c4908fa4eac8375887ddc3c9fe1dd31f19322aec8ab15189a52407d68cf51786c9796')
ticket_price <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/USD 4th Yr/S1/BUAN 314/SB_TicketPrice.csv")

#cleaning data 
SB0 <- select(SB_og,-qb_winner_2,-qb_loser_2,-qb_loser_1)
TV <- select(TV,super_bowl,network,avg_us_viewers,ad_cost)
conferences <- select(conferences,Name,Conference,Division)
mvp <- select(mvp,SB,PLAYER,POSITION)
mvp$SB <- gsub('[^0-9]',"",mvp$SB)
 #merging data
SB1 <- merge(TV,SB0, by.x='super_bowl', by.y='super_bowl')
SB2 <- merge(conferences,SB1,by.x='Name',by.y='team_winner', all.y = TRUE)
SB <- merge(mvp,SB2,by.x='SB',by.y='super_bowl')
SB <- SB %>% rename(MVP = PLAYER)
SB$date <- as.Date(SB$date)
SB$SB <- as.numeric(SB$SB)
SB <- merge(SB,team_wins, by.x='Name',by.y='Name', all.x = TRUE)
SB <- merge(SB,ticket_price, by.x='SB', by.y='SB')



#new data set (NO NA)

SB <- read.csv("~/Downloads/SB4xlsx.csv")
SB$date <- as.Date(SB$date)

SB$SB <- as.numeric(SB$SB)

#1
ggplot(SB, aes(x = date, y = combined_pts)) + 
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") + 
  labs(
    title = "Trend of Total Points Scored in Super Bowl Games Over the Years",
    x = "Date",
    y = "Total Points"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#2
stadium_counts <- SB2 %>%
  group_by(venue) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
print(stadium_counts)
ggplot(data = stadium_counts, aes(x = reorder(venue, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Super Bowl Hosting Frequency by Stadium",
    x = "Stadium",
    y = "Number of Super Bowls Hosted"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#3
SB <- SB %>%
  mutate(point_diff = abs(winning_pts - losing_pts),
         close_game = ifelse(point_diff <= 7, "Close Game", "Blowout"))
ggplot(SB, aes(x = close_game, y = avg_us_viewers, fill = close_game)) +
  geom_boxplot() +
  labs(title = "TV Ratings vs Game Competitiveness", x = "Game Type", y = "TV Ratings (in thousands)") +
  scale_y_continuous(labels = label_number(scale = 0.001),limits = c(0, NA))+
  theme_minimal()

# Conduct t-test
t_test_result <- t.test(avg_us_viewers ~ close_game, data = SB)
print(t_test_result)


#4
team_wins <- SB %>% 
  group_by(Name) %>%
  summarise(Win_Count = n ()) %>%
  arrange(desc(Win_Count))
print(team_wins)

ggplot(team_wins, aes(x = reorder(Name, -Win_Count), y = Win_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Most Successful Teams in Super Bowl History",
       x = "NFL Team",
       y = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#5
#calc margins 
SB <- SB %>%
  mutate(margin_of_victory = abs(winning_pts - losing_pts))
# Calculate average margin of victory
average_margin <- mean(SB$margin_of_victory, na.rm = TRUE)
# Identify closest and widest margin games
closest_game <- SB %>%
  filter(margin_of_victory == min(margin_of_victory, na.rm = TRUE))
widest_game <- SB %>%
  filter(margin_of_victory == max(margin_of_victory, na.rm = TRUE))
# Display results
cat("Average Margin of Victory:", average_margin, "\n")
cat("Closest Game:\n")
print(closest_game)
cat("Widest Margin Game:\n")
print(widest_game)
# Visualization: Margin of Victory Over Time
ggplot(SB, aes(x = SB, y = margin_of_victory)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Margin of Victory Over Time in Super Bowl Games",
       x = "Super Bowl",
       y = "Margin of Victory") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#6 
#plot trend of avg viewership
ggplot(SB,aes(x= SB, y = avg_us_viewers)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Trend of Average Viewership Over the Years", 
    x = "SuperBowl",
    y = "Average Viewers (millions)"
  )+
  theme_minimal()
# correlation between ad cost and avg viewership
correlation <- cor(SB$avg_us_viewers, SB$ad_cost, use = 'complete.obs')
print(paste("correlation between average viewers and ad cost:", correlation))
#scatter plot
ggplot(SB, aes(x = avg_us_viewers, y = ad_cost)) +
  geom_point(color = 'purple') +
  geom_smooth(method = "lm", color = "black") + 
  labs(
    title = "Ad Cost vs Average Viewers", 
    x = "Average Viewers (millions)",
    y = "Ad Cost (millions)"
  ) +
  scale_y_continuous(labels = label_number(scale = 0.001),limits = c(0, NA)) +
  scale_x_continuous(labels = label_number(scale = 0.001)) + 
  theme_minimal()

#7 
conference_wins <- SB %>% 
  group_by(Conference) %>%
  summarise(total_wins = n(), .groups = "drop")

ggplot(conference_wins, aes(x = Conference, y = total_wins, fill = Conference)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Super Bowl Wins by Conference",
    x = "Conference",
    y = "Total Wins"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("AFC" = "steelblue", "NFC" = "darkorange"))



#8
position_counts <- mvp %>%
group_by(POSITION) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Plot the data
ggplot(position_counts, aes(x = reorder(POSITION, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Super Bowl MVPs by Position",
       x = "Position",
       y = "Number of MVPs") +
  theme_minimal()


#11
SB <- SB %>%
  mutate(avg_ticket_price = parse_number(`avg._ticket_price`))

correlation_ticket_price_attendance <- SB %>%
  select(attendance, avg_ticket_price) %>%
  summarise(correlation = cor(attendance, avg_ticket_price, use = "complete.obs"))
print(correlation_ticket_price_attendance)

correlation_test <- cor.test(SB$attendance, SB$avg_ticket_price)

print(correlation_test)

ggplot(SB, aes(x = avg_ticket_price, y = attendance)) +
  geom_point(color = "blue", alpha = 0.6) +    
  geom_smooth(method = "lm", color = "red") +  
  labs(
    title = "Correlation Between Ticket Prices and Attendance",
    x = "Average Ticket Price",
    y = "Attendance"
  ) +
  theme_minimal()

#12

SB <- SB %>%
  mutate(avg_ticket_price = parse_number(`avg_ticket_price`))
summary(SB$avg_ticket_price)
SB <- SB %>%
  mutate(avg._ticket_price = parse_number(avg._ticket_price))

ggplot(SB, aes(x = Year, y = avg._ticket_price)) +
  geom_line(color = "blue", size = 1) +  # Line showing the trend
  geom_point(color = "red", size = 2) +  # Points for individual years
  labs(
    title = "Trend of Ticket Prices in the Super Bowl Over Time",
    x = "Year",
    y = "Average Ticket Price (USD)"
  ) +
  theme_minimal()

#inflation
SB <- SB %>%
  mutate(inflation_adj = parse_number(`inflation_adj`))
ggplot(SB, aes(x = Year, y = inflation_adj)) +
  geom_line(color = "blue", size = 1) +  # Line showing the trend
  geom_point(color = "red", size = 2) +  # Points for individual years
  labs(
    title = "Trend of Ticket Prices in the Super Bowl Over Time",
    x = "Year",
    y = "Inflated Ticket Price (USD)"
  ) +
  theme_minimal()


#9
division_wins <- SB %>%
  group_by(Division) %>%
  summarise(total_wins = n(), .groups = "drop")

ggplot(division_wins, aes(x = Division, y = "", fill = total_wins)) +
  geom_tile(color = "white") +  # Heat map tiles
  geom_text(aes(label = total_wins), color = "black", size = 5) +  
  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  
  labs(
    title = "Total Super Bowl Wins by Division",
    x = "Division",
    y = NULL,
    fill = "Total Wins"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )


#13

# Logistic regression to analyze the relationship
logistic_model <- glm(Name ~ winning_pts, data = SB, family = binomial)

# Summary of the model
summary(logistic_model)

# Visualize the logistic regression
ggplot(SB, aes(x = Name, y = winning_pts)) +
  geom_jitter(width = 0.2, height = 0.02, alpha = 0.6, color = "blue") +  # Add jittered points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +  # Logistic curve
  labs(
    title = "Relationship Between Points Scored and Winning Status",
    x = "Points Scored",
    y = "Winning Probability"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#13
view_ad_cost_cor <- SB %>%
  select(avg_us_viewers, ad_cost) %>%
  cor(use ="complete.obs")

print(view_ad_cost_cor)


ggplot(SB, aes(x = avg_us_viewers, y = ad_cost)) +
  geom_point(color = "blue", alpha = 0.6) +
  # Scatter plot points
  geom_smooth(method = "lm", color = "red") +
  # Add linear regression line
  labs(
    title = "Correlation Between Average Viewership and Ad Cost",
    x = "Average US Viewers (in thousdands)",
    y = "Ad Cost (in thousands $)" 
  ) +
  scale_x_continuous(labels = label_number(scale = 0.001)) + 
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  theme_minimal()

correlation_test <- cor.test(SB$avg_us_viewers, SB$ad_cost)
print(correlation_test)

