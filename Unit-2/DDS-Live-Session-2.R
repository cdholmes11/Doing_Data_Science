
# Packages
  
library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)
library(tidyr)


# Data Import from csv

bball_players = read.csv("C:/Users/corey/OneDrive/Documents/School/Classes/Doing Data Science/MSDS_6306_Doing-Data-Science/Unit 2/PlayersBBall.csv",header = TRUE) # headers are included in data



# Visually represent the number of players in each position.

bball_players %>% ggplot(aes(x = position)) + geom_bar(fill = c("#3399CC")) + 
  xlab("Position") + ylab("Count") +
  ggtitle("Player Count by Position") + theme_minimal()


# Use the dataset to visually investigate the distribution of the weight of 
# centers (C) is greater than the distrbution of the weight of forwards (F).

w = bball_players %>% filter(position == "C" | position == "F") %>% 
  ggplot(aes(fill = position)) + geom_histogram(aes(x = weight)) + 
  xlab("Weight(lbs)") + ylab("Count") + xlim(100,400) +
  ggtitle("Center & Forward Weight Distribution") + facet_wrap(~position) +
  theme_minimal()
ggplotly(w)


# Use the dataset to visually investigate if the distribution of the height of 
# centers (C) is greater than the distrbution of the height of forwards (F)

## Transform Height to numeric
bball_new = bball_players %>% 
  separate(height, c("feet", "inch"), sep = "-", fill = "right") %>% 
  mutate(inches = 12 * as.numeric(feet) + as.numeric(inch))

## Set x scale based on min and max height values
min_height = min(bball_new$inches, na.rm = TRUE)
max_height = max(bball_new$inches, na.rm = TRUE)

## Plot histogram of Height by Position
h = bball_new %>% filter(position == "C" | position == "F") %>% 
  ggplot(aes(x = inches, fill = position)) + geom_histogram() + 
  facet_wrap(~position) + xlab("Height(in)") + ylab("Count") + 
  xlim(min_height,max_height) +
  ggtitle("Center & Forward Height Distribution") + theme_minimal()
ggplotly(h)



# Use the dataset to visually investigate if the distribution of height is 
# different between any of the positions.

## Plot histogram of Height by Position
h = bball_new %>%  
  ggplot(aes(x = inches, fill = position)) + geom_histogram() + 
  facet_wrap(~position) + xlab("Height(in)") + ylab("Count") + 
  xlim(min_height,max_height) +
  ggtitle("Center & Forward Height Distribution") + theme_minimal()
ggplotly(h)



# Use the dataset to investigate how the player's height is related to the 
# player's weight. How does height change as the weight changes?

bball_new %>% ggplot(aes(x = inches, y = weight)) + 
  geom_point(color = "blue", position = "jitter") + xlab("Height(in)") + 
  ylab("Weight(lbs)") + ggtitle("Height v. Weight") + theme_minimal()


# Is there any difference in the relationship between height and weight between
# positions? Are height and weight related differently for different positions?

## Single scatter plot view
bball_new %>% ggplot(aes(x = inches, y = weight)) + 
  geom_point(aes(color = position), position = "jitter") + xlab("Height(in)") + 
  ylab("Weight(lbs)") + ggtitle("Height v. Weight by Position") + 
  theme_minimal()

## Facet scatter plot view
bball_new %>% ggplot(aes(x = inches, y = weight)) + 
  geom_point(aes(color = position), position = "jitter") + 
  facet_wrap(~position) + xlab("Height(in)") + ylab("Weight(lbs)") + 
  ggtitle("Height v. Weight by Position (Facet)") + theme_minimal()



# A historian would like to investigate the claim that the heights of players 
# have increased over the years. Analyze this claim graphically / visually.

## I chose to use 'year_start' to compare heights over 'year_end' because 
## waiting till the end of a players career to consider their heights will skew 
## the data due to different career lengths.

## Visualization
m_height = bball_new %>% ggplot(aes(x = year_start, y =inches)) + 
  geom_bar(stat = "summary", fill = "red") + xlab("Star Year") + 
  ylab("Height(in)") + ggtitle("League Mean Height Over Time") + theme_minimal()
ggplotly(m_height)


# Create a 3D plot of height vs. weight vs. year and color code the points by 
# position.

d = plot_ly(bball_new, x = ~year_start, y = ~weight, z = ~inches, color = ~position) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Start Year'),
                      yaxis = list(title = 'Weight'),
                      zaxis = list(title = 'Height')))
d


# Use one of the 50 best plots to visualize some aspects of the data and 
# provide at least one insight. 

## Find last
start_max = max(bball_new$year_start,na.rm = TRUE)
start_range = 40

bp = bball_new %>% 
  filter(year_start>=(start_max-start_range) & year_start<=start_max) %>% 
  ggplot(aes(x = as.character(year_start), y = inches)) + xlab("Start Year") +
  ylab("Height(in)") + ggtitle("Player Height Summary Past 40 years") + 
  geom_boxplot(fill = c("#3399CC")) + theme_minimal()
ggplotly(bp)


# Education Income

# Visually test the claim that the distribution of incomes increase 
#(mean or median) as the educatin level rises.

## Set Education Level Order
edu_income$Educ = factor(edu_income$Educ, levels = c("<12","12","13-15","16",">16"))

## Bar Chart
edu_income %>% ggplot(aes(x = Educ, y = Income2005)) + 
  geom_bar(stat = "summary") + xlab("Education Level") + ylab("Mean Income") +
  ggtitle("Mean Income by Education Level") + theme_minimal()


