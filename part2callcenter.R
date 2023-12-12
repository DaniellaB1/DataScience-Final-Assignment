#PARTII
library(dplyr)
library(ggplot2)

# Read the dataset
file2_path <- "~/Desktop/Call_Center.csv"
call_center_data<- read.csv(file2_path)


#Customer Satisfaction Analysis

# Descriptive Statistics, i know this is what most people do first but it got me nothing
summary(call_center_data$Csat_Score)



# Visualize Distribution of CSAT Scores

# Sentiment vs. CSAT
sentiment_vs_csat <- call_center_data %>%
  group_by(Sentiment) %>%
  summarize(avg_csat = mean(as.numeric(as.character(Csat.Score), na.rm = TRUE)))

# Bar chart there are no bars on mine, when i checked out the data i saw there were
#lots of nulls so that makes sense by i didd sentiment as a predictor of csat and labeled it
ggplot(sentiment_vs_csat, aes(x = Sentiment, y = avg_csat)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average CSAT Scores by Sentiment", x = "Sentiment", y = "Average CSAT Score")



#  Response Time and Call Duration Analysis

# Response Time Analysis
summary(call_center_data$Response_Time)

# Response Time vs. CSAT,i looked up this code, and it makes sense
#the code works except that the graph does not show anything and the curve line specified
#blue shows nothing and the i dont see any dots 
ggplot(call_center_data, aes(x = Response.Time, y = Csat.Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Response Time vs. CSAT", x = "Response Time", y = "CSAT Score")

# Call Duration vs. CSAT
# i still confused if this is some sort of dot plot but basically i see the smooth
#curve line of the scatter plot somewhere around 6 minutes
# very cool
ggplot(call_center_data, aes(x = Call.Duration.In.Minutes, y = Csat.Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Call Duration vs. CSAT", x = "Call Duration (Minutes)", y = "CSAT Score")




# Not sure if this all does much analysis, had plenty of errors so now I am 
#going to go into tableau and do the rest