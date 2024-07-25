# r-project

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Your dataset
data <- data.frame(
  user = c(235136, 333588, 254414, 234192, 51549, 56480, 144649, 249366, 372004, 338013, 43555, 317454, 205375, 307608, 359855, 284938, 235143, 141402),
  first_open = c("14:51.3", "16:00.9", "19:09.2", "08:46.4", "50:48.7", "58:15.8", "33:18.5", "07:49.9", "22:01.6", "22:16.0", "48:27.6", "07:07.4", "28:45.9", "52:31.8", "48:48.9", "41:35.7", "07:35.1", "12:46.9"),
  dayofweek = c(3, 6, 1, 4, 1, 2, 1, 1, 2, 4, 1, 1, 0, 5, 0, 5, 6, 5),
  hour = c("02:00:00", "01:00:00", "19:00:00", "16:00:00", "18:00:00", "09:00:00", "02:00:00", "03:00:00", "14:00:00", "18:00:00", "04:00:00", "11:00:00", "06:00:00", "19:00:00", "04:00:00", "18:00:00", "16:00:00", "21:00:00"),
  age = c(23, 24, 23, 28, 31, 20, 35, 26, 29, 26, 39, 32, 25, 23, 17, 25, 21, 55),
  screen_list = c("idscreen,joinscreen,Cycle,product_review,ScanPreview,VerifyDateOfBirth,VerifyPhone,VerifyToken,ProfileVerifySSN,Loan2,Settings,ForgotPassword,Login", "joinscreen,product_review,product_review2,ScanPreview,VerifyDateOfBirth,location,VerifyCountry,VerifyPhone,VerifyToken,Institutions,Loan2", "Splash,Cycle,Loan", "product_review,Home,product_review,Loan3,Finances,Credit3,ReferralContainer,Leaderboard,Rewards,RewardDetail,ScanPreview,location,VerifyDateOfBirth,VerifyPhone,VerifySSN,Credit1,Credit2", "idscreen,joinscreen,Cycle,Credit3Container,ScanPreview,VerifyPhone,VerifySSN,Credit1,Loan2,Home,Institutions,SelectInstitution,BankVerification,ReferralContainer,product_review,product_review2,VerifyCountry,VerifyToken,product_review", "idscreen,Cycle,Home,ScanPreview,VerifyPhone,VerifySSN,Credit1,Credit3Dashboard,Loan2,Institutions,product_review,product_review,product_review3", "product_review,product_review2,ScanPreview", "Splash,Cycle,Home,Credit3Container,Credit3Dashboard,Loan2,product_review,product_review2,ScanPreview,VerifyCountry,VerifyPhone,VerifyToken,product_review,product_review,SelectInstitution,BankVerification,TransactionList", "product_review,product_review2,ScanPreview,VerifyCountry,VerifyPhone,VerifyToken,VerifySSN,product_review,SelectInstitution,BankVerification,Institutions,RewardsContainer,ReferralContainer,Home,Loan2,location,product_review,product_review,product_review,product_review,Loan4,product_review,product_review,product_review,product_review,product_review,product_review", "Home,Loan2,product_review,product_review,product_review3,ScanPreview,VerifyDateOfBirth,location,VerifyCountry,VerifyPhone,VerifyToken,product_review,product_review,VerifySSN,product_review,SelectInstitution,BankVerification", "Splash,idscreen,Home,RewardsContainer,Settings,product_review,Leaderboard,Loan2,product_review", "product_review,Home,Loan2,Credit3Container,VerifyPhone,Credit3Dashboard,Institutions,SelectInstitution,product_review,ScanPreview,VerifyCountry,VerifyToken,product_review,product_review,BankVerification,CC1,CC1Category", "idscreen,joinscreen,Cycle,product_review,product_review2,ScanPreview,VerifyCountry,VerifyToken,VerifyPhone", "Alerts,ProfilePage,Home,Credit3Container", "joinscreen,product_review,product_review2,ScanPreview,Home", "idscreen,joinscreen,Cycle,Loan2,product_review,product_review2,VerifyDateOfBirth,location,VerifyCountry,VerifyPhone,VerifyToken,VerifySSN,product_review,SelectInstitution,BankVerification", "product_review,product_review,product_review,product_review,VerifyDateOfBirth,location", "joinscreen,Cycle,product_review,Loan2,product_review2,VerifyDateOfBirth,location,VerifyCountry,VerifyPhone,VerifyToken,VerifySSN,product_review,SelectInstitution,BankVerification"),
  numscreens = c(15, 13, 3, 40, 32, 14, 3, 41, 33, 19, 14, 25, 11, 4, 9, 26, 6, 20),
  minigame = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0),
  used_premium_feature = c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  enrolled = c(0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1),
  liked = c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

# Convert hour column to POSIXct format
data$hour <- as.POSIXct(data$hour, format = "%H:%M:%S")

# Age Distribution
age_dist <- ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Enrollment Status
enrollment_status <- ggplot(data, aes(x = factor(enrolled), fill = factor(enrolled))) +
  geom_bar() +
  labs(title = "Enrollment Status", x = "Enrolled", y = "Count") +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Enrolled", "Enrolled"))

# Liked vs. Not Liked
liked_status <- ggplot(data, aes(x = factor(liked), fill = factor(liked))) +
  geom_bar() +
  labs(title = "Liked vs. Not Liked", x = "Liked", y = "Count") +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Liked", "Liked"))

# Day of the Week Distribution
dayofweek_dist <- ggplot(data, aes(x = factor(dayofweek))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Day of the Week Distribution", x = "Day of Week", y = "Frequency") +
  scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# Hourly Distribution
hourly_dist <- ggplot(data, aes(x = hour)) +
  geom_histogram(binwidth = 3600, fill = "skyblue", color = "black") +
  labs(title = "Hourly Distribution", x = "Hour", y = "Frequency") +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M")

# Pie chart: Premium feature usage
premium_pie <- ggplot(data, aes(x = factor(used_premium_feature), fill = factor(used_premium_feature))) +
  geom_bar(width = 1, fill = c("red", "green")) +
  coord_polar(theta = "y") +
  labs(title = "Premium Feature Usage", x = "", y = "") +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Used", "Used"))

# Line Chart: Age vs. Number of Screens
line_chart <- ggplot(data, aes(x = age, y = numscreens)) +
  geom_line() +
  labs(title = "Age vs. Number of Screens", x = "Age", y = "Number of Screens")

# Bar Chart: Age vs. Number of Screens
bar_chart <- ggplot(data, aes(x = age, y = numscreens)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Age vs. Number of Screens", x = "Age", y = "Number of Screens")

# Scatter Plot: Age vs. Number of Screens
scatter_plot <- ggplot(data, aes(x = age, y = numscreens)) +
  geom_point() +
  labs(title = "Age vs. Number of Screens", x = "Age", y = "Number of Screens")

# Arrange plots
grid.arrange(age_dist, enrollment_status, liked_status, dayofweek_dist, hourly_dist, premium_pie, line_chart, bar_chart, scatter_plot, ncol = 3)
