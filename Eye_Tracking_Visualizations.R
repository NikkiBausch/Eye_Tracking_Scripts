df<- questionnaire
QQs= data.frame(df$V1, df$V5, df$V6, df$V7, df$V8, df$V9, df$V10, df$V11, df$V12, df$V13, df$V14, df$V15, df$V16, df$V17, df$V18, df$V19, df$V20)
library(viridis)
col_selection <- viridis(5, option = "G")


# Questions (excluding the first three about familiarity with book)
# Scale: 1 (very bad) to 5 (very good)
# Book 1: (black and white, profile)
b1_color <- as.numeric(df$V5)
b1_text <- as.numeric(df$V6)
b1_overall <- as.numeric(df$V7)
# Colors
hist(b1_color, breaks = 6, xlab = "Color Rankings as Numbers", ylab = "Number of Preferences Marked", main = "Color Preference Results for B&W Portrait", col = col_selection)
# Text
hist(b1_text, breaks = 6, xlab = "Font Rankings as Numbers", ylab = "Number of Preferences Marked", main = "Font Preference Results for B&W Portrait", col = col_selection)
# Overall Appearance
hist(b1_overall, breaks = 3, xlab = "Overall Appearance (on a scale of 1-5)", ylab = "Number of Preferences Marked", xlim = c(1,5), main= "Overall Appearance Ratings for B&W Portrait", col= col_selection)

# Book 2: (Close up face)
col_selection2 = viridis(6, option = "C")
b2_color <- as.numeric(df$V8)
b2_text <- as.numeric(df$V9)
b2_overall <- as.numeric(df$V10)
# Colors
hist(b2_color, breaks = 5, xlab ="Color Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Color Preference Results for Facial close-up", col = col_selection2)
# Text
hist(b2_text, breaks = 5, xlab ="Font Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Font Preference Results for Facial close-up", col = col_selection2)
# Overall Appearance
hist(b2_overall, breaks = 5, xlab = "Overall Appearance (on a scale of 1-5)",ylab= "Number of Preferences Marked", xlim = c(1,5), main = "Overall Appearance Ratings for Facial close-up", col = col_selection2)

# Book 3: (Portrait, zoomed out view)
col_selection3 = viridis(6, option = "H")
b3_color <- as.numeric(df$V11)
b3_text <- as.numeric(df$V12)
b3_overall <- as.numeric(df$V13)
# Colors 
hist(b3_color, breaks = 6, xlab = "Color Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Color Preference Results for Portrait", col = col_selection3)
# Text
hist(b3_text, breaks = 6, xlab = "Font Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Font Preference Results for Portrait", col = col_selection3)
# Overall Appearance
hist(b3_overall, breaks = 6, xlab = "Overall Appearance (on a scale of 1-5)", ylab = "Number of Preferences Marked", xlim= c(1,5), main = "Overall Appearance Ratings for Portrait", col = col_selection3)

# Book 4: (floral, plain)
col_selection4 = viridis(6, option = "B")
b4_color <- as.numeric(df$V14)
b4_text <- as.numeric(df$V15)
b4_overall <- as.numeric(df$V16)
# Colors 
hist(b4_color, breaks = 6, xlab = "Color Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Color Preference Results for Floral Pattern", col = col_selection4)
# Text 
hist(b4_text, breaks = 6, xlab = "Font Rankings as Numbers", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Font Preference Results for Floral Pattern", col = col_selection4)
# Overall Appearance
hist(b4_overall, breaks = 6, xlab = "Overall Appearance (on a scale of 1-5)", ylab = "Number of Preferences Marked", xlim = c(1,5), main = "Overall Appearance Ratings for Floral Pattern", col = col_selection4)

# Rank them from most to least preferred
b1_Rank <- as.numeric(df$V17)
b2_Rank <- as.numeric(df$V18)
b3_Rank <- as.numeric(df$V19)
b4_Rank <- as.numeric(df$V20)

# B&W Profile 
col_selection5 = viridis(6, option = "C")
hist(b1_Rank, breaks = 6, xlab = "Ranked Preference (in a scale of 1-4)", ylab = "Number of Preferences Marked", xlim = c(0,4), main= "Overall Ranking Chosen (1 being the highest and 4 being the lowest)", col = col_selection5)

# Close up Face Portrait
hist(b2_Rank, breaks = 6, xlab = "Ranked Preference (in a scale of 1-4)", ylab = "Number of Preferences Marked", xlim = c(0,4), main= "Overall Ranking Chosen (1 being the highest and 4 being the lowest)", col = col_selection5)

# Portrait
hist(b3_Rank, breaks = 6, xlab = "Ranked Preference (in a scale of 1-4)", ylab = "Number of Preferences Marked", xlim = c(0,4), main= "Overall Ranking Chosen (1 being the highest and 4 being the lowest)", col = col_selection5)

# Floral Pattern
hist(b4_Rank, breaks = 6, xlab = "Ranked Preference (in a scale of 1-4)", ylab = "Number of Preferences Marked", xlim = c(0,4), main= "Overall Ranking Chosen (1 being the highest and 4 being the lowest)", col = col_selection5)

