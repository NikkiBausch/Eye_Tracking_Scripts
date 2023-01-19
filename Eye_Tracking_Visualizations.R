df<- questionnaire
QQs= data.frame(df$V1, df$V5, df$V6, df$V7, df$V8, df$V9, df$V10, df$V11, df$V12, df$V13, df$V14, df$V15, df$V16, df$V17, df$V18, df$V19, df$V20)
library(viridis)
library(lme4)
library(jtools)
library(rsq)
library(car)
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


# Distribution of Data for Saccadic Events 
df2 <- Consolidated_Saccadic_Data
df2 <- na.omit(df2$AVERAGE_BLINK_DURATION)
df2$AVERAGE_BLINK_DURATION <- gsub(",", ".", df2$AVERAGE_BLINK_DURATION)
df2$AVERAGE_FIXATION_DURATION <- gsub(",", ".", df2$AVERAGE_FIXATION_DURATION)
df2$DURATION <- as.numeric(df2$DURATION)
df2$BLINK_COUNT <- as.numeric(df2$BLINK_COUNT)
df2$AVERAGE_BLINK_DURATION <- as.numeric(df2$AVERAGE_BLINK_DURATION)
#Average_Blink_Duration contains NAs. 
df2$FIXATION_COUNT <- as.numeric(df2$FIXATION_COUNT)
df2$MEDIAN_FIXATION_DURATION <- as.numeric(df2$MEDIAN_FIXATION_DURATION)
df2$AVERAGE_FIXATION_DURATION <- as.numeric(df2$AVERAGE_FIXATION_DURATION)

# Subsetting for each Book cover 
B01= subset(df2, filename_book == "b01.png")
B02 = subset(df2, filename_book == "b02.png")
B03 = subset(df2, filename_book == "b03.png")
B04 = subset(df2, filename_book == "b04.png")

# Example Boxplot to see if this works: 
boxplot(B01$BLINK_COUNT, xlab = "blink count", horizontal = TRUE, col = col_selection2)

#Boxplots for all 4 separate book covers and columns 

BX_Blink <- data.frame(B01$BLINK_COUNT, B02$BLINK_COUNT, B03$BLINK_COUNT, B04$BLINK_COUNT)

boxplot(BX_Blink, xlab = "Blink Count", main = "Overall Blink Counts for Each Cover", horizontal = TRUE, col = col_selection3)

BX_Fixation_Count <- data.frame(B01$FIXATION_COUNT, B02$FIXATION_COUNT, B03$FIXATION_COUNT, B04$FIXATION_COUNT)

boxplot(BX_Fixation_Count, xlab = "Fixation Count", main = "Overall Fixation Counts for each Cover", horizontal = TRUE, col = col_selection3)

BX_Median_Fixation <- data.frame(B01$MEDIAN_FIXATION_DURATION, B02$MEDIAN_FIXATION_DURATION, B03$MEDIAN_FIXATION_DURATION, B04$MEDIAN_FIXATION_DURATION)
boxplot(BX_Median_Fixation, xlab = "Median Fixation Duration (in seconds)", main = "Overall Median Fixation Duration for each Book Cover", horizontal = TRUE, col = col_selection3)

hist(df2$BLINK_COUNT, breaks = 4, xlab = "Blink Count (all)", main = "Visualization for Blink Count", col = "purple")
hist(df2$FIXATION_COUNT, xlab = "Fixation Count (all)", main = "Visualization for Fixation Count", col = "purple")
hist(df2$MEDIAN_FIXATION_DURATION, breaks = 4, xlab = "Median Fixation Duration (all)", main = "Visualization for Median Fixation duration", col = "purple")

View(B01)
View(B02)
View(B03)
View(B04)

DescTools::Range(B01$BLINK_COUNT) # 0,23
DescTools::Range(B02$BLINK_COUNT) # 0, 17
DescTools::Range(B03$BLINK_COUNT) # 1, 16
DescTools::Range(B04$BLINK_COUNT) # 0, 19
DescTools::Range(B01$FIXATION_COUNT) # 40, 73
DescTools::Range(B02$FIXATION_COUNT) # 32, 70 
DescTools::Range(B03$FIXATION_COUNT) # 45, 74
DescTools::Range(B04$FIXATION_COUNT) # 33, 75

DescTools::Median(B01$BLINK_COUNT) # 3
DescTools::Median(B02$BLINK_COUNT) # 4
DescTools::Median(B03$BLINK_COUNT) # 3
DescTools::Median(B04$BLINK_COUNT) # 4

DescTools::Median(B01$FIXATION_COUNT) # 63
DescTools::Median(B02$FIXATION_COUNT) # 61
DescTools::Median(B03$FIXATION_COUNT) # 67
DescTools::Median(B04$FIXATION_COUNT) # 58

# Individual histograms for Blink Count
hist(B01$BLINK_COUNT, breaks = 4, xlab = "Blink Count (B01)", main = "Visualization for Blink Count", col = "darkmagenta")
hist(B02$BLINK_COUNT, breaks = 4, xlab = "Blink Count (B02)", main = "Visualization for Blink Count", col = "darkmagenta")
hist(B03$BLINK_COUNT, breaks = 4, xlab = "Blink Count (B03)", main = "Visualization for Blink Count", col = "darkmagenta")
hist(B04$BLINK_COUNT, breaks = 4, xlab = "Blink Count (B04)", main = "Visualization for Blink Count", col = "darkmagenta")

#fixation count, individual too.

hist(B01$FIXATION_COUNT, breaks = 4, xlab = "Fixation Count (B01)", main = "Visualization for Fixation Count", col = "darkgreen") # Normally distributed?
hist(B02$FIXATION_COUNT, breaks = 4, xlab = "Fixation Count (B02)", main = "Visualization for Fixation Count", col = "darkgreen")
hist(B03$FIXATION_COUNT, breaks = 4, xlab = "Fixation Count (B03)", main = "Visualization for Fixation Count", col = "darkgreen")
hist(B04$FIXATION_COUNT, breaks = 4, xlab = "Fixation Count (B04)", main = "Visualization for Fixation Count", col = "darkgreen") # Normally distributed?

mdl = lm(B01$FIXATION_COUNT~B01$BLINK_COUNT, data=QQs)
summary(mdl)




qqnorm(B01$FIXATION_COUNT, main = "Q-Q Plot for Normality Fixation Count (B01)", col = "darkgreen")
qqline(B01$FIXATION_COUNT, col = "darkred", lwd = 1, lty = 1) #median = 63 

DescTools::Mean(B01$FIXATION_COUNT) #Mean 61.46 (rounded to second place)

qqnorm(B04$FIXATION_COUNT, main = "Q-Q Plot for Normality Fixation Count (B04)", col = "hotpink")
qqline(B04$FIXATION_COUNT, col = "darkblue", lwd = 1, lty = 1) # median = 58

DescTools::Mean(B04$FIXATION_COUNT) #mean = 58.53 #SOOOOOO CLOOOOOSE!

# Compare IA_Fixation_Count values and Fixation_count values in scatterplots. 

df2$IA_FIXATION_COUNT <- as.numeric(df2$IA_FIXATION_COUNT)
B01$IA_FIXATION_COUNT <- as.numeric(B01$IA_FIXATION_COUNT)

View(B01)

hist(B01$IA_FIXATION_COUNT, breaks = 4, xlab = "AOI Fixation Count", main = "Visualization for AOI Fixation Count (B01)", col = "green")
hist(B02$IA_FIXATION_COUNT, breaks = 4, xlab = "AOI Fixation Count", main = "Visualization for AOI Fixation Count (B02)", col = "green")
hist(B03$IA_FIXATION_COUNT, breaks = 4, xlab = "AOI Fixation Count", main = "Visualization for AOI Fixation Count (B03)", col = "green")
hist(B04$IA_FIXATION_COUNT, breaks = 4, xlab = "AOI Fixation Count", main = "Visualization for AOI Fixation Count (B04)", col = "green")

qqnorm(B03$IA_FIXATION_COUNT, main = "Q-Q Plot for Normality AOI Fixation Count (B03)", col = "darkred")
qqline(B03$IA_FIXATION_COUNT, col = "darkblue", lwd = 1, lty = 1) #median = 63 

DescTools::Median(B03$IA_FIXATION_COUNT) # 34
DescTools::Mean(B03$IA_FIXATION_COUNT) # 35.38

plot(B01$IA_FIXATION_COUNT, col = "darkmagenta", main = "AOI Fixation Counts (B01)", xlab = "", ylab = "")
plot(B01$FIXATION_COUNT, col = "darkmagenta", main = "Fixation Counts (B01)", xlab = "", ylab = "")

plot(B02$IA_FIXATION_COUNT)
plot(B02$FIXATION_COUNT)

plot(B03$IA_FIXATION_COUNT)
plot(B03$FIXATION_COUNT)

plot(B04$IA_FIXATION_COUNT)
plot(B04$FIXATION_COUNT)

