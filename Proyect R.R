# First, we imported the data from Kaggle

library(readxl)
iran = read_excel("search_term_iran.xlsx")
jp = read_excel("search_term_japan.xlsx")
us = read_excel("search_term_us.xlsx")
canada = read_excel("search_term_canada.xlsx")
uk = read_excel("search_term_uk.xlsx")
italia = read_excel("search_term_italy.xlsx")
sk = read_excel("search_term_sk.xlsx")
# Second we analyzed with a graph for all countries the total mental health issues across time


# US t

Total_mental_health_issues = rowSums(us[,c(2:10)])
us$Week = as.Date(us$Week)
us_at = data.frame(week = us$Week, Total_mental_health_issues)

library(ggplot2)
ggplot (us_at,aes(x =`week`, y = `Total_mental_health_issues`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("US Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# Japan t

Total_mental_health_issues_jp = rowSums(jp[,c(2:10)])
jp$Week = as.Date(jp$Week)
japan_at = data.frame(week = jp$Week, Total_mental_health_issues_jp)

library(ggplot2)
ggplot (japan_at,aes(x = `week`, y = `Total_mental_health_issues_jp`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Japan Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# uk t

Total_mental_health_issues_uk = rowSums(uk[,c(2:10)])
uk$Week = as.Date(uk$Week)
uk_at = data.frame(week = uk$Week, Total_mental_health_issues_uk)

library(ggplot2)
ggplot (uk_at,aes(x = `week`, y = `Total_mental_health_issues_uk`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("United Kingdom Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# italy t

Total_mental_health_issues_itl = rowSums(italia[,c(2:10)])
italia$Week = as.Date(italia$Week)
itl_at = data.frame(week = italia$Week, Total_mental_health_issues_itl)

library(ggplot2)
ggplot (itl_at,aes(x = `week`, y = `Total_mental_health_issues_itl`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Italy Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# canada t

Total_mental_health_issues_canada = rowSums(canada[,c(2:10)])
canada$Week = as.Date(canada$Week)
canada_at = data.frame(week = canada$Week, Total_mental_health_issues_canada)

library(ggplot2)
ggplot (canada_at,aes(x = `week`, y = `Total_mental_health_issues_canada`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Canada Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# South Korea t

Total_mental_health_issues_sk = rowSums(sk[,c(2:10)])
sk$Week = as.Date(sk$Week)
sk_at = data.frame(week = sk$Week, Total_mental_health_issues_sk)

library(ggplot2)
ggplot (sk_at,aes(x = `week`, y = `Total_mental_health_issues_sk`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("South Korea Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# iran t

Total_mental_health_issues_iran = rowSums(iran[,c(2:10)])
iran$Week = as.Date(iran$Week)
iran_at = data.frame(week = iran$Week, Total_mental_health_issues_iran)

library(ggplot2)
ggplot (iran_at,aes(x = `week`, y = `Total_mental_health_issues_iran`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Iran Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week")

# Once analyzed, we compared the real date in which each country recorded its begging of the pandemic, with our graphs and determine the date we were going to use as a separator of pre and post pandemic

# Third, we graph the total mental health issues, pre and post pandemic, by country 

# Pre pandemic

pre_pandemic = read_excel("Pre-pandemic by country.xlsx")

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)

pre_pandemic_plot = barplot(pre_pandemic$`Mental Health Issues`, names.arg = pre_pandemic$Country,
                            xlab = "Countries", ylab = "Total Mental Health Issues", col = c("red", "orange", "yellow", "green","dark green","light blue", "blue"), 
                            main = "Pre-pandemic Analysis", ylim = c(0,20000), font.axis = 2)
text(pre_pandemic_plot, pre_pandemic$`Mental Health Issues`, adj = c(0.5,-0.3), labels = pre_pandemic$`Mental Health Issues`, col = "Black")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")

pre_pandemic_plot = barplot(pre_pandemic$`Mental Health Issues`, names.arg = pre_pandemic$Country,
                            xlab = "Countries", ylab = "Total Mental Health Issues", col = c("red", "orange", "yellow", "green","dark green","light blue", "blue"), 
                            main = "Pre-pandemic Analysis", ylim = c(0,20000), font.axis = 2, add = TRUE)

# Post Pandemic

post_pandemic = read_excel("Post-pandemic by country.xlsx")

post_pandemic_plot = barplot(post_pandemic$`Mental Health Issues`, names.arg = post_pandemic$Country,
                             xlab = "Countries", ylab = "Total Mental Health Issues", col = c("red", "orange", "yellow", "green","dark green","light blue", "blue"), 
                             main = "Post-pandemic Analysis", ylim = c(0,20000), font.axis = 2)
text(post_pandemic_plot, post_pandemic$`Mental Health Issues`, adj = c(0.5,-0.3), labels = post_pandemic$`Mental Health Issues`, col = "Black")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")

post_pandemic_plot = barplot(post_pandemic$`Mental Health Issues`, names.arg = post_pandemic$Country,
                             xlab = "Countries", ylab = "Total Mental Health Issues", col = c("red", "orange", "yellow", "green","dark green","light blue", "blue"), 
                             main = "Post-pandemic Analysis", ylim = c(0,20000), font.axis = 2, add = TRUE)

# Fourth, once analyzed both graphs, we selected three countries based on the range of the data between pre and post pandemic to try to obtain a deeper analysis. These were Canada, Italy and Iran

# Five, we created for each country we selected, a matrix correlation in order to see in each country, what mental health issues were the most relevant and analyzed them 

# Correlation Canada

canada.2 = canada
canada.2$Week = NULL
canada_cor = cor(canada.2)

library(corrplot)
corrplot(canada_cor, type = "upper", order = "hclust", # We selected axiety, depression and OCD
         tl.col = "black", tl.srt = 45)

# Data frame Canada


Mental_health_issues_cnd = c(rep("depression" , 2) , rep("anxiety" , 2) , rep("OCD" , 2))
period = rep(c("Pre_pandemic" , "Post_pandemic"), 1)
value_1 = c(78.92, 85.59, 84.50, 86.59, 55.84, 54.54)

table_canada_mental_health =  data.frame(Mental_health_issues_cnd ,period, value_1)

ggplot(table_canada_mental_health, aes(fill=period, y=value_1, x=Mental_health_issues_cnd)) + 
  geom_bar(position="dodge", stat = "identity") + 
  geom_text(aes(label = round(value_1, 1)), 
            position = position_dodge(0.9),
            color="black",vjust = 1.5,hjust = 0.5) + ggtitle("Pre and Post Pandemic Mental Health Issues Change in Canada") +
  scale_fill_manual(values=c("blue", "light blue"))

# Correlation Iran

iran.2 = iran
iran.2$Week = NULL
iran_cor = cor(iran.2)

library(corrplot)
corrplot(iran_cor, type = "upper", order = "hclust", # We selected axiety, depression and OCD
         tl.col = "black", tl.srt = 45)

# Data frame Iran

Mental_health_issues_iran = c(rep("depression" , 2) , rep("anxiety" , 2) , rep("OCD" , 2))
period_iran = rep(c("Pre_pandemic" , "Post_pandemic"), 1)
value_iran = c(83.88, 66.83, 70.74, 78.79, 20.85, 19.25)

table_iran_mental_health =  data.frame(Mental_health_issues_iran ,period_iran, value_iran)

ggplot(table_iran_mental_health, aes(fill=period_iran, y=value_iran, x=Mental_health_issues_iran)) + 
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label = round(value_iran, 1)), 
            position = position_dodge(0.9),
            color="black",vjust = 1.5,hjust = 0.5) + ggtitle("Pre and Post Pandemic Mental Health Issues Change in Iran") +
  scale_fill_manual(values=c("blue", "light blue"))

# Correlation Italy

italy.2 = italia
italy.2$Week = NULL
italy_cor = cor(italy.2)

corrplot(italy_cor, type = "upper", order = "hclust", # We selected axiety, depression and OCD
         tl.col = "black", tl.srt = 45)

# Data fame Italy

Mental_health_issues_italy = c(rep("depression" , 2) , rep("anxiety" , 2) , rep("OCD" , 2))
period_italy = rep(c("Pre_pandemic" , "Post_pandemic"), 1)
value_italy = c(87.92, 74.60, 75.86, 87.93, 34.19, 33.80)

table_italy_mental_health =  data.frame(Mental_health_issues_italy ,period_italy, value_italy)

ggplot(table_italy_mental_health, aes(fill=period_italy, y=value_italy, x=Mental_health_issues_italy)) + 
  geom_bar(position="dodge", stat = "identity") + 
  geom_text(aes(label = round(value_italy, 1)), 
            position = position_dodge(0.9),
            color="black",vjust = 1.5,hjust = 0.5) + ggtitle("Pre and Post Pandemic Mental Health Issues Change in Italy") +
  scale_fill_manual(values=c("blue", "light blue"))

# Statistic Analysis of Iran Pre Pandemic


pre_pandemic_iran = rowSums(iran[,c(2:4)][c(1:27),])
week_prepandemic_iran = iran$Week [c(1:27)]
iran_statistics_prepandemic = data.frame( week = week_prepandemic_iran, pre_pandemic_iran)

iran_median_prepandemic = median(iran_statistics_prepandemic$`pre_pandemic_iran`)
iran_median_prepandemic

iran_mean_prepandemic = mean(iran_statistics_prepandemic$pre_pandemic_iran)
iran_mean_prepandemic

iran_sd_prepandemic = sd(iran_statistics_prepandemic$pre_pandemic_iran)
iran_sd_prepandemic

iran_var_prepandmic = var(iran_statistics_prepandemic$pre_pandemic_iran)
iran_var_prepandmic

# Statistics Analysis of Iran Post Pandemic

post_pandemic_iran = rowSums(iran[,c(2:4)][c(36:51),])
week_postpandemic_iran = iran$Week [c(36:51)]
iran_statistics_postpandemic = data.frame( week = week_postpandemic_iran, post_pandemic_iran)

iran_median_postpandemic = median(iran_statistics_postpandemic$post_pandemic_iran)
iran_median_postpandemic

iran_mean_postpandemic = mean(iran_statistics_postpandemic$post_pandemic_iran)
iran_mean_postpandemic

iran_sd_postpandemic = sd(iran_statistics_postpandemic$post_pandemic_iran)
iran_sd_postpandemic

iran_var_postpandemic = var(iran_statistics_postpandemic$post_pandemic_iran)
iran_var_postpandemic

# Statistics Analysis of Iran Pre vs Post

iran_prevspost = data.frame(stats_c = c("Median", "Mean", "SD", "Variance"), 
                             pre_pandemic_stats_iran = c(iran_median_prepandemic, iran_mean_prepandemic, iran_sd_prepandemic, iran_var_prepandmic),
                             post_pandemic_stats_iran = c(iran_median_postpandemic, iran_mean_postpandemic, iran_sd_postpandemic, iran_var_postpandemic))

# Statistics Analysis of Italy Pre Pandemic

pre_pandemic_italy = rowSums(italia[,c(2:4)][c(1:36),])
week_prepandemic_italy = italia$Week [c(1:36)]
italy_statistics_prepandemic = data.frame( week = week_prepandemic_italy, pre_pandemic_italy)

italy_median_prepandemic = median(italy_statistics_prepandemic$pre_pandemic_italy)
italy_median_prepandemic

italy_mean_prepandemic = mean(italy_statistics_prepandemic$pre_pandemic_italy)
italy_mean_prepandemic

italy_sd_prepandemic = sd(italy_statistics_prepandemic$pre_pandemic_italy)
italy_sd_prepandemic

italy_var_prepandmic = var(italy_statistics_prepandemic$pre_pandemic_italy)
italy_var_prepandmic

# Statistics Analysis of Italy Post Pandemic

post_pandemic_italy = rowSums(italia[,c(2:4)][c(42:51),])
week_postpandemic_italy = italia$Week [c(42:51)]
italy_statistics_postpandemic = data.frame( week = week_postpandemic_italy, post_pandemic_italy)

italy_median_postpandemic = median(italy_statistics_postpandemic$post_pandemic_italy)
italy_median_postpandemic

italy_mean_postpandemic = mean(italy_statistics_postpandemic$post_pandemic_italy)
italy_mean_postpandemic

italy_sd_postpandemic = sd(italy_statistics_postpandemic$post_pandemic_italy)
italy_sd_postpandemic

italy_var_postpandemic = var(italy_statistics_postpandemic$post_pandemic_italy)
italy_var_postpandemic

# Statistics Analysis of Italy Pre vs Post

italy_prevspost = data.frame(stats_c = c("Median", "Mean", "SD", "Variance"), 
                              pre_pandemic_stats_itl = c(italy_median_prepandemic, italy_mean_prepandemic, italy_sd_prepandemic, italy_var_prepandmic),
                              post_pandemic_stats_itl = c(italy_median_postpandemic, italy_mean_postpandemic, italy_sd_postpandemic, italy_var_postpandemic))

# Statistics Analysis of Canada Pre Pandemic

pre_pandemic_canada = rowSums(canada[,c(2:4)][c(1:26),])
week_prepandemic_canada = canada$Week [c(1:26)]
canada_statistics_prepandemic = data.frame( week = week_prepandemic_canada, pre_pandemic_canada)

canada_median_prepandemic = median(canada_statistics_prepandemic$pre_pandemic_canada)
canada_median_prepandemic

canada_mean_prepandemic = mean(canada_statistics_prepandemic$pre_pandemic_canada)
canada_mean_prepandemic

canada_sd_prepandemic = sd(canada_statistics_prepandemic$pre_pandemic_canada)
canada_sd_prepandemic

canada_var_prepandmic = var(canada_statistics_prepandemic$pre_pandemic_canada)
canada_var_prepandmic

# Statistics Analysis of Canada Post Pandemic

post_pandemic_canada = rowSums(canada[,c(2:4)][c(30:51),])
week_postpandemic_canada = canada$Week [c(30:51)]
canada_statistics_postpandemic = data.frame( week = week_postpandemic_canada, post_pandemic_canada)

canada_median_postpandemic = median(canada_statistics_postpandemic$post_pandemic_canada)
canada_median_postpandemic

canada_mean_postpandemic = mean(canada_statistics_postpandemic$post_pandemic_canada)
canada_mean_postpandemic

canada_sd_postpandemic = sd(canada_statistics_postpandemic$post_pandemic_canada)
canada_sd_postpandemic

canada_var_postpandemic = var(canada_statistics_postpandemic$post_pandemic_canada)
canada_var_postpandemic

# Statistics Analysis of Canda Pre vs Post

canada_prevspost = data.frame(stats_c = c("Median", "Mean", "SD", "Variance"), 
                              pre_pandemic_stats_c = c(canada_median_prepandemic, canada_mean_prepandemic, canada_sd_prepandemic, canada_var_prepandmic),
                              post_pandemic_stats_c = c(canada_median_postpandemic, canada_mean_postpandemic, canada_sd_postpandemic, canada_var_postpandemic))

########################### Analysis of three countries pre and post pandemic by its total mental health issues #####################

# Median Italy Pre Pandemic Total Mental Health Issues

pre_pandemic_italy_n = rowSums(italia[,c(2:10)][c(1:38),])  # we chose 2020-03-01 as date for defining pre and post pandemic
week_prepandemic_italy_n = italia$Week [c(1:38)]
italy_statistics_prepandemic_n = data.frame( week = week_prepandemic_italy_n, pre_pandemic_italy_n)

italy_median_prepandemic_n = median(italy_statistics_prepandemic_n$pre_pandemic_italy_n)
italy_median_prepandemic_n  # 549


# Median Italy Post Pandemic Total Mental Health Issues

post_pandemic_italy_n = rowSums(italia[,c(2:10)][c(39:51),])
week_postpandemic_italy_n = italia$Week [c(39:51)]
italy_statistics_postpandemic_n = data.frame( week = week_postpandemic_italy_n, post_pandemic_italy_n)

italy_median_postpandemic_n = median(italy_statistics_postpandemic_n$post_pandemic_italy_n)
italy_median_postpandemic_n  #568

# Italy analysis pre and post pandemic graph

Total_mental_health_issues_itl = rowSums(italia[,c(2:10)])
itl_at = data.frame(week = italia$Week, Total_mental_health_issues_itl)

library(ggplot2)
ggplot (itl_at,aes(x = `week`, y = `Total_mental_health_issues_itl`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Italy Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week") + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=2, cex = 2, col= "red") +
  geom_segment(x= as.numeric(as.Date("2019-06-16")),xend= as.numeric(as.Date("2020-03-01")),y=549,yend=549, linetype = 2,  cex = 1, col = "Blue") +
  geom_segment(x= as.numeric(as.Date("2020-03-03")),xend= as.numeric(as.Date("2020-05-31")),y=568,yend=568, linetype = 2,  cex = 1, col = "Green")


# Median Iran Pre Pandemic Total Mental Health Issues

pre_pandemic_iran_n = rowSums(iran[,c(2:10)][c(1:36),])  # we chose 2020-02-16 as date for defining pre and post pandemic
week_prepandemic_iran_n = iran$Week [c(1:36)]
iran_statistics_prepandemic_n = data.frame( week = week_prepandemic_iran_n, pre_pandemic_iran_n)

iran_median_prepandemic_n = median(iran_statistics_prepandemic_n$`pre_pandemic_iran_n`)
iran_median_prepandemic_n  # 415

# Median Iran Post Pandemic Total Mental Health Issues

post_pandemic_iran_n = rowSums(iran[,c(2:10)][c(37:51),])
week_postpandemic_iran_n = iran$Week [c(37:51)]
iran_statistics_postpandemic_n = data.frame( week = week_postpandemic_iran_n, post_pandemic_iran_n)

iran_median_postpandemic_n = median(iran_statistics_postpandemic_n$post_pandemic_iran_n)
iran_median_postpandemic_n  # 400

# Iran analysis pre and post pandemic graph

Total_mental_health_issues_iran = rowSums(iran[,c(2:10)])
iran_at = data.frame(week = iran$Week, Total_mental_health_issues_iran)

library(ggplot2)
ggplot (iran_at,aes(x = `week`, y = `Total_mental_health_issues_iran`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Iran Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week") + 
  geom_vline(xintercept = as.numeric(as.Date("2020-02-16")), linetype=2, cex = 2, col= "red") +
  geom_segment(x= as.numeric(as.Date("2019-06-16")),xend= as.numeric(as.Date("2020-02-16")),y=415,yend=415, linetype = 2,  cex = 1, col = "Blue") +
  geom_segment(x= as.numeric(as.Date("2020-02-17")),xend= as.numeric(as.Date("2020-05-31")),y=400,yend=400, linetype = 2,  cex = 1, col = "Green")


# Median Canada Pre Pandemic Total Mental Health Issues

pre_pandemic_canada_n = rowSums(canada[,c(2:10)][c(1:27),])  # we chose 2019-12-15 as date for defining pre and post pandemic
week_prepandemic_canada_n = canada$Week [c(1:27)]
canada_statistics_prepandemic_n = data.frame( week = week_prepandemic_canada_n, pre_pandemic_canada_n)

canada_median_prepandemic_n = median(canada_statistics_prepandemic_n$pre_pandemic_canada_n)
canada_median_prepandemic_n  # 651


# Median Canada Post Pandemic Total Mental Health Issues

post_pandemic_canada_n = rowSums(canada[,c(2:10)][c(28:51),])
week_postpandemic_canada_n = canada$Week [c(28:51)]
canada_statistics_postpandemic_n = data.frame( week = week_postpandemic_canada_n, post_pandemic_canada_n)

canada_median_postpandemic_n = median(canada_statistics_postpandemic_n$post_pandemic_canada_n)
canada_median_postpandemic_n  #658

# Canada analysis pre and post pandemic graph

Total_mental_health_issues_canada = rowSums(canada[,c(2:10)])
canada_at = data.frame(week = canada$Week, Total_mental_health_issues_canada)

library(ggplot2)
ggplot (canada_at,aes(x = `week`, y = `Total_mental_health_issues_canada`)) + geom_line() + scale_x_date(date_labels = "%Y-%m") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("Canada Total Mental Health Issues across time") + ylab("Total Mental Health Issues") + xlab("Week") + 
  geom_vline(xintercept = as.numeric(as.Date("2019-12-15")), linetype=2, cex = 2, col= "red") +
  geom_segment(x= as.numeric(as.Date("2019-06-16")),xend= as.numeric(as.Date("2019-12-15")),y=651,yend=651, linetype = 2,  cex = 1, col = "Blue") +
  geom_segment(x= as.numeric(as.Date("2019-12-16")),xend= as.numeric(as.Date("2020-05-31")),y=658,yend=658, linetype = 2,  cex = 1, col = "Green")

