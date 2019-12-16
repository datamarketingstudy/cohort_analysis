library(data.table)
library(dplyr)
library(ggplot2)
library(prettyR)
library(lubridate)
library(skimr)
library(extrafont)
loadfonts()
library(ggpubr)
library(knitr)
library(DT)

# File load

df_trans <- fread("Retail_Data_Transactions.csv", stringsAsFactors = FALSE)
df_respo <- fread("Retail_Data_Response.csv", stringsAsFactors = FALSE)

str(df_trans)
str(df_respo)

# Convert Data Type

df_trans$trans_date <- dmy(df_trans$trans_date)


# EDA

ggplot(data = df_trans, aes(x = tran_amount, y = ..density..)) +
  geom_histogram(fill = "cornsilk", color = "grey60", size = 0.2) +
  geom_density() +
  xlim(0, 105)

ggplot(data = df_trans, aes(x = tran_amount)) +
  geom_density(fill = "skyblue", size = 0.8, alpha = 0.6) +
  ggtitle("Density plot of Transaction data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"))
ggsave("densityPlot.jpg", dpi = 300)


# 코호트 분석

cohort <- df_trans
cohort$Year <- as.numeric(format(df_trans$trans_date, '%Y'))

cohort2014 <- cohort %>% 
  filter(Year == 2014) %>%
  select(customer_id, trans_date, Year) %>%
  arrange(customer_id, trans_date)
str(cohort2014)

# Getting the first transaction dates for each customer
join.date <- aggregate(trans_date ~ customer_id, cohort2014, min, na.rm = TRUE)

# Changing the name of the column InvoiceDate to Join_Date
# since this is the first transaction date for each customer
colnames(join.date)[2] <- "Join_Date"

# Merge the Join date data to the cohort2011 data frame
cohort2014 <- merge(cohort2014, join.date, by.x = "customer_id",by.y = "customer_id", all.x = TRUE)

# Creating the groups/Cohorts based on the join date month
cohort2014$Cohort <- as.numeric(format(cohort2014$Join_Date, "%m"))

# This visualization is not necessary. We're using this
# interactive table to confirm that the join data is correct. 

DT::datatable(head(cohort2014,500),
              filter = 'top',
              rownames = FALSE,
              options = list(
                pageLength = 10,
                pageLength = c(10,20,30,40,50)))

# Calculating the difference in days between the trans date column by join date column
# There is no option for month, but getting the month from the days is simple division
cohort2014$Age_by_Day <- as.numeric(difftime(cohort2014$trans_date,cohort2014$Join_Date,units = c("days")))

# Dividing the days by 30 to get the number of months
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

cohort2014$Age_by_Month <- elapsed_months(cohort2014$trans_date, cohort2014$Join_Date)

# Dumping the day element from the join date column
cohort2014$Join_Date <- format(cohort2014$Join_Date, "%Y-%m")


# Now we remove the day element from the InvoiceDate data since
# this Cohort Analysis is based on monthly activity.
cohort2014$trans_date <- format(cohort2014$trans_date, "%Y-%m")

# Its important that we remove the day data so that we can remove
# extra observations in the months where customers have multiple
# observations in a single month due to having multiple orders in
# a single month. We'll use the function duplicated for this later

# We relabel the cohort column data to something more intuitive for the sake
# of the report consumers, then factor them since these are sequential
groups <- c("Jan Cohorts",
            "Feb Cohorts",
            "Mar Cohorts",
            "Apr Cohorts",
            "May Cohorts",
            "Jun Cohorts",
            "Jul Cohorts",
            "Aug Cohorts",
            "Sep Cohorts",
            "Oct Cohorts",
            "Nov Cohorts",
            "Dec Cohorts")
for(i in 1:12){
  cohort2014[cohort2014$Cohort==i,"Cohort"] <- groups[i]
}
rm(i,groups)

cohort2014$Cohort <- factor(cohort2014$Cohort,ordered = T,levels =c("Jan Cohorts",
                                                                    "Feb Cohorts",
                                                                    "Mar Cohorts",
                                                                    "Apr Cohorts",
                                                                    "May Cohorts",
                                                                    "Jun Cohorts",
                                                                    "Jul Cohorts",
                                                                    "Aug Cohorts",
                                                                    "Sep Cohorts",
                                                                    "Oct Cohorts",
                                                                    "Nov Cohorts",
                                                                    "Dec Cohorts"))

DT::datatable(head(cohort2014,500),
              filter = 'top',
              rownames = FALSE,
              options = list(
                pageLength = 10,
                pageLength = c(10,20,30,40,50)))


# The day and month Age variables keep us from removing
# duplicates which is why we need to exclude them both
dupes <- which(duplicated(cohort2014[,c(-5,-6)]))

# Removing the duplicate observations
cohort2014 <- cohort2014[-dupes,]


# Dropping to the dupes vector
# for memory efficiency
rm(dupes)

# Creating rows for each cohort group
# Creating columns for each value in the Age_by_Month column;0-11
# The default aggregation setup for dcast is, fun.aggregate = length
cohorts.wide <- reshape2::dcast(cohort2014,Cohort~Age_by_Month,
                                value.var="customer_id",
                                fun.aggregate = n_distinct)


# Cloning the output for retention and churn mixpanels
# to be used later
cw.retention <- cohorts.wide
cw.churn <- cohorts.wide

# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks <- quantile(cohorts.wide[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155) } )


# The Retention Mixpanel with counts
DT::datatable(cohorts.wide,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 12) ) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatStyle(names(cohorts.wide[c(-1,-2)]),fontWeight = 'bold',color = 'white', backgroundColor = styleInterval(breaks,colors))


# Calculating the percentages. month number/join month number
# DT will handle the *100 and % formating.
# The sequence needs to be reversed because if the first
# column is worked on, everything else will be divided by 1.
# Instead of formatting column 0 to show 100% for each row, it seems
# more useful to leave this as the original count, showing how
# many new customers were acquired in its respective month. This
# is why the for loop ends right before column 0.
for (i in rev(3:ncol(cw.retention))){
  cw.retention[,i] <- round(cw.retention[,i]/cw.retention[,2],4)
}
rm(i)

# Cloning the retention mixpanel
retention.avgs <- cw.retention

# When calculating the column averages, 0 won't get ignored,
# which is a problem. Converting these 0 to NAs solves this issue.
retention.avgs[retention.avgs == 0.0000] <- NA
avgs.ret <- round(apply(retention.avgs[,-1],2,mean, na.rm=TRUE),4)

# We use the zero because this is a numerical vector
# Changing it after the merge can't happen due to the
# factoring of the Cohort labels
avgs.ret <- c(0,avgs.ret)

# Adding the averages row to the retention mixpanel
cw.retention <- rbind(cw.retention,avgs.ret)
cw.retention[13,2] <- round(cw.retention[13,2],0)

# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks <- quantile(cw.retention[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155) } )


# The retention rate mixpanel
DT::datatable(cw.retention,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 13) ) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),1) %>% # We don't want column 0 in %
  formatStyle("1", fontWeight = 'bold') %>%
  formatStyle(names(cw.retention[c(-1,-2)]),color = 'white',fontWeight = 'bold', backgroundColor = styleInterval(breaks,colors))


# Calculating the percentages. month number/join month number
# DT will handle the *100 and % formating.
# The sequence needs to be reversed because if the first
# column is worked on, everything else will be divided by 1.
# Instead of formatting column 0 to show 100% for each row, it seems
# more useful to leave this as the original count, showing how
# many new customers were acquired in its respective month. This
# is why the for loop ends right before column 0.
for (i in rev(3:ncol(cw.churn))){
  
  #Calculates the retention rate
  cw.churn[,i] <- round(cw.churn[,i]/cw.churn[,2],4)
  
  # Turns the retention rate into the churn rate. The ifelse
  # part is to avoid doing any calculations to the zeros.
  cw.churn[,i] <- ifelse(cw.churn[,i] !=0, 1.0-cw.churn[,i], 0+cw.churn[,i])
}
rm(i)

# Cloning the churn mixpanel
churn.avgs <- cw.churn

# When calculating the column averages, 0 gets included in the calculations,
# This is a problem. Converting these 0 to NAs solves this issue thanks to na.rm.
churn.avgs[churn.avgs == 0.0000] <- NA
avgs.chu <- round(apply(churn.avgs[,-1],2,mean, na.rm=TRUE),4)

# We use the zero because this is a numerical vector
# Changing it after the merge can't happen due to the
# factoring of the Cohort labels
avgs.chu <- c(0,avgs.chu)

# Adding the averages row to the retention mixpanel
cw.churn <- rbind(cw.churn,avgs.chu)
cw.churn[13,2] <- round(cw.churn[13,2],0)


# Creating 19 breaks and 20 rgb color values ranging from red to white
breaks2 <- quantile(cw.churn[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors2 <- sapply(round(seq(255, 40, length.out = length(breaks2) + 1), 0),
                  function(x){ rgb(255,x,x, maxColorValue = 255) } )


# The churn rate mixpanel
DT::datatable(cw.churn,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 13) ) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatPercentage(c(3:13),1) %>% # We don't want column 0 in %
  formatStyle("1", fontWeight = 'bold') %>%
  formatStyle(names(cw.churn[c(-1,-2)]),color = 'white',fontWeight = 'bold', backgroundColor = styleInterval(breaks2,colors2))
