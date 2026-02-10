library('tidyverse')

getwd()

setwd('C:/Users/jbuendia/Documents/Learning/CS85/Project')

dir()

df <- read_csv("DataFrame.csv")

View(df)

#I renamed the columns
df <- df %>%
  rename(CPID = `Child ID`,Dom_Reg_Sen = `0.0 - Regulatory / Sensory Org.`, Dom_Cog = `1.0 - Cognitive`, Dom_Lan_Rec =`2.0I - Receptive`,Dom_Lan_Exp = `2.0II - Expressive`,Dom_Gros = `3.0 - Gross Motor`, Dom_FM =`4.0 - Fine Motor`, Dom_Soc =`5.0 - Social - Emotional`, Dom_Self = `6.0 - Self-Help`)

#I remove any rows with missing CPID values
df_clean <- df %>% 
  filter(!is.na(CPID))

#I view it again
View(df_clean)

#I mutate the columns into their data types for analysis and drop the redundnat ones
df_clean <- df_clean %>% 
  mutate(
    CPID = as.character(CPID),
    Assessment_Period = factor(Assessment_Period),
    Site = factor(Site),
    Caseload = factor(Caseload),
    Gen = factor (Gen), 
    Zip = as.character(Zip),
    Dual = factor(Dual),
   Year = as.integer(Year)
  ) %>%
  select(
    -Gen, -Dual
  )

#I view the cleaned data frame
View(df_clean)

#I take a glimpse at the data frame and summary statistics
glimpse(df_clean)

summary(df_clean)

#I will see counts of assessments for children
help_counts <- df_clean %>%
  count(CPID, name = "count_assessments")

#I view the counts of assessments for children
View(help_counts)

#I filter the data frame to include only children with 3 assessments
filtered_children <- df_clean %>%
  inner_join(help_counts %>% filter(count_assessments == 3),
             by = "CPID")
#I view the filtered data frame
View(filtered_children)

#I check the number of unique children in the filtered data frame
length(unique(filtered_children$CPID))

#I discrentalize the help categories
categories_rating <- c(
  "increased rate of development significantly but continues within less than age expected range" = 1,
  "continued or decreased  rate of developent within less than age expected range" = 1,
  "increased rate of development to age expected range" = 2,
  "maintained or increased rate of development within greater than age expected range" = 2,
  "maintained or increased rate of development within age expected range" = 3,
  "increased rate of development to greater than age expected range" = 3
)
#i get the average from each child and add a row
df_scored <- filtered_children %>%
  mutate(across(starts_with("Dom_"),~ as.numeric(categories_rating[.]))) %>%
  mutate(avg_1 = rowMeans(select(., starts_with("Dom_")), na.rm = TRUE))

#I rename the column
df_scored <- df_scored %>%
  rename(ave_scores = avg_1)

#i check the column names
colnames(df_scored)

#I pivot the data frame to have the average scores for each assessment period in separate columns
df_wide <- df_scored %>%
  select(CPID, Assessment_Period, ave_scores) %>%
  pivot_wider(
    names_from = Assessment_Period, 
    values_from = ave_scores,
    names_prefix = "P"
  )

#I view the pivoted data frame
View(df_wide)

#I calculate the growth between the first and second assessment periods
growth_1 <- df_wide %>%
  mutate(result1 = P2-P1)

#I view the growth between the first and second assessment periods
View(growth_1)

#I calculate the growth between the second and third assessment periods
overallresults <- df_wide %>%
  mutate(
    Growth_P1_P2 = P2 - P1,
    Growth_P2_P3 = P3 - P2,
    Growth_P1_P3 = P3 - P1
  )

#I view the overall results with the growth calculations
View(overallresults)

#I remove the redundant growth calculation between the first and third assessment periods
overallresults <- overallresults %>%
  select(-Growth_P1_P3)

#I view the overall results with the redundant growth calculation removed
View(overallresults)

#I calculate the rate of change in growth between the first and second assessment periods and the second and third assessment periods
overallresults <- overallresults %>%
  mutate(
    rate_of_change = Growth_P2_P3 - Growth_P1_P2
  )

#I view the overall results with the rate of change calculation
head(overallresults,5)

#any rate of change that is negative showsa decline growth, positive show increase in growth, 0 indicates steady growth
