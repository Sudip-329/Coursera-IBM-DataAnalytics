# Installing Libraries
install.packages("httr")
install.packages("rvest")

library(httr)
library(rvest)



# Task - 1
get_wiki_covid19_page <- function(){
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  query_params <- list(title = "Template:COVID-19_testing_by_country")
  response <- GET(url = wiki_base_url, query= query_params)
  return(response)
}

covid19_page_response <- get_wiki_covid19_page()
print(covid19_page_response)




# Task - 2
html_content <- content(covid19_page_response, as = "text")
html_root <- read_html(html_content)
html_root

table_nodes <- html_nodes(html_root, "table")
covid_table_node <- table_nodes[2]
covid_table_node

covid_table_df <- as.data.frame(html_table(table_nodes[2], fill = TRUE))
print(covid_table_df)




# Task - 3
summary(covid_table_df)

preprocess_covid_data_frame <- function(data_frame) {
  shape <- dim(data_frame)
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  return(data_frame)
}

processed_covid_df <- preprocess_covid_data_frame(covid_table_df)

summary(processed_covid_df)

write.csv(processed_covid_df, "covid.csv", row.names = FALSE)




# Task - 4
covid_data_frame_csv <- read.csv("covid.csv", header = TRUE)
selected_rows <- covid_data_frame_csv[5:10, c("country", "confirmed")]
print(selected_rows)




# Task - 5
total_confirmed_cases <- sum(covid_data_frame_csv$confirmed, na.rm = TRUE)
total_tested_cases <- sum(covid_data_frame_csv$tested, na.rm = TRUE)
positive_ratio <- total_confirmed_cases / total_tested_cases
print(positive_ratio)




# Task - 6
country_column <- covid_data_frame_csv$country

country_class <- class(country_column)
print(paste("Class of country column:", country_class))

country_column_char <- as.character(country_column)

sorted_AtoZ <- sort(country_column_char)

sorted_ZtoA <- sort(country_column_char, decreasing = TRUE)

print(sorted_ZtoA)




# Task - 7
matched_countries <- grep("^United.+", covid_data_frame_csv$country, value = TRUE)
print(matched_countries)




# Task - 8
country_1 <- "United States"
subset_country_1 <- covid_data_frame_csv[covid_data_frame_csv$country == country_1, c("country", "confirmed", "confirmed.population.ratio")]
print(subset_country_1)

country_2 <- "Canada"
subset_country_2 <- covid_data_frame_csv[covid_data_frame_csv$country == country_2, c("country", "confirmed", "confirmed.population.ratio")]
print(subset_country_2)




# Task - 9
ratio_country_1 <- subset_country_1$confirmed.population.ratio
ratio_country_2 <- subset_country_2$confirmed.population.ratio

if (ratio_country_1 > ratio_country_2) {
  print(paste(country_1, "has a higher confirmed cases to population ratio:", ratio_country_1))
} else if (ratio_country_1 < ratio_country_2) {
  print(paste(country_2, "has a higher confirmed cases to population ratio:", ratio_country_2))
} else {
  print("Both countries have the same confirmed cases to population ratio.")
}




# Task - 10
threshold <- 0.01
low_risk_countries <- covid_data_frame_csv[covid_data_frame_csv$confirmed.population.ratio < threshold, c("country", "confirmed.population.ratio")]
print("Countries with confirmed to population ratio less than 1%:")
print(low_risk_countries)

#----------------ALL TASKS COMPLETED------------
  