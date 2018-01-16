install.packages("bigrquery")
install.packages('devtools')
devtools::install_github("rstats-db/bigrquery")
library(bigrquery)

##==== Tutorial 1 ====

# install.packages('devtools') devtools::install_github("rstats-db/bigrquery")

# Use your project ID here
project <- "favorita-190722" # put your project ID here

# Example query - select copies of files with content containing "TODO"
sql <- "SELECT SUM(copies)
FROM [bigquery-public-data.github_repos.sample_contents]
WHERE NOT binary AND content LIKE '%TODO%'"

# Execute the query and store the result
todo_copies <- query_exec(sql, project = project, useLegacySql = FALSE)


#Next, let's select the count of copies for files and draw a pie chart showing the ratio of files containing TODO strings:

# Example query - select count of copies in the sample contents table
sql <- "SELECT SUM(copies)
FROM [bigquery-public-data.github_repos.sample_contents]
WHERE NOT binary"

# Execute the query and store the result
sum_copies <- query_exec(sql, project = project, useLegacySql = FALSE)

copy_counts <- c(sum_copies[1,1] - todo_copies[1,1], todo_copies[1,1])
labels <- c("TODO", "NO TODO")
pie(rev(copy_counts), labels = labels, col=heat.colors(2),
    main="Files containing \"TODO\"")

# Store the average for later
mean_todo <- 100 * todo_copies[1,1] / sum_copies[1,1]



##==== Tutorial 2 ====
# Use your project ID here
project <- "favorita-190722" # put your project ID here


# Standard SQL query string for count of copies by language name.
sql <- 'CREATE TEMPORARY FUNCTION maxBytesLanguage(languages ARRAY<STRUCT<name STRING, bytes INT64>>)

RETURNS STRING LANGUAGE js AS """
var out = ""
var maxBytes = 0
for (var i=0; i < languages.length; i++) {
var lang = languages[i]
if (lang.bytes > maxBytes) {
maxBytes = lang.bytes
out = lang.name
}
}
return out
""";

SELECT SUM(copies) as copies, language_name
FROM (
SELECT
sc.copies as copies,
maxBytesLanguage(l.language) as language_name
FROM [bigquery-public-data.github_repos.sample_contents] as sc
JOIN [bigquery-public-data.github_repos.languages] as l
ON sc.sample_repo_name = l.repo_name
WHERE NOT binary
)
WHERE language_name in ("Awk","C", "C++", "C#", "Go", "Haskell", "Java",
"JavaScript", "Objective-C", "PHP", "Python", "R",
"Ruby", "Shell", "Swift", "Yacc")
GROUP BY language_name
ORDER BY language_name asc'


# Standard SQL query string for total count of copies by language name.
total_copy_counts <- query_exec(sql, project = project, useLegacySql = FALSE)

# Query string for count of file copies by language name where file contains TODO.
sql <- 'CREATE TEMPORARY FUNCTION maxBytesLanguage(languages ARRAY<STRUCT<name STRING, bytes INT64>>)

RETURNS STRING LANGUAGE js AS """
var out = ""
var maxBytes = 0
for (var i=0; i < languages.length; i++) {
var lang = languages[i]
if (lang.bytes > maxBytes) {
maxBytes = lang.bytes
out = lang.name
}
}
return out
""";

SELECT SUM(copies) as copies, language_name
FROM (
SELECT
sc.copies as copies,
maxBytesLanguage(l.language) as language_name
FROM [bigquery-public-data.github_repos.sample_contents] as sc
JOIN [bigquery-public-data.github_repos.languages] as l
ON sc.sample_repo_name = l.repo_name
WHERE NOT binary AND content LIKE "%TODO%"
)
WHERE language_name in ("Awk","C", "C++", "C#", "Go", "Haskell", "Java",
"JavaScript", "Objective-C", "PHP", "Python", "R",
"Ruby", "Shell", "Swift", "Yacc")
GROUP BY language_name
ORDER BY language_name asc'


# Standard SQL query string for total count of copies by language name.
total_copy_counts <- query_exec(sql, project = project, useLegacySql = FALSE)

# Store total copy counts for files by language
todo_copy_counts <- query_exec(sql, project = project, useLegacySql = FALSE)

# Calculate the % copies with TODO comments relative to mean from earlier.
lang_ratios <- c((100 * todo_copy_counts$copies / total_copy_counts$copies) - mean_todo)

# Manually clean up the Objective-C label
total_copy_counts$l_language_name[9] <-"Obj-C"

# Sort
data <- data.frame(lang_ratios, total_copy_counts$l_language_name)
data <- data[order(data[1], decreasing=FALSE),]

# Plot the results
barplot(data$lang_ratios,
        names.arg = data$total_copy_counts.l_language_name,
        ylab="% having TODO", las=2, ylim=c(-2,0.5) )


# Tutorial 3 --------------------------------------------------------------

library('bigrquery')
project <- "favorita-190722" # put your project ID here
# Create a data frame to store results by year
year_results <- data.frame(matrix(nrow = 0, ncol = 0))

sql <- paste("SELECT MAX(max) as HIGH, state, year ",
             "FROM ",
             "  ( ",
             "    SELECT max, ",
             "      stn as istn,",
             "      wban as iwban, year",
             "    FROM `bigquery-public-data.noaa_gsod.gsod*`",
             "  ) a ",
             "JOIN `bigquery-public-data.noaa_gsod.stations` b ",
             "ON a.istn = b.usaf ",
             "  AND a.iwban = b.wban ",
             "WHERE state in (",
             "  'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', ",
             "  'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', ",
             "  'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', ",
             "  'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', ",
             "  'VA', 'VT', 'WA', 'WI', 'WV', 'WY')",
             "  AND max < 1000 ",
             "  AND country = 'US' ",
             "GROUP BY state, year ",
             "ORDER BY year desc, state asc",
             sep = "")
res <- query_exec(sql, project = project, use_legacy_sql = FALSE)

# Store result for this table query in year_results
if (nrow(year_results) == 0) {
  year_results <- res
} else {
  year_results <- rbind(year_results, res)
}

# Create mapping of state name to rainbow color
colIndex <- c(1:50)
abbrevs <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA',
             'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME',
             'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM',
             'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX',
             'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')
names(colIndex) <- abbrevs

# Plot data using color defined by state
plot(year_results$year, year_results$HIGH, xlab="YEAR", ylab="Temp",
     col=rainbow(50)[colIndex[year_results$state]],
     ylim=c(60, 140), xlim=c(1980, 2015))
plot(year_results$year, year_results$HIGH, xlab="YEAR", ylab="Temp",
     col=rainbow(50)[colIndex[year_results$state]],
     ylim=c(60, 140), xlim=c(1980, 2015))
plot(year_results$year, year_results$HIGH, xlab="YEAR", ylab="Temp",
     col=rainbow(50)[colIndex[year_results$state]],
     ylim=c(80, 140), xlim=c(2005, 2015))
