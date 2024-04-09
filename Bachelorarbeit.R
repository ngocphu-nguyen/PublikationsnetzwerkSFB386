###### Load necessary libraries
library(dplyr)
library(tidyr)
library("ISIPTA.eProceedings")
library(igraph)
library(plyr)
library(reshape2)
library(tidyverse)

##### Process raw_papers.R
for (year in 1995:2006) {
  # Construct the variable name for the current year
  variable_name <- paste("papers", year, sep = "_")
  
  # Use get() to access the data for the current year
  year_data <- get(variable_name)
  
  # Assume `year_data` is a string containing all the papers for the year,
  # split it into lines
  lines <- unlist(strsplit(year_data, "\n"))
  
  # Initialize empty lists to store each component
  authors <- list()
  titles <- list()
  paper_numbers <- list()
  
  # Iterate over each line to extract information
  for(line in lines) {
    # Extract authors by capturing everything before the year
    author_match <- regmatches(line, regexpr("^(.+?)\\(\\d{4}\\):", line))
    # Replace ' and ' with '; '
    cleaned_authors <- gsub(" and ", "; ", gsub("\\s\\(\\d{4}\\):", "", author_match))
    authors <- c(authors, cleaned_authors)
    
    # Extract titles by capturing everything between ": " and ". Collaborative"
    title_match <- regmatches(line, regexpr("(?<=: ).+?(?=\\. Collaborative)", line, perl=TRUE))
    titles <- c(titles, title_match)
    
    # Extract paper numbers by capturing digits following "Discussion Paper"
    number_match <- regmatches(line, regexpr("(?<=Discussion Paper )\\d+", line, perl=TRUE))
    paper_numbers <- c(paper_numbers, number_match)
  }
  
  # Combine the lists into a dataframe
  papers <- data.frame(
    author = unlist(authors),
    title = unlist(titles),
    id = unlist(paper_numbers),
    stringsAsFactors = FALSE
  )
  
  papers <- papers %>%
    mutate(author = strsplit(as.character(author), ";\\s*")) %>%
    unnest(author) %>%
    mutate(has_comma = grepl(",", author)) %>%
    mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
    separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
    unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
    mutate(author = trimws(author)) %>%
    select(-has_comma) %>%
    mutate(year = year)
  
  # Assign the processed dataframe back to a variable (can replace the original or create a new one)
  assign(variable_name, papers)
}

##### Add all of the years
# all_papers <- rbind(papers_1996, papers_1997)
# all_papers <- papers_1996
all_papers <- rbind(papers_1995, papers_1996, papers_1997, papers_1998, papers_1999, papers_2000, papers_2001, papers_2002, papers_2003, papers_2004, papers_2005, papers_2006)
all_papers <- as.data.frame(select(all_papers, year, id, author, -title))

##### 
all_papers$year <- as.integer(all_papers$year)
all_papers$id <- as.integer(all_papers$id)
all_papers$author <- as.factor(all_papers$author)

authors_locations <- all_papers
papers_authors <- all_papers

str(all_papers)
str(papers_authors)
