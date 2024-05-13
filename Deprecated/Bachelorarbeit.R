###### Load necessary libraries
library(dplyr)
library(tidyr)
library("ISIPTA.eProceedings")
library(RColorBrewer)
library(igraph)
library(plyr)
library(reshape2)
library(tidyverse)
library(ggrepel)
library(extrafont)
library(xtable)

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
all_papers_title <- rbind(papers_1995, papers_1996, papers_1997, papers_1998, papers_1999, papers_2000, papers_2001, papers_2002, papers_2003, papers_2004, papers_2005, papers_2006)
all_papers <- as.data.frame(select(all_papers_title, year, id, author, -title))
# all_papers <- all_papers %>%
#    mutate(author = str_replace(author, "Leonhard Knorr-Held", "Leonhard Held"))

unique_authors <- all_papers %>%
  select(author) %>%
  distinct(author, .keep_all = TRUE) %>%
  mutate(author = as.character(author))

normalize_name <- function(name) {
  parts <- unlist(strsplit(name, split = "\\s+"))
  if (length(parts) > 1) {
    return(paste0(substring(parts[1], 1, 1), ". ", parts[length(parts)]))
  } else {
    return(name)
  }
}

unique_authors$normalized_author <- sapply(unique_authors$author, normalize_name)

# Find authors whose name is stored in both shortened and full format, e.g. S. Lang and Stefan Lang
duplicates <- unique_authors %>%
  group_by(normalized_author) %>%
  filter(n_distinct(author) > 1) %>%
  filter(normalized_author != author & normalized_author != "M. Smith") %>%
  arrange(normalized_author, author)

# Add cases of authors appearing twice due to other name inconsistencies: e.g. "ss" and "ß", or alternative names
# or replacing short with full names
name_cleaning <- data.frame(author = c("Albrecht Neiß", "Albrecht Neiß", "Günter Raßer", "Leonhard Held", "Volkmar Liebscher", "Christian Kastner", "Andreas Fieger", "Vinay Kumar Srivastava", "Olaf Wittich"), 
                       normalized_author = c("A. Neiss", "A. Neiß", "G. Rasser", "Leonhard Knorr-Held", "V. Liebscher", "C. Kastner", "A. Fieger", "V. K. Srivastava", "O. Wittich"))

duplicates <- bind_rows(duplicates, name_cleaning)

replace_names <- function(name) {
  match <- duplicates$normalized_author == name
  if (any(match)) {
    return(duplicates$author[match])
  } else {
    return(name)
  }
}

unique_authors$author <- sapply(unique_authors$author, replace_names)

unique_authors <- unique_authors %>%
  distinct(author, .keep_all = TRUE) %>%
  select(author) %>%
  arrange(author)

all_papers$author <- sapply(all_papers$author, replace_names)

##### 
all_papers$year <- as.integer(all_papers$year)
all_papers$id <- as.integer(all_papers$id)
all_papers$author <- as.factor(all_papers$author)

authors_locations <- all_papers
papers_authors <- all_papers

##### A map of authors and their id number
author_id <- unique_authors %>%
  select(author) %>%
  mutate(author_id = row_number(author))

##### Export author id table as Latex code
author_id_export <- data.frame(matrix(nrow = 101, ncol = 6))
author_id_export$X1 <- author_id$author[1:101]
author_id_export$X2 <- author_id$author_id[1:101]
author_id_export$X3 <- author_id$author[102:202]
author_id_export$X4 <- author_id$author_id[102:202]
author_id_export$X5 <- c(author_id$author[203:302], NA)
author_id_export$X6 <- c(author_id$author_id[203:302], NA)

latex_table <- print(xtable(author_id_export), include.rownames = FALSE, floating = FALSE, hline.after = NULL, comment = FALSE)



########## Deskriptive Analyse

##### Number of papers each year
npapers_year <- all_papers %>%
  distinct(year, id, .keep_all = TRUE) %>%
  group_by(year) %>%
  dplyr::summarise(papers = n()) %>%
  mutate(year = as.factor(year))

ggplot(npapers_year, aes(x = year, y = papers)) + 
  geom_col() +
  geom_text(aes(label = papers), vjust = -0.5) +
  labs(x = "Year", y = "Papers", fill = "Authors per paper") +
  labs(title = "Paper distribution") +
  theme_minimal()

pdf("./Deskriptive Analyse/Anzahl Papiere pro Jahr.pdf", width = 10, height = 5.6)
ggplot(npapers_year, aes(x = year, y = papers)) + 
  geom_point(size = 2.2, , color = "#4d9221") +
  geom_line(group = 1, color = "#4d9221", size = 1) +
  geom_label_repel(aes(label = papers),
                   nudge_y = c(-1.5, 1.8, 1.5, -1.5, -1.5, 1.5, -1.5, -1.5, 1.5, -1.5, 1.5, -1.5),
                   color = "#276419",
                   family = "CMU Serif") +
  scale_y_continuous(limits = c(0, 70)) +
  labs(x = "Jahr", y = "Anzahl") +
  theme_minimal() +
  theme(text = element_text(family = "CMU Serif", size = 16))
dev.off()

##### Papers per author
papers_per_author <- as.data.frame(t6)
papers_per_author <- papers_per_author %>%
  mutate(Var1 = factor(Var1))

pdf("./Deskriptive Analyse/Anzahl Papiere pro Autor.pdf", width = 10, height = 5.6)
ggplot(papers_per_author, aes(x = Var1, y = Freq)) + 
  geom_col(stat = "identity", fill = "#c51b7d") +
  geom_text(aes(label = Freq), vjust = -0.5, family = "CMU Serif", color = "#8e0152") +
  labs(x = "...Papieren", y = "Anzahl Autoren mit...") +
  scale_y_continuous(limits = c(0, 160)) +
  scale_x_discrete() +
  theme_minimal() +
  theme(text = element_text(family = "CMU Serif", size = 16))
dev.off()

##### Top 10 authors with the most paper contributions
author_asc <- authors_ncontributions %>%
  arrange(desc(ncontribs)) %>%
  mutate(author = paste(author, ' (', row_number(author), ')', sep=''))

top_authors <- authors_ncontributions %>%
  slice_max(order_by = ncontribs, n = 12) %>%
  mutate(pleiter = as.factor(ifelse(author %in% pleiter_name$author, "Projektleiter", "Kein Projektleiter")))

top_authors$author <- sapply(top_authors$author, add_numbering)
top_authors$author <- gsub(" \\(", "\n(", top_authors$author)

top_authors <- top_authors %>%
  mutate(author = fct_reorder(author, ncontribs, .desc=TRUE))

pdf("./Deskriptive Analyse/Top Autoren.pdf", width = 16, height = 6.9)
ggplot(top_authors, aes(x = author, y = ncontribs, fill = pleiter)) +
  geom_col() +
  geom_text(aes(label = ncontribs), vjust = -0.5, family = "CMU Serif", size = 6, color = "#67000d") +
  theme_minimal() +
  scale_fill_manual(values = c("#fcae91", "#cb181d")) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(text = element_text(family = "CMU Serif", size = 18),
        axis.text.x = element_text(size = 10.25),
        legend.position = c(0.8,0.8),
        legend.background = element_rect(fill="white", size=0.5)) +
  labs(x = "Autor",
       y = "Anzahl Papiere",
       fill = "Rolle")
dev.off()

##### Authors per paper
authors_per_paper <- papers_nauthors %>%
  group_by(nauthors) %>%
  dplyr::summarise(n = n()) %>%
  mutate(nauthors = as.factor(nauthors),
         n = as.numeric(n))

pdf("./Deskriptive Analyse/Autoren pro Papier.pdf", width = 10, height = 5.6)
ggplot(authors_per_paper, aes(x = nauthors, y = n)) +
  geom_col(fill = "#4d9221") + labs(x = "Authors", y = "Papers") +
  geom_text(aes(label = n), vjust = -0.5, family = "CMU Serif", color = "#276419", fontface = "bold") +
  scale_y_continuous(limits = c(0, 230)) +
  labs(x = "...Autoren",
       y = "Anzahl Papiere mit...") +
  theme_minimal() +
  theme(text = element_text(family = "CMU Serif", size = 16))
dev.off()

##### Projektleiter getrennt
pleiter_paper <- authors_ncontributions %>%
  mutate(pleiter = ifelse(author %in% pleiter_name$author, "Projektleiter", "Kein Projektleiter"))

pleiter_paper_y <- subset(pleiter_paper, pleiter_paper$pleiter == "Projektleiter")
hist(log(pleiter_paper_y$ncontribs))

pleiter_paper_n <- subset(pleiter_paper, pleiter_paper$pleiter != "Projektleiter")
hist(pleiter_paper_n$ncontribs)
  
##### Treppenfunktion von papers per author
papers_per_author <- as.data.frame(t6)
papers_per_author$Var1 <- as.numeric(papers_per_author$Var1)
papers_per_author$Freq <- as.numeric(papers_per_author$Freq)

# Create cumulative frequency
cumulative_freq <- cumsum(papers_per_author$Freq) / sum(papers_per_author$Freq)

##### Anzahl Papers von Projektleitern
pleiter_name <- author_id %>%
  filter(author %in% c("Dorothee P. Auer", "Thomas Augustin", "Claudia Czado", "Ludwig Fahrmeir",
                       "Alfred Hamerle", "Leonhard Held", "Stephan Klasen", "Claudia Klüppelberg",
                       "Helmut Küchenhoff", "Volkmar Liebscher", "Albrecht Neiß", "Iris Pigeot",
                       "Helmut Pruscha", "Georg Schmidt", "Hans Schneeweiß", "Helge Toutenburg",
                       "Gerhard Tutz", "Kurt Ulm", "Gerhard Winkler", "Olaf Wittich", "Klaus F. Zimmermann"))

pleiter <- authors_ncontributions %>%
  mutate(author_id = row_number(author)) %>%
  filter(author %in% pleiter_name$author) %>%
  mutate(author = paste(author, ' (', author_id, ')', sep='')) %>%
  mutate(author = fct_reorder(author, ncontribs, .desc=TRUE),
         ncontribs = as.factor(ncontribs)) %>%
  select(-author_id) %>%
  arrange(desc(ncontribs))

ggplot(pleiter, aes(x = author, y = ncontribs)) +
  geom_col() +
  geom_text(aes(label = ncontribs), vjust = -0.5) +
  theme_minimal()

##### Wer mit wem am meisten publiziert haben
add_numbering <- function(name) {
  # Find a match in the name_map
  match <- author_id$author == name
  if (any(match)) {
    # If a match is found, return the corresponding full name and numbering
    return(paste(name, ' (', author_id$author_id[match], ')', sep=''))
  } else {
    # If no match is found, return the original value
    return(name)
  }
}

top_pair <- coauthors_npairs
top_pair$author1 <- sapply(top_pair$author1, add_numbering)
top_pair$author2 <- sapply(top_pair$author2, add_numbering)

top_pair <- top_pair %>%
  filter(npairs >= 10) %>%
  arrange(desc(npairs)) %>%
  mutate(coauthor_pair = fct_reorder(paste(author1, ' - ', author2, sep=''), npairs, .desc = TRUE),
         npairs = as.factor(npairs)) %>%
  select(coauthor_pair, npairs, -author1, -author2)

ggplot(top_pair, aes(x = coauthor_pair, y = npairs)) +
  geom_col() +
  geom_text(aes(label = npairs), vjust = -0.5) +
  theme_minimal()

##### Wer am meisten ko-publiziert haben
top_coauthor <- papers_ncoauthors_overall %>%
  mutate(author = paste(author, ' (', rownames(papers_ncoauthors_overall), ')', sep='')) %>%
  # filter(ncoauthors >= 40) %>%
  mutate(author = fct_reorder(author, ncoauthors, .desc = TRUE)) %>%
  arrange(desc(ncoauthors))

ggplot(top_coauthor, aes(x = author, y = ncoauthors)) +
  geom_col() +
  geom_text(aes(label = ncoauthors), vjust = -0.5) +
  theme_minimal()

##### Wer mit am meisten unique Kollaborationen
top_unique_coauthor <- unique_coauthors %>%
  mutate(name = paste(name, ' (', rownames(unique_coauthors), ')', sep='')) %>%
  filter(ncoauthors >= 20) %>%
  mutate(name = fct_reorder(name, ncoauthors, .desc = TRUE)) %>%
  arrange(desc(ncoauthors))

ggplot(top_unique_coauthor, aes(x = name, y = ncoauthors)) +
  geom_col() +
  geom_text(aes(label = ncoauthors), vjust = -0.5) +
  theme_minimal()


############ Network analysis
#### Remove isolated nodes
lone_nodes <- V(graph)[degree(graph) == 0]
graph_no_isolated <- delete_vertices(graph, lone_nodes)
graph_no_isolated_id <- author_id %>%
  dplyr::filter(!(author %in% V(graph)[degree(graph) == 0]$name))

#### Longest shortest path
longest_path_df <- data.frame(name = V(graph)[get.diameter(graph)]$name)
longest_path_df$name <- sapply(longest_path_df$name, add_numbering)

#### Degree centrality
degree_centrality <- degree(graph_no_isolated)

degree_centrality_df <- data.frame(centrality = degree_centrality)
degree_centrality_df <- degree_centrality_df %>%
  mutate(name = row.names(degree_centrality_df)) %>%
  select(name, centrality)
degree_centrality_df$name <- sapply(degree_centrality_df$name, add_numbering)

#### Betweenness centrality
betweenness_centrality <- betweenness(graph_no_isolated, normalized = TRUE)

betweenness_centrality_df <- data.frame(centrality = betweenness_centrality)
betweenness_centrality_df <- betweenness_centrality_df %>%
  mutate(name = row.names(betweenness_centrality_df)) %>%
  select(name, centrality)
betweenness_centrality_df$name <- sapply(betweenness_centrality_df$name, add_numbering)

#### Community detection
##### Largest component
graph_decompose <- decompose(graph)[[1]]
graph_decompose_id <- V(graph_decompose)$name
graph_decompose_id <- author_id %>%
  dplyr::filter(author %in% graph_decompose_id)

clusters_girvan_decompose <- cluster_edge_betweenness(graph_decompose)

##### Clustering largest component
clusters_girvan <- cluster_edge_betweenness(graph_decompose) # 30 groups
community_girvan <- data.frame(Name = character(), Group = integer())
for (i in seq_along(communities(clusters_girvan))) {
  temp_df <- data.frame(Name = communities(clusters_girvan)[[i]], Group = i)
  
  community_girvan <- rbind(community_girvan, temp_df)
}
# community_girvan <- community_girvan %>%
#   mutate(pleiter = ifelse(Name %in% pleiter_name$author, "Projektleiter", "Kein Projektleiter"))
community_girvan$Name <- sapply(community_girvan$Name, add_numbering)
summary_girvan <- community_girvan %>%
  group_by(Group) %>%
  dplyr::summarise(n())


pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/Deskriptive Analyse/Coauthor-Network/girvan-biggest-component.pdf", width=16, height=16, family = "CMU Serif")
op1 <- par(mar = c(1, 0, 0, 0))
colors <- brewer.pal(min(12, max(membership(clusters_girvan_decompose))), "Paired")
set.seed(networkseed)
coords <- layout_with_graphopt(graph = graph_decompose, niter = 2000)
plot(graph_decompose,
     vertex.size = 5,
     vertex.color = colors[membership(clusters_girvan_decompose)],
     vertex.label = graph_decompose_id$author_id,
     vertex.label.family = "CMU Serif",
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = coords)

legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")
par(op1)
dev.off()

pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/Deskriptive Analyse/Coauthor-Network/biggest-component.pdf", width=16, height=16, family = "CMU Serif")
op1 <- par(mar = c(1, 0, 0, 0))
set.seed(networkseed)
coords <- layout_with_graphopt(graph = graph_decompose, niter = 2000)
plot(graph_decompose,
     vertex.size = 5,
     vertex.color = "gray90",
     vertex.label = graph_decompose_id$author_id,
     vertex.label.family = "CMU Serif",
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = coords)

legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")
par(op1)
dev.off()

