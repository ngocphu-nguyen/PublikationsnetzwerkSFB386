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