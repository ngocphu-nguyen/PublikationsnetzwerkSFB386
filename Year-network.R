### first run coauthors-network.R!

networkseed <- 2006
### big graph 2013 with rownames(vertices), i.e. rownumber as id
set.seed(networkseed)
#coords <- layout_(graph, with_graphopt(charge=0.01)) 
#coords <- layout_components(graph, layout = layout_with_fr)
coords <- layout_with_fr(graph, niter = 2000)
pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/network2006.pdf", width=20, height=20)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = "gray90",
     vertex.label = rownames(vertices),
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = coords)
#     layout = layout.fruchterman.reingold)
legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")
par(op1)
dev.off()

#namestest <- (as.character(authors_years$author) == as.character(vertices[,1]))

### Graphs over time:
years <- levels(coauthors_pairs$year)
years <- sapply(years, grep,
                colnames(coauthors_years), value = TRUE)
#years <- years[-length(years)] # to exclude latest graph

pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/networkevolution.pdf", width=16, height=8)
#op <- par(mfrow = c(1, length(years)))
set.seed(networkseed)
op <- par(mfrow = c(2, 6)) 
for ( i in years ) {
  ewidth <- coauthors_years[[i]]
  ewidth[is.na(ewidth) | ewidth <= 0] <- 1
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  
  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(networkseed)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = coords)
  #layout = layout.fruchterman.reingold)
  
  mtext(i, side = 1, line = 0)
  par(op1)
}
par(op)
dev.off()


### Every year
for ( i in years ) {
  ewidth <- coauthors_years[[i]]
  ewidth[is.na(ewidth) | ewidth <= 0] <- 1
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "gray90", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "gray90", NA)
  lcolor <- ifelse(authors_years[[i]] > 0, "black", "#00000000")
  
  set.seed(networkseed)
  coords <- layout_with_fr(graph, niter = 2000)
  file_name <- paste0("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/network", i, ".pdf")
  pdf(file=file_name, width=20, height=20)  
  op1 <- par(mar = c(1, 0, 0, 0))
  plot(graph,
       vertex.size = 3.5,
       vertex.color = vcolor,
       vertex.label.color = lcolor,
       vertex.label = rownames(authors_years),
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = coords)
  legend("topleft",
         legend = sort(unique(edgelist$width)),
         lwd = sort(unique(edgelist$width)),
         col = "SkyBlue2",
         bty = "n")

  mtext(i, side = 1, line = 0)
  par(op1)
  par(op)
  dev.off()
}
