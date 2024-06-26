### Papers per author.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)
library(reshape2)


authors_npapers <-
  ddply(papers_authors, .(author, year),
        function(x) {
          data.frame(year = x$year[1],
                     author = x$author[1],
                     npapers = nrow(x))
        })

authors_npapers$year <- ordered(authors_npapers$year)

authors_npapers_overall <-
  ddply(authors_npapers, .(author), numcolwise(sum))

t6 <- table(authors_npapers_overall$npapers)
t6





ggplot(melt(t6, varnames = c("npapers")),
       aes(ordered(npapers), value)) + geom_bar(stat = "identity") +
  labs(x = "Papers in SFB 386", y = "Authors") +
  labs(title = "Paper contributions by author")


## Who are the authors with a high number of papers?
subset(authors_npapers_overall, npapers > 6)



### Maximum number of papers per author per year: ####################

ddply(authors_npapers, .(year), numcolwise(max))


## Who?
ddply(authors_npapers, .(year),
      function(x) {
        subset(x, npapers == max(x$npapers))
      })



### Mean papers per author: ##########################################

ddply(authors_npapers, .(year), numcolwise(mean))
ddply(authors_npapers, .(year), numcolwise(median))
ddply(authors_npapers, .(year), numcolwise(sd))

