# Dependencies
library(rvest)
library(XML)
library(ggplot2)
library(stringr)
library(reshape2)

# film build
filmURL <- html("https://en.wikipedia.org/wiki/List_of_Academy_Award-winning_films")
filmTable <- html_table(html_nodes(filmURL, "table") [[1]])
head(filmTable)
str(filmTable)

# film clean
filmTable <- data.frame(filmTable, colsplit(filmTable$Awards, pattern = "\\(", names = c("Awards", "special")))
filmTable$Awards <- NULL
filmTable$special <- NULL

filmTable <- data.frame(filmTable, colsplit(filmTable$Nominations, pattern = "\\[", names = c("Nominations", "footnote")))
filmTable$Nominations <- NULL
filmTable$footnote <- NULL

colnames(filmTable) <- c("Film", "Year", "Awards", "Nominations")
filmTable$Nominations <- as.numeric(filmTable$Nominations)

# film histogram plots
f <- ggplot(filmTable, aes(x = Awards)) +
    stat_bin(binwidth = 1, origin = -0.5) +
    stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -1, hjust = 2) +
    scale_x_continuous(breaks = unique(filmTable$Awards)) +
    xlab("Number of Awards per Film") + ylab("Frequency") +
    ggtitle("Total Awards for film")
f

fn <- ggplot(filmTable, aes(x = Nominations)) +
    stat_bin(binwidth = 1, origin = -0.5) +
    stat_bin(binwidth = 1, origin = -0.5, geom = "text", aes(label = ..count..), vjust = -1.5, hjust = 0.5) +
    scale_x_continuous(breaks = unique(filmTable$Nominations)) +
    xlab("Number of Nominations per film") + ylab("Frequency") +
    ggtitle("Total Nominations for a film")
fn

# film regression
summary(lm(filmTable$Awards ~ filmTable$Nominations))

# best picture build
bpURL <- html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture")
bpTable <- data.frame()
target <- data.frame()
for (i in 3:length(html_table(html_nodes(bpURL, "table")))) {
  target <- html_table(html_nodes(bpURL, "table") [[i]])
  bpTable <- rbind(bpTable, target)
}

# appears that the <h3> tags are somehow breaking the loop

target <- html_table(html_nodes(bpURL, "table") [[4]])
bpTable <- rbind(bpTable, target)






head(target)
head(bpTable)
bpTable11 <- html_table(html_nodes(bpURL, "table") [[11]])
bpTable12 <- html_table(html_nodes(bpURL, "table") [[12]])

head(bpTable11)
head(bpTable12)

rbind(bpTable11, bpTable12)









