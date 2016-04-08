library(ggplot2)
theme_set(theme_bw(14))
library(RSQLite)
library(dplyr)

DATAPATH <- "~/tmp/"

#### Functions ####
getStats <- function(db) {
    con <- dbConnect(SQLite(), file.path(DATAPATH, db))
    query <- dbSendQuery(conn = con, statement = "SELECT 
                     (SELECT COUNT(DISTINCT(uri)) FROM songs) AS total,
                     COUNT(*) AS count, artist, title 
                     FROM songs GROUP BY artist, title")
    res <- dbFetch(query)
    dbClearResult(query)
    dbDisconnect(con)
    return(res)
}

lowerCI <- function(included, total) {
    tmp <- prop.test(included, total)
    return(tmp$conf.int[1])
}

upperCI <- function(included, total) {
    tmp <- prop.test(included, total)
    return(tmp$conf.int[2])
}

#### Get data & compute results ####
cat("Reading data...\n")
# Get stats
sad <- getStats("sad.db")
happy <- getStats("happy.db")
control <- getStats("control.db")

# Merge control into sad and happy and remove "uncontrolled" samples
cat("Merging control with sad...\n")
sad <- left_join(sad, control, by = c("artist", "title"))
sad <- sad[complete.cases(sad), ]

cat("Merging control with happy...\n")
happy <- left_join(happy, control, by = c("artist", "title"))
happy <- happy[complete.cases(happy), ]

# Compute lower and upper CIs: lower for sad and happy and upper for control
cat("CI (lower) for sad music...\n")
sad$lowerCI <- apply(sad[, c("count.x", "total.x")], 1, 
                     function(x) lowerCI(x[1], x[2])) 

cat("CI (upper) for sad music...\n")
sad$upperCI <- apply(sad[, c("count.y", "total.y")], 1, 
                     function(x) upperCI(x[1], x[2])) 

cat("CI (lower) for happy music...\n")
happy$lowerCI <- apply(happy[, c("count.x", "total.x")], 1, 
                       function(x) lowerCI(x[1], x[2])) 

cat("CI (upper) for happy music...\n")
happy$upperCI <- apply(happy[, c("count.y", "total.y")], 1, 
                       function(x) upperCI(x[1], x[2]))

# Check if this makes any difference (it doesn't, could have avoided the 
# cost of computing the upper & lower bounds).
sad$true.x <- sad$count.x / sad$total.x
sad$true.y <- sad$count.y / sad$total.y
happy$true.x <- happy$count.x / happy$total.x
happy$true.y <- happy$count.y / happy$total.y

# Compute the rankings
sad$mood <- sad$lowerCI / sad$upperCI
sad <- sad[order(-sad$mood), ]
happy$mood <- happy$lowerCI / happy$upperCI
happy <- happy[order(-happy$mood), ]
# Damn Jesse & Joy
happy <- happy[!grepl("Joy", happy$artist), ]

# Popularity control
pop_threshold <- 10 # or 200, to get the same results I did
sad <- sad[sad$count.x > pop_threshold & sad$count.y > pop_threshold, ]
happy <- happy[happy$count.x > pop_threshold & happy$count.y > pop_threshold, ]

# Reorder things a bit
saddest <- head(sad, 25)
saddest$orig_mood <- saddest$mood
saddest$mood <- saddest$mood / max(saddest$mood)
happiest <- head(happy, 25)
happiest$orig_mood <- happiest$mood
happiest <- happiest[order(happiest$mood), ]
happiest$mood <- happiest$mood / max(happiest$mood)
saddest$label <- "sad"
saddest$mood <- -saddest$mood
happiest$label <- "happy"

res <- rbind(saddest, happiest)
res$artitle <- paste(res$artist, res$title, sep = " - ")
res$artitle <- reorder(res$artitle, res$mood, FUN = mean)
res$art_score <- c(-25:-1, 1:25) + 25
res$artitle <- reorder(res$artitle, res$art_score, FUN = mean)

#### Attempts at plotting ####
# Shamelessly copied from http://kyrcha.info/2012/07/08/tutorials-fitting-a-sigmoid-function-in-r/
sigmoid <- function(params, x) {
    params[1] / (1 + exp(-params[2] * (x - params[3])))
}

res$art_score <- sigmoid(c(1, 0.1, 25), res$art_score) - 0.5
res$index <- 1:nrow(res)

plt1 <- ggplot(res) + geom_bar(aes(x = artitle, y = mood, fill = art_score), 
                               stat = "identity") +
        ylab("Mood") + 
        xlab("") +
        scale_y_continuous(breaks = c(-0.25, 0.25), labels = c("Sad", "Happy")) +
        scale_fill_gradient2(low = "red", high = "blue", mid = "gray") + 
        coord_flip() +
        theme(legend.position = "none")
print(plt1)

plt2 <- ggplot(res) + geom_label(aes(x = art_score, y = index, label = artitle,
                                    fill = mood), color = "white",
                                hjust = 0, fontface = "bold", size = 5) +
        scale_fill_gradient2("Mood", 
                             breaks = c(-1, 1), 
                             labels = c("Sad", "Happy"),
                             low = "blue", high = "red", mid = "gray") +
        scale_x_continuous(limits = c(-0.5, 2)) +
        theme(legend.position = c(0.2, 0.8),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              panel.border = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank()
              )
print(plt2)
