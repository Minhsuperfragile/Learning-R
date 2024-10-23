epa.df = readRDS("rintro-chapter14-epa.rds")
summary(epa.df)

head(sort((table(epa.df$page)), decreasing=TRUE),10)
head(sort(table(epa.df$page[epa.df$pagetype=="html"]),
          decreasing = T),10 )

library(ggplot2)
p = ggplot(epa.df, aes(x=datetime)) + geom_density()
p
library(scales)
p = ggplot(epa.df, aes(x=datetime, fill=I("lightblue"))) + 
  geom_density(alpha=0.5, bw="sj-dpi", adjust = 2.0) +
  scale_x_datetime(breaks = date_breaks("2 hours"),
                   date_labels = "%b %d %H:%M") +
  theme(axis.text = 
          element_text(angle=45, vjust=1, hjust=1)) + 
  ylab("HTTP Request (proportion)") +
  xlab("Date / Time")
p

err.page = epa.df$page[epa.df$status >= "http400"]
head(sort(table(err.page), decreasing = T))

length(unique(epa.df$host))
host.tab = sort(table(epa.df$host), decreasing = T)
plot(host.tab)
head(host.tab, 10)
#plot(log(host.tab))

epa.ordered = epa.df[order(epa.df$host, epa.df$datetime),]
head(epa.ordered)

epa.ordered$timediff = c(NA, as.numeric(
  epa.ordered$datetime[2:nrow(epa.ordered)] - epa.ordered$datetime[1:nrow(epa.ordered)-1],
  units = "mins"
))

session.time = 15
epa.ordered$newsession = NA
epa.ordered$newsession[1] = T
epa.ordered$newsession[2:nrow(epa.ordered)] = 
  ifelse(
        epa.ordered$host[2:nrow(epa.ordered)] != epa.ordered$host[1:nrow(epa.ordered)-1],
        TRUE, epa.ordered$timediff[2:nrow(epa.ordered)] >= session.time
        )
epa.ordered[1:10, c("host", "datetime", "newsession")]
epa.ordered$session = cumsum(epa.ordered$newsession)
epa.ordered$timediff[epa.ordered$newssion == 1] = NA
epa.ordered[1:100, c(1, 7, 11:13)]

#how many req per session
nrow(epa.ordered) / sum(epa.ordered$newsession)

session.length = rle(epa.ordered$session)$length
table(session.length)
plot(table(session.length))
(sesslen = rep(session.length, session.length))

set.seed(2710)
sessamp = sample(unique(epa.ordered$session[sesslen==7]), 10)
epa.ordered[epa.ordered$session %in% sessamp, c("session", "page")]

session.length.html = rle(epa.ordered$session[epa.ordered$pagetype == "html"])$length
plot(table(session.length.html))

# Markov chain 
p.start = c(0.7, 0.2, 0.1) # probabilities of starting on 1 in 3 web pages
p.trans = matrix(c(0.1,0.5,0.2,
                   0.6,0.4,0.8,
                   0.3,0.1,0.0), nrow = 3, byrow = TRUE)

#one click (step)
p.trans %*% p.start

library(expm)
p.trans %^% 100
p.trans %^% 100 %*% c(1,0,0) # show that no mater where you start, after 100 steps, you will end up the same
p.trans %^% 100 %*% c(0.333, 0.334, 0.333)

#formatting EPA data for click stream analysis
library(clickstream)
top.page = names(head(sort(table(epa.df$page[epa.df$pagetype == "html"]), 
                           decreasing = TRUE), 20))
epa.html = subset(epa.ordered, pagetype=='html' & page %in% top.page)
epa.session = split(epa.html, epa.html$session)
#remove session with length = 1
epa.stream.len = lapply(epa.session, nrow)
epa.session = epa.session[epa.stream.len > 1]

length(epa.session)

epa.stream = unlist(lapply(epa.session, 
                           function(x) {
                             paste0(unique(x$host), ",",
                                    paste0(unlist(x$page), collapse = ","),
                                    ",END")
                           }))
head(epa.stream)
any(grepl("ii", epa.stream))
epa.stream = gsub("/", "ii", epa.stream)
epa.stream = gsub(".html", "", epa.stream, fixed = TRUE)
head(epa.stream)

#put data into click stream object
click.tempfile = tempfile()
writeLines(epa.stream, click.tempfile)
epa.trans = readClickstreams(click.tempfile, header = TRUE)

#estimating the markov chain 
epa.mc = fitMarkovChain(epa.trans, order = 1)
View(epa.mc@transitions[1][["1"]])

epa.mc.mat = t( epa.mc@transitions [[1]])
dimnames(epa.mc.mat)[[1]] = gsub("ii", "/", dimnames(epa.mc.mat)[[1]])
dimnames(epa.mc.mat)[[2]] = gsub("ii", "/", dimnames(epa.mc.mat)[[2]])

#visualize 
library(superheat)
set.seed(2710)
superheat(epa.mc.mat[-1,],
          bottom.label.size = 0.4,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          heat.col.scheme = "red",
          n.clusters.rows = 5,
          n.clusters.cols = 5,
          left.label = "variable",
          bottom.label = "variable",
          title = "Transitions, in sequences of top 20 pages (Row-to-Col)")

set.seed(2710)
plot(epa.mc, minProbability=0.25)

# fit a model with order = 2
epa.trans.ge3 = epa.trans[lapply(epa.trans, length) >= 4]
epa.mc2 = fitMarkovChain(epa.trans.ge3, order=2)
# predict next page with mc2
epa.trans[160]
epa.ex = new("Pattern", sequence= head(unlist(epa.trans[160]), -1))
predict(epa.mc2, epa.ex, dist=1) # predict 1 next page
predict(epa.mc2, epa.ex, dist=4) # predict 4 next pages
