library(rvest)

url_1 <- "http://www.bioinfo-scrounger.com/archives/555"
url_2 <- "https://www.jianshu.com/p/f9573a4a8aeb"

get_text <- function(url, node) {
  read_html(url) %>%
    html_node(node) %>%
	html_text %>%
	gsub("<[^>]>|\t|\n", "", .)
}

text_1 <- get_text(url_1, "article") %>% strsplit("") %>% `[[`(1)
text_2 <- get_text(url_2, "div.post") %>% strsplit("") %>% `[[`(1)

block <- 10
m <- sapply(seq(1, length(text_1), by = block),
             function(i) {
               sapply(seq(1, length(text_2), by = block),
                      function(j) {
                        sum(text_1[i : (i + block - 1)] %in%
                            text_2[j : (j + block - 1)]) >= block - 1
                      })
             })

png("dot-plot.png", 800, 800)
image(m)
dev.off()
