library(ggplot2)


first_injuries <- head(injuries$EVTYPE, 3)
first_fatalities <- head(fatalities$EVTYPE, 3)


as.year <- function(x) {
    strftime(x, "%Y")
}

first_injuries_by_year <- storm %>% 
    mutate(YEAR=as.year(BGN_DATE)) %>%
    group_by(YEAR, EVTYPE) %>%
    filter(EVTYPE %in% first_injuries) %>%
    summarise(total=sum(INJURIES))

first_fatalities_by_year <- storm %>% 
    mutate(YEAR=as.year(BGN_DATE)) %>%
    group_by(YEAR, EVTYPE) %>%
    filter(EVTYPE %in% first_fatalities) %>%
    summarise(total=sum(FATALITIES))


g1 <- ggplot(first_injuries_by_year, aes(YEAR, total))
g1 + geom_bar(aes(fill=EVTYPE), 
              position="identity", stat="identity", 
              alpha=0.5) + theme_bw() + theme(axis.text.x = element_text(angle=90))



g2 <- ggplot(first_fatalities_by_year, aes(YEAR, total, group=EVTYPE))
g2 + geom_line(aes(color=EVTYPE)) + 
    theme(axis.text.x = element_text(angle=90)) +
    labs(title="Number of fatalities per year from 1950 to 2011")
