library(glue)
library(dplyr)
library(readr)
library(ggthemes)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(ggpubr)


# Load the CSV data
male_poland <- read.csv("data/male_poland.csv", header = TRUE, sep = ",")
colnames(male_poland) <- c('Timestamp','Period','Value')
male_poland$Timestamp <- as.Date(male_poland$Timestamp)

female_poland <- read.csv("data/female_poland.csv", header = TRUE, sep = ",")
colnames(female_poland) <- c('Timestamp','Period','Value')
female_poland$Timestamp <- as.Date(female_poland$Timestamp)


# theme_set(theme_classic())

# Allow Default X Axis Labels

# p <- ggplot() +
#         geom_line(male_poland, mapping = aes(x=Timestamp, y=Value)) +
#         guides(fill=guide_legend(title=NULL)) +
#         scale_x_date(date_breaks = "years", date_labels = "%Y") +

#         geom_line(female_poland, mapping = aes(x=Timestamp, y=Value)) +
#         guides(fill=guide_legend(title=NULL)) +
#         scale_x_date(date_breaks = "years", date_labels = "%Y") +

#         theme_ipsum() +
#         theme(
#             legend.title = element_blank(),
#             legend.text = element_text(size=12, face="bold"),
#             plot.title = element_text(size=18),
#             axis.title.x = element_text(size=12, face="bold"),
#             axis.title.y = element_text(size=12, face="bold"),
#         )
# print(p)

# Zadanie 1 
# a)

# data_a <- read.csv("data/task_1_a.csv", header = TRUE, sep = ",")
# colnames(data_a) <- c('Timestamp','Period','Not_adjusted', 'Adjusted')
# data_a$Timestamp <- as.Date(data_a$Timestamp)


p <- ggplot() +
        geom_line(data_a, mapping = aes(x=Timestamp, y=Not_adjusted, color = 'Bez korekcji sezonowości')) +
        guides(fill=guide_legend(title=NULL)) +
        labs(y = "Procent", x = "Rok")+
        geom_line(data_a, mapping = aes(x=Timestamp, y=Adjusted,color = ' Z korekcją sezonowości')) +
        guides(fill=guide_legend(title=NULL)) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +

        # for the x axis label
        theme_ipsum() +
        theme(
            legend.title = element_blank(),
            legend.text = element_text(size=12, face="bold"),
            plot.title = element_text(size=18),
            axis.title.x = element_text(size=12, face="bold"),
            axis.title.y = element_text(size=12, face="bold"),
            aspect.ratio = 6.5/11,
        )
options(repr.p.width=4,repr.p.height=300)
print(p)
ggsave("Zadanie_1a.png", p, width=11, height=6.5, dpi=400)

#b)

# p <- ggplot() +
#         geom_histogram(data_a, mapping = aes(x=Not_adjusted, color = 'Bez korekcji sezonowości'), position="dodge",alpha=0.2) +


#         geom_histogram(data_a, mapping = aes(x=Adjusted, color = 'Z korekcją sezonowości'), position="dodge",alpha=0.2) +
#         labs(y = "Liczba wystąpień", x = "Procent")+
#         theme_ipsum() +
#         theme(
#             legend.title = element_blank(),
#             legend.text = element_text(size=12, face="bold"),
#             plot.title = element_text(size=18),
#             axis.title.x = element_text(size=12, face="bold"),
#             axis.title.y = element_text(size=12, face="bold"),
#             aspect.ratio = 6.5/11,
#         )
# options(repr.p.width=4,repr.p.height=300)
# ggsave("Zadanie_1b.png", p, width=11, height=6.5, dpi=400)
# print(p)

# Zadanie 2

countries <- read.csv("data/task_2_a.csv", header = TRUE, sep = ",")
colnames(countries) <- c('Timestamp','Period','Czechy','Niemcy','Dania','Hiszpania','Francja','Grecja','Węgry','Irlandia','Niderlandy','Polska')

countries$Timestamp <- as.Date(countries$Timestamp)
selected_countries <- countries[countries$Timestamp > "1998-04-01",]
selected_countries <- subset(selected_countries, select = -Period)

# podpunkt a)
library(reshape2)
# selected_countries <- melt(selected_countries, id.vars = "Timestamp")

# p <- ggplot(selected_countries, mapping = aes(x = Timestamp)) +
#     geom_line(selected_countries,mapping = aes(y = Czechy, color = 'Czechy')) +
#     geom_line(selected_countries,mapping = aes(y = Niemcy, color = 'Niemcy')) +
#     geom_line(selected_countries,mapping = aes(y = Polska, color = 'Polska')) +
#     geom_line(selected_countries,mapping = aes(y = Grecja, color = 'Grecja')) +
#     geom_line(selected_countries,mapping = aes(y = Irlandia, color = 'Irlandia')) +
#     geom_line(selected_countries,mapping = aes(y = Niderlandy, color = 'Niderlandy')) +
#     geom_line(selected_countries,mapping = aes(y = Węgry, color = 'Węgry')) +
#     geom_line(selected_countries,mapping = aes(y = Dania, color = 'Dania'))+
#     labs(y = "Procent", x = "Rok")+
#     theme_ipsum() +
#     scale_x_date(date_breaks = "2 years", date_labels = "%Y") +

#     theme(
#         legend.title = element_blank(),
#         legend.text = element_text(size=12, face="bold"),
#         plot.title = element_text(size=18),
#         axis.title.x = element_text(size=12, face="bold"),
#         axis.title.y = element_text(size=12, face="bold"),
#         aspect.ratio = 6.5/11,
#     )

# options(repr.p.width=4,repr.p.height=300)
# print(p)
# ggsave("Zadanie_2a.png", p, width=11, height=6.5, dpi=400)

# podpunkt b)
p <- ggplot() +
    geom_histogram(selected_countries,mapping = aes(x = Czechy, color = 'Czechy'),position="dodge",alpha=0.2) +
    geom_histogram(selected_countries,mapping = aes(x = Niemcy, color = 'Niemcy'),position="dodge",alpha=0.2) +
    geom_histogram(selected_countries,mapping = aes(x = Polska, color = 'Polska'),position="dodge",alpha=0.2) +
    # geom_histogram(selected_countries,mapping = aes(x = Grecja, color = 'Grecja'),position="dodge",alpha=0.2) +
    geom_histogram(selected_countries,mapping = aes(x = Irlandia, color = 'Irlandia'),position="dodge",alpha=0.2) +
    # geom_histogram(selected_countries,mapping = aes(x = Niderlandy, color = 'Niderlandy'),position="dodge",alpha=0.2) +
    geom_histogram(selected_countries,mapping = aes(x = Węgry, color = 'Węgry'),position="dodge",alpha=0.2) +
    # geom_histogram(selected_countries,mapping = aes(x = Dania, color = 'Dania'),position="dodge",alpha=0.2)+
    labs(y = "Liczba wystąpień", x = "Wartośc procentowa")+
    theme_ipsum() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size=12, face="bold"),
        plot.title = element_text(size=18),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        aspect.ratio = 6.5/11,
    )

options(repr.p.width=4,repr.p.height=300)
print(p)
ggsave("Zadanie_2b.png", p, width=11, height=6.5, dpi=400)
