#Install dan panggil package
#install.packages("rvest")
library("rvest")

#CASE IMDB

#Menentukan alamat web
url <- "https://www.imdb.com/search/title/?release_date=2021-01-01,2021-11-09&start=1&ref_=adv_nxt"
webpage <- read_html(url) #membaca url

#Scraping peubah Rank
#Tentukan nodes peubah pada HTML
rank_data_html <- html_nodes(webpage,'.text-primary')
#Ambil teks yang ada pada nodes yang sudah ditentukan
rank_data <- html_text(rank_data_html)
#Karena data yang terambil adalah teks yang bertipe karakter, akan kita ubah ke numerik
rank_data <- as.numeric(rank_data)

#Sraping peubah Title
#Tentukan nodes peubah pada HTML
title_data_html <-html_nodes(webpage,'.lister-item-header a')
#Ambil teks yang ada pada nodes yang sudah ditentukan
title_data <- html_text(title_data_html)

#Scraping peubah Description
#Tentukan nodes peubah pada HTML
description_data_html <- html_nodes(webpage,'.ratings-bar+.text-muted')
#Ambil teks yang ada pada nodes yang sudah ditentukan
description_data <- html_text(description_data_html)
#Menghilangkan karakter yang tidak diperlukan hasil scraping
description_data <- gsub("\n",'',description_data)

#Scraping peubah runtime
#Tentukan nodes peubah pada HTML
runtime_data_html <- html_node(html_nodes(webpage,'.lister-item-content'),'.text-muted .runtime')
#Ambil teks yang ada pada nodes yang sudah ditentukan
runtime_data <- html_text(runtime_data_html)
#Menghilangkan karakter yang tidak diperlukan hasil scraping
runtime_data <-gsub(" min","",runtime_data)
#Karena data yang terambil adalah teks yang bertipe karakter, akan kita ubah ke numerik
runtime_data <- as.numeric(runtime_data)

#Scraping peubah Genre
#Tentukan nodes peubah pada HTML
genre_data_html <- html_nodes(webpage,'.text-muted .genre')
#Ambil teks yang ada pada nodes yang sudah ditentukan
genre_data <- html_text(genre_data_html)
#Menghilangkan karakter yang tidak diperlukan hasil scraping
genre_data <- gsub('\n','',genre_data)
genre_data <- gsub(' ','',genre_data)
genre_data <- gsub(',',', ',genre_data)

#Scraping peubah Rating
#Tentukan nodes peubah pada HTML
rating_data_html <- html_node(html_nodes(webpage,'.lister-item-content'),'.ratings-imdb-rating strong')
#Ambil teks yang ada pada nodes yang sudah ditentukan
rating_data <- html_text(rating_data_html)

#Scraping peubah Director & Actor
#Tentukan nodes peubah pada HTML
director_actor_data_html <- html_nodes(webpage,'.text-muted+ p')
#Ambil teks yang ada pada nodes yang sudah ditentukan
director_actor_data <- html_text(director_actor_data_html)
#Menghilangkan karakter yang tidak diperlukan hasil scraping
director_actor_data <- gsub('\n    ','',director_actor_data)
director_actor_data <- gsub('\n','',director_actor_data)
director_actor_data <- gsub('             [|] ',';',director_actor_data)

#Ambil data Director
director_data <- gsub('Stars:.*','',director_actor_data)
director_data <- gsub('.*Directors:','',director_data)
director_data <- gsub('.*Director:','',director_data)
director_data <- gsub(';','',director_data)

#Ambil data Actor
actor_data <- gsub('.*Stars:','',director_actor_data)

#Scraping peubah Metascore
#Tentukan nodes peubah pada HTML
metascore_data_html <- html_node(html_nodes(webpage,'.lister-item-content .ratings-bar'),'.ratings-metascore span')
#Ambil teks yang ada pada nodes yang sudah ditentukan
metascore_data <- html_text(metascore_data_html)
#Menghilangkan karakter yang tidak diperlukan hasil scraping
metascore_data <- gsub(' ','',metascore_data)
#Karena data yang terambil adalah teks yang bertipe karakter, akan kita ubah ke numerik
metascore_data <- as.numeric(metascore_data)

#Scraping peubah Votes
#Tentukan nodes peubah pada HTML
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
#Ambil teks yang ada pada nodes yang sudah ditentukan
votes_data <- html_text(votes_data_html)
#Karena data yang terambil adalah teks yang bertipe karakter, akan kita ubah ke numerik
votes_data <- gsub(',','',votes_data)
votes_data <- as.numeric(votes_data)

#Merge variabel
df_movie <- data.frame(Rank = rank_data, Title = title_data, Description = description_data,
                       Runtime = runtime_data, Genre = genre_data, Rating = rating_data,
                       Director = director_data, Actor = actor_data, Metascore = metascore_data,
                       Votes = votes_data)

#Untuk melihat dataframe, jalankan syntax di bawah tanpa tanda #
View(df_movie)

#PAGINATION
#Terdapat lebih dari satu halaman yang perlu dilakukan scraping.
#Solusi: Dengan fungsi rekursif

#1 - Dibuat list kosong masing-masing peubah
Rank = NULL
Title = NULL
Description = NULL
Runtime = NULL
Genre = NULL
Rating = NULL
Director = NULL
Actor = NULL
Metascore = NULL
Votes = NULL

#2 - Fungsi rekursif
for (i in seq(1,200,50)){ #angka 200 dapat diganti sesuai jumlah halaman yang diinginkan
  base_url <- "https://www.imdb.com/search/title/?release_date=2021-01-01,2021-11-09&start="
  url <- paste(base_url,toString(i),sep = '')
  webpage <- read_html(url)
  
  #Dilakukan scraping sama dengan sebelumnya, namun setiap hasilnya ditambahkan ke list
  #---Rank---
  rank_data_html <- html_nodes(webpage,'.text-primary')
  rank_data <- html_text(rank_data_html)
  #Kategorik -> Numerik
  rank_data <- as.numeric(rank_data)
  
  Rank <- append(Rank,rank_data)
  
  #---Title---
  title_data_html <-html_nodes(webpage,'.lister-item-header a')
  title_data <- html_text(title_data_html)
  
  Title <- append(Title, title_data)
  
  #---Descrpition---
  description_data_html <- html_nodes(webpage,'.ratings-bar+.text-muted')
  description_data <- html_text(description_data_html)
  #Menghilangkan karakter yang tidak diperlukan hasil scraping
  description_data <- gsub("\n",'',description_data)
  
  Description <- append(Description,description_data)
  
  #---Runtime---
  runtime_data_html <- html_node(html_nodes(webpage,'.lister-item-content'),'.text-muted .runtime')
  runtime_data <- html_text(runtime_data_html)
  #Remove "min"
  runtime_data <-gsub(" min","",runtime_data)
  #Kategorik -> Numerik
  runtime_data <- as.numeric(runtime_data)
  
  Runtime <- append(Runtime,runtime_data)
  
  #---Genre---
  genre_data_html <- html_nodes(webpage,'.text-muted .genre')
  genre_data <- html_text(genre_data_html)
  #Data Cleaning
  genre_data <- gsub('\n','',genre_data)
  genre_data <- gsub(' ','',genre_data)
  genre_data <- gsub(',',', ',genre_data)
  
  Genre <- append(Genre, genre_data)
  
  #---Rating---
  rating_data_html <- html_node(html_nodes(webpage,'.lister-item-content'),'.ratings-imdb-rating strong')
  rating_data <- html_text(rating_data_html)
  #Kategorik -> Numerik
  rating_data <- as.numeric(rating_data)
  
  Rating <- append(Rating, rating_data)
  
  #---Director & Actor---
  director_actor_data_html <- html_nodes(webpage,'.text-muted+ p')
  director_actor_data <- html_text(director_actor_data_html)
  #Cleaning
  director_actor_data <- gsub('\n    ','',director_actor_data)
  director_actor_data <- gsub('\n','',director_actor_data)
  director_actor_data <- gsub('             [|] ',';',director_actor_data)
  
  #Director
  director_data <- gsub('Stars:.*','',director_actor_data)
  director_data <- gsub('.*Directors:','',director_data)
  director_data <- gsub('.*Director:','',director_data)
  director_data <- gsub(';','',director_data)
  
  Director <- append(Director,director_data)
  
  #Actor
  actor_data <- gsub('.*Stars:','',director_actor_data)
  
  Actor <- append(Actor, actor_data)
  
  #---Metascore---
  metascore_data_html <- html_node(html_nodes(webpage,'.lister-item-content .ratings-bar'),'.ratings-metascore span')
  metascore_data <- html_text(metascore_data_html)
  #Menghilangkan karakter
  metascore_data <- gsub(' ','',metascore_data)
  #karakter -> numerik
  metascore_data <- as.numeric(metascore_data)
  
  Metascore <- append(Metascore, metascore_data)
  
  #---Votes---
  votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
  votes_data <- html_text(votes_data_html)
  #karakter -> numerik
  votes_data <- gsub(',','',votes_data)
  votes_data <- as.numeric(votes_data)
  
  Votes <- append(Votes, votes_data)
}

#Gabungkan dalam 1 dataframe
df_movie2 <- data.frame(Rank = Rank, Title = Title, Description = Description, Runtime = Runtime,
                       Genre = Genre, Rating = Rating, Director = Director, Actor = Actor,
                       Metascore = Metascore, Votes = Votes)
View(df_movie2)
