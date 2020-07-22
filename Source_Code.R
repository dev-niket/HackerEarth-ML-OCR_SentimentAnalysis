#Written by Niket Ganatra for ML Challenge on HackerEarth (July, 2020)
#HackerEarth: www.hackerearth.com/@niket82
#GitHub: github.com/dev-niket

# NOTE: Please insert appropriate file path in place of '...' wherever a file path is mentioned

library(tesseract)
eng <- tesseract("eng")
library(magick)
library(magrittr)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(textdata)

df <- data.frame(Filename=character(), 
                 Category=character())


images <- list.files("C:/Users/.../Dataset")
for(i in images)
{
  imageName <- glue("C:/Users/.../Dataset/", i, sep = "")
  text <- image_read(imageName) %>%
    image_resize("2000") %>%
    image_convert(colorspace = 'gray') %>%
    image_trim() %>%
    image_ocr()
  if(text != "")
  {
    fileCont<-file("C:/Users/.../temp.txt")
    writeLines(text, fileCont)
    close(fileCont)
    
    fileName <- "C:/Users/.../temp.txt"
    fileName <- trimws(fileName)
    # read in the new file
    fileText <- glue(read_file(fileName), .open = "{{")
    # remove any dollar signs (they're special characters in R)
    fileText <- gsub("\\$", "", fileText) 
    
    
    tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
    
    tdf = tokens %>%
            inner_join(get_sentiments("afinn")) %>% 
            summarise(sentiment = sum(value)) %>% 
            mutate(method = "AFINN")
    
    s = tdf[1,1][[1]] #Extracting the sentiment value from the tibble
    
    if(s>0)
    {
      category = "Positive"
    }
    else if(s<0)
    {
      category = "Negative"
    }
    else
    {
      category = "Random"
    }
    
  }
  else
  {
    category = "Random"
  }
  
  df[nrow(df) + 1,] = c(i,category)
  
}

write.csv(df,"C:/Users/..../submission.csv",row.names = FALSE)