newdata$tone <- ifelse(newdata$mode==0, "minor", "major")

newdata$scale <-ifelse (newdata$key==0, "C",
                      ifelse(newdata$key==1, "C#",
                             ifelse(newdata$key==2, "D",
                                    ifelse(newdata$key==3,"D#" ,
                                           ifelse(newdata$key==4, "E",
                                                  ifelse(newdata$key==5, "E#",
                                                         ifelse(newdata$key==6,"F",
                                                                ifelse(newdata$key==7, "F#",
                                                                       ifelse(newdata$key==8, "G",
                                                                              ifelse(newdata$key==9,"G#",
                                                                                     ifelse(newdata$key==10,"A",
                                                                                            "A#"))))))))))) 


newdata$keys <- paste(newdata$scale, newdata$tone, sep= " ")

newdata$keysign <- ifelse (newdata$keys %in% c("C major","A minor" ), "Original",
                         ifelse(newdata$keys %in% c("G major","E minor","D# minor" ), "F sharp",
                                ifelse(newdata$keys %in% c("D major","B minor" ), "F,C Sharp",
                                       ifelse(newdata$keys %in% c("A major","F# minor" ), "F,C,G Sharp",    
                                              ifelse(newdata$keys %in% c("E major","C# minor" ), "F,A,G,D Sharp",
                                                     ifelse(newdata$keys %in% c("B major","G# minor" ), "F,C,G,D,A Sharp",
                                                            ifelse(newdata$keys %in% c("F# major","G# minor" ), "F,A,C,G,D,A,E Sharp",
                                                                   ifelse(newdata$keys %in% c("C# major","A# minor" ), "F,A,C,G,D,A,E,B Sharp",
                                                                          ifelse(newdata$keys %in% c("F major","D minor","E# major" ), "B Flat",
                                                                                 ifelse(newdata$keys %in% c("G minor", "A# major" ), "B,E Flat",
                                                                                        ifelse(newdata$keys %in% c("C minor","D# major"), "B,E, A Flat",
                                                                                               ifelse(newdata$keys %in% c("F minor","G# major","E# minor" ), "B,A,D,E Flat",
                                                                                                      "Unknown"))))))))))))


newdata$keylabel <- ifelse(newdata$keys== "C major",  "C major :Innocently Happy", 
                         ifelse (newdata$keys=="C minor", "C minor :Innocently Sad, Love-Sick",
                                 ifelse(newdata$keys=="C# minor" , "C sharp minor : Despair, Wailing, Weeping", 
                                        ifelse(newdata$keys=="C# major","C sharp major: Fullness, Sonorousness, Euphony",
                                               ifelse(newdata$keys=="D major", "D major: Triumphant, Victorious War-Cries",
                                                      ifelse(newdata$keys=="D minor", "D minor: Serious, Pious, Ruminating",
                                                             ifelse(newdata$keys=="D# minor", "D sharp minor: Deep Distress, Existential Angst",
                                                                    ifelse(newdata$keys=="D# major", "Cruel, Hard, Yet Full of Devotion",
                                                                           ifelse(newdata$keys=="E major", "E major: Quarrelsome, Boisterous, Incomplete Pleasure",
                                                                                  ifelse(newdata$keys=="E minor", "E minor: Effeminate, Amorous, Restless",
                                                                                         ifelse(newdata$keys %in% c("E# major" ,"F major"), "F major: Complaisance and calm",
                                                                                                ifelse(newdata$keys %in% c("F minor", "E# minor"),  "F minor: Obscure, Plaintive, Funereal",
                                                                                                       ifelse(newdata$keys=="F# major", "F sharp major : Conquering Difficulties, Sighs of Relief",
                                                                                                              ifelse(newdata$keys=="F# minor", "F sharp minor: Gloomy, Passionate Resentment",
                                                                                                                     ifelse(newdata$keys=="G major", "G major: Serious, Magnificent, Fantasy",
                                                                                                                            ifelse(newdata$keys=="G minor", "G minor: Discontent, Uneasiness",
                                                                                                                                   ifelse(newdata$keys=="G# major", "G sharp major : Death, Eternity, Judgement",
                                                                                                                                          ifelse(newdata$keys=="G# minor", "G sharp minor: Grumbling, Moaning, Wailing",
                                                                                                                                                 ifelse(newdata$keys=="A major", "A major : Joyful, Pastoral, Declaration of Love",
                                                                                                                                                        ifelse(newdata$keys=="A minor", "A minor : Tender, Plaintive, Pious",
                                                                                                                                                               ifelse (newdata$keys=="A# major", "A sharp major: Joyful, Quaint, Cheerful",
                                                                                                                                                                       ifelse(newdata$keys=="A# minor", "A sharp minor: Terrible, the Night, Mocking",
                                                                                                                                                                              "Unknown")  )))))))      )))       )))))))))))



# tempo classification
newdata$tempoc[newdata$tempo >= 66 & newdata$tempo <76] <- "Adagio" 
newdata$tempoc[newdata$tempo >=  76 & newdata$tempo <108] <- "Andante" 
newdata$tempoc[newdata$tempo >= 108 & newdata$tempo <120] <- "Moderato" 
newdata$tempoc[newdata$tempo >= 120 & newdata$tempo <156 ] <- "Allegro" 
newdata$tempoc[newdata$tempo >= 156 & newdata$tempo <176] <- "Vivace" 
newdata$tempoc[newdata$tempo >= 176 ] <- "Presto" 


newdata$tlabel[newdata$tempo >= 66 & newdata$tempo <76] <-" 66- 76"
newdata$tlabel[newdata$tempo >=  76 & newdata$tempo <108] <- "76-108" 
newdata$tlabel[newdata$tempo >= 108 & newdata$tempo <120] <- "108- 120" 
newdata$tlabel[newdata$tempo >= 120 & newdata$tempo <156 ] <- "120 -156" 
newdata$tlabel[newdata$tempo >= 156 & newdata$tempo <176] <- "156-176" 
newdata$tlabel[newdata$tempo >= 176 ] <- "> 176" 