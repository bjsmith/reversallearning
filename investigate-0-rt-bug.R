View(rl.subjects.f1.list[,sum(reaction_time==0),by=.(subid,correct)]%>% tidyr::spread(correct,V1))

View(rl.subjects.f1.list[subid==120])

#it's simply that we set a proper response to 