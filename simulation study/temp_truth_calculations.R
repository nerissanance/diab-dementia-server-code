

table(d.always$event_dementia_1)
table(d.never$event_dementia_1)

table(d.always$event_dementia_1)[2]/table(d.never$event_dementia_1)[2]
table(d.always$event_dementia_2)[2]/table(d.never$event_dementia_2)[2]
table(d.always$event_dementia_3)[2]/table(d.never$event_dementia_3)[2]
table(d.always$event_dementia_4)[2]/table(d.never$event_dementia_4)[2]
table(d.always$event_dementia_5)[2]/table(d.never$event_dementia_5)[2]

exp(-0.8)



#need to account for cumulative survival
cc <- as.data.frame(cc)
exp(cc[cc$var=="event_dementia_1",colnames(cc)=="glp1_1"])


colSums(cc[cc$var=="event_dementia_1",-c(1:2)], na.rm = FALSE)
Y_a1_t1 <- exp(sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T))
Y_a0_t1 <- exp(sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")])
Y_a1_t1/Y_a0_t1

prop.table(table(d.always$event_dementia_1))
