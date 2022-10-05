res1 <- rio::import("./targets/dementia/export/table_ltmle_main_results_dementia_2022-08-11_ic.txt")
res2 <- rio::import("./targets/dementia/export/table_sensitivity_results_dementia_2022-08-11_ic_.txt")

data=res1

make_graph(data,param){

ggplot(data,aes(x=time,y=`dementia risk`,color=treatment))+
  geom_point()+geom_line()+theme(legend.position = "bottom")+
  ggtitle("Risk of xxx")


}
