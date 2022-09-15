
followREGIMEN = data.table(time=1:10,do.call("rbind",lapply(1:10,function(a){
    avars=paste0("glp1_",1:a)
    compl=y[[avars[[1]]]]
    compl2=1-y[[avars[[1]]]]
    if (a>1)
        for (u in 2:a) {
            compl=compl*y[[avars[[u]]]]
            compl2=compl2*(1-y[[avars[[u]]]])
        }
    c(GLP1=sum(compl,na.rm=TRUE),
      NON_GLP1=sum(compl2,na.rm=TRUE))
})))
fwrite(followREGIMEN,file="z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/export/followREGIMEN_dementia.txt")
