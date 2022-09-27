get_labs <- function(path_corona_update,
                     pnr,
                     nobs,
                     isas=importSAS_custom,
                     use_cached=FALSE){
    
    if (use_cached){
        lab <- readRDS(file="Z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/cached_data/lab.rds")
        return(lab)
    }else{
        # Hb1AC - creatinine - cholesterol
        # Kbh amt - cholesterol difficult, old data
        hbc1 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_kbhamt.sas7bdat',
                          where="testcode=:'HBA1C' or testcode=:'CREA' or batterycode=:'CHOL' or batterycode=:'LDL' or batterycode=:'HDL'",
                          keep=c("pnr","batterycode","testcode","testresult","collectdate","result_normal_range"),
                          character.vars = c("pnr","testresult","result_normal_range"), 
                          filter = pnr, obs=nobs)
        hbc1[,testresult:=as.numeric(gsub('>','',testresult))]
        hbc1 <- hbc1[!is.na(testresult)]
        hbc1[,start:=as.Date(collectdate)]
        hbc1[grepl('^CREA',testcode),class:='CREA']
        hbc1[grepl('^CHOL',testcode),class:='T_chol']
        hbc1[grepl('^LDL',testcode) & !grepl('LDLED',testcode),class:='LDL_chol']
        hbc1[grepl('^HDL',testcode) ,class:='HDL_chol']
        hbc1[grepl('^HBA1C',testcode),class:='HBA1C']
        hbc1[class=='HBA1C' & testresult<1,testresult:=testresult*100]
        hbc1[class=='HBA1C',testresult:=testresult*10.93-23.5]
        hbc1 <- hbc1[!is.na(class)]
        hbc1 <- hbc1[,.(pnr,class,testresult,start)]
        setnames(hbc1,"testresult","res")
        
        # KPLL - strange numbers for hba1c - diabetic patients with hba1c with peak at 4
        hbc2 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_kpll.sas7bdat',
                          where = "col3='CR' or col3 in ('CHO','LDL','HDL')",
                          filter = pnr, obs=nobs)
        hbc2[,res:=as.numeric(gsub(',','.',col5))]
        hbc2[,start:=as.Date(col2,format="%d-%m-%Y")]
        hbc2[col3=='CR',class:='CREA']
        hbc2[col3=='CHO',class:='T_chol']
        hbc2[col3=='LDL',class:='LDL_chol']
        hbc2[col3=='HDL',class:='HDL_chol']
        hbc2 <- hbc2[,.(pnr,class,res,start)]
        
        #Region Nord
        hbc3 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord0607.sas7bdat',filter = pnr, obs=nobs)
        hbc4 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord0809.sas7bdat',filter = pnr, obs=nobs)
        hbc5 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord1213.sas7bdat',filter = pnr, obs=nobs)
        hbc345 <- rbind(hbc3,hbc4,hbc5)
        nord <- importSAS('X:/Data/Rawdata_Hurtig/706582/analyser_labkaii.sas7bdat',
                          character.vars = "analyse_id") #note doesn't have a pnr to filter on
        hbc345 <- merge(hbc345,nord,by="analyse_id")
        hbc345[,resultat:=gsub('>','',resultat)]
        hbc345[,res:=as.numeric(gsub(",",".",resultat))]
        hbc345[,start:=as.Date(prv_datoklok)]
        hbc345[analysenavn=='P-Creatinin',class:='CREA']
        hbc345[grepl('DCCT',component)|grepl('IFCC',component),class:='HBA1C']
        hbc345[grepl('DCCT',component) & resultat<1,resultat:=res*100]
        hbc345[grepl('DCCT',component),resultat:=res*10.93-23.5]
        hbc345[npucode=='NPU01566',class:='T_chol']
        hbc345[npucode=='NPU01568',class:='LDL_chol']
        hbc345[npucode=='NPU01567',class:='HDL_chol']
        hbc345 <- hbc345[,.(pnr,class,res,start)]
        hbc345 <- hbc345[!is.na(class)]
        
        hbc6 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_nordfinal.sas7bdat',
                          where="component='Hemoglobin A1c' or component='Creatinin' or npucode=:'NPU0156'",
                          filter = pnr, obs=nobs)
        hbc6[,res:=as.numeric(result)]
        hbc6[,start:=as.Date(sampledate,format="%d.%m.%Y")]
        hbc6[component=='Creatinin',class:='CREA']
        hbc6[component=='Hemoglobin A1c',class:='HBA1C']
        hbc6[class=='HBA1C' & res<1,res:=res*100]
        hbc6[class=='HBA1C',res:=res*10.93-23.5]
        hbc6[npucode=='NPU01566',class:='T_chol']
        hbc6[npucode=='NPU01568',class:='LDL_chol']
        hbc6[npucode=='NPU01567',class:='HDL_chol']
        hbc6 <- hbc6[,.(pnr,class,res,start)]
        
        #Roskilde
        hbc7 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_roskilde.sas7bdat',
                          where="analyse in('HBA1C','CREA','CHOL','LDL','HDL')",
                          filter = pnr, obs=nobs)
        hbc7[,resultat:=as.numeric(gsub('=','',resultat))]
        hbc7[,start:=as.Date(dato,format="%d%B%y")]
        
        hbc7[analyse=='CREA',class:='CREA']
        hbc7[analyse=='HBA1C',class:='HBA1C']
        hbc7[analyse=='HBA1C' & resultat<1,resultat:=resultat*100]
        hbc7[analyse=='HBA1C',resultat:=resultat*10.93-23.5]
        hbc7[analyse=='CHOL',class:='T_chol']
        hbc7[analyse=='LDL',class:='LDL_chol']
        hbc7[analyse=='HDL',class:='HDL_chol']
        hbc7 <- hbc7[,.(pnr,class,resultat,start)]
        setnames(hbc7,"resultat","res")
        
        #Labka
        hbc8 <- importSAS('X:/Data/Rawdata_Hurtig/706582/lab_dm_forskerny.sas7bdat',
                          where="analysiscode  in ('NPU27300','NPU03835','NPU02307','NPU18016','NPU18105','NPU01807',
          'NPU01566','NPU01567','NPU01568')",
          filter = pnr, obs=nobs)

        hbc9 <- importSAS(paste0(path_corona_update,'/lab_forsker.sas7bdat'),
                          where="analysiscode  in ('NPU27300','NPU03835','NPU02307','NPU18016','NPU18105','NPU01807',
          'NPU01566','NPU01567','NPU01568')",
          character.vars="patient_cpr",
          #filter = pnr,#no pnr to filter on
          obs=nobs)
        setnames(hbc9,"patient_cpr","pnr") 
        hbc9 <- merge(hbc9,pnr,by="pnr")
        hbc89 <- rbind(hbc8,hbc9)
        hbc89[,res:=as.numeric(value)]
        setnames(hbc89,"samplingdate","start")
        
        hbc89[analysiscode %in% c('NPU18016','NPU18105','NPU01807'),class:='CREA']
        hbc89[analysiscode %in% c('NPU27300','NPU03835','NPU02307'),class:='HBA1C']
        hbc89[analysiscode=='NPU03835' & res<1,res:=res*100]
        hbc89[analysiscode=='NPU03835', res:=res*10.93-23.5]
        hbc89[class=='HBA1C' & res<20, res:=NA]
        hbc89[analysiscode=='NPU01566',class:='T_chol']
        hbc89[analysiscode=='NPU01568',class:='LDL_chol']
        hbc89[analysiscode=='NPU01567',class:='HDL_chol']
        hbc89 <- hbc89[,.(pnr,class,res,start)]
        
        lab <- rbind(hbc1,hbc2,hbc345,hbc6,hbc7,hbc89)
        lab <- lab[!is.na(res)]
        lab <- unique(lab)
        
        return(lab)
    }
}
