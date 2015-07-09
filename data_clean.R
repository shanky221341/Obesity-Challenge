file<-c('http://www.hscic.gov.uk/catalogue/PUB16988/Obes-phys-acti-diet-eng-2015-tab.csv','http://www.hscic.gov.uk/catalogue/PUB13648/Obes-phys-acti-diet-eng-2014-tab_CSV.csv','http://www.hscic.gov.uk/catalogue/PUB10364/obes-phys-acti-diet-eng-2013-csv-tab.csv','http://www.hscic.gov.uk/catalogue/PUB05131/obes-phys-acti-diet-eng-2012-csv-tab.csv','http://www.hscic.gov.uk/catalogue/PUB00210/obes-phys-acti-diet-eng-2011-tab.xls','http://www.hscic.gov.uk/catalogue/PUB00206/obes-phys-acti-diet-eng-2010-tab.xls','http://www.hscic.gov.uk/catalogue/PUB00188/obes-phys-acti-diet-eng-2009-tab.xls','http://www.hscic.gov.uk/catalogue/PUB00175/obes-phys-acti-diet-eng-2008-tab.xls')
# Check the type of the file for csv or xls
# Set the screen wide to display all the columns
howWide=150
options(width=howWide)
temp2<-NULL
regions <-c("E92000001","E18000001","E18000002","E18000003","E18000004","E18000005","E18000006","E18000007","E18000008","E18000009","E18000010")
regions1 <-c("E92000001","E18000001","E18000002","E18000003","E18000004","E18000005","E18000006","E18000007","E18000008","E18000010")
parsed_final<-NULL
for (file in file) {

  temp2<-NULL
  #print(file)
  csv.check <- grepl(".csv", file)
    # cleaning for 2015 file
    if (csv.check == 1) {
        # for extracting the year from the file name
        splits <- strsplit(file, "-")
        splits <- splits[[1]]
        
        if (splits[6] == "2015") {
            # read the contents of the file
            data <- read.csv(file, stringsAsFactors = FALSE, header = FALSE)
            names(data)[1] <- "ons_code"
            # Using re expression to find only those rows with ons code and the relevant
            # columns
            data_mod <- subset(data, grepl("E[0-9]+", ons_code), select = c(1, 4, 
                5, 6, 7))
            # set the names of the new data
            names.new <- c("ons_code", "name", "total_admissions", "male_adm", "female_adm")
            colnames(data_mod) <- names.new
            # separate files for separate tables
            indicators <- c("primary_diag_by_region", "prim_diag_by_comm_area", "prim_diag_second_diag_by_region", "prim_diag_second_diag_by_comm_area", 
"prim_diag_with_bariat_by_region", "primary_diag_with_bariat_by_comm")
            
            # iterator for rows
            i = 1
            # iterator for file name
            k = 1
            # splitting the data using 'E92000001'
            temp <- data_mod[i, ]
            parsed<-NULL
            while (i != nrow(data_mod)) {
                i = i + 1
                if (data_mod[i, 1] != "E92000001") {
                  temp <- rbind(temp, data_mod[i, ])
                } else {
                  from_year <- rep(2013, nrow(temp))
                  from_month <- rep(0, nrow(temp))
                  to_year <- rep(0, nrow(temp))
                  to_month <- rep(0, nrow(temp))
                  indicator<-rep(indicators[k],nrow(temp))
                  ons_code <- temp[, 1]
                  temp <- cbind(ons_code, cbind(from_year, from_month, to_year, to_month,indicator), 
                    temp[, 2:ncol(temp)])
                  parsed<-rbind(parsed,temp)
                  k = k + 1
                  temp <- data_mod[i, ]
                }
            }
            #print(temp1)
            #print(ncol(parsed))
            
        } else {
            # for years other than 2015 in csv format
            data <- read.csv(file, stringsAsFactors = FALSE)
            data_mod <- subset(data, grepl("E[0-9]+", X))
            # Using re expression to find only those rows with ons code and the relevant columns
            data_mod <- data_mod[, 2:6]
            names.new <- c("ons_code", "name", "total_admissions", "male_adm", "female_adm")
            indicators <- c("primary_diag", "prim_diag_second_diag", "prim_diag_with_bariat")
            
            colnames(data_mod) <- names.new
            row_num <- nrow(data_mod)
            # In this the ons codes repeats for the three categories of the data which is
            # primary, primary-secondary and bariatric Hence the whole rows will be multiple
            # of three Initialize the start and the end of row
            r_start = 1
            r_end = row_num/3            
            # to find the file year
            splits <- strsplit(file, "-")
            splits <- splits[[1]]
            names(data)[1] <- "X"
            year <- subset(data, grepl("^[0-9]{4}/[0-9]{2}$", X), select = 1)
            year <- unique(year)
            year <- year[nrow(year), ]
            from_year <- rep(year, nrow(data_mod))
            from_month <- rep(0, nrow(data_mod))
            to_year <- rep(0, nrow(data_mod))
            to_month <- rep(0, nrow(data_mod))
            ons_code <- data_mod[, 1]
            indicator<-rep(indicators,each=nrow(data_mod)/3)
            parsed<- cbind(ons_code, cbind(from_year, from_month, to_year, to_month,indicator), data_mod[, 2:ncol(data_mod)])
            
            #print(temp2)
            #print(data_mod)
            # this will split the whole data into three files based on the row numbers
            #for (i in 1:3) {
             #   print(data_mod[r_start:r_end, ])
            #    r_start = r_end + 1
             #   r_end = r_end + nrow(data_mod)/3
            #}
        }
    } else {
        parsed<-NULL
        # for the xls format files
        require(xlsx)
        
        # set the new names of the columns
        names.new <- c("name", "total_admissions", "male_adm", "female_adm")
        indicators <- c("primary_diag", "prim_diag_second_diag", "prim_diag_with_bariat")
        # Initialize the sheet name
        sheet.names <- "null"
        
        # find the year of the file
        splits <- strsplit(file, "-")
        splits <- splits[[1]]
        
        # set the sheet names based on the year
        if (splits[6] == "2011" || splits[6] == "2009") {
            sheet.names <- c("7.8", "7.11", "7.13")
            ons_code <- regions
        } else if (splits[6] == "2010") {
            sheet.names <- c("7.7", "7.10", "7.12")
            year <- "2008/9"
            ons_code <- regions
        } else if (splits[6] == "2008") {
            sheet.names <- c("7.10", "7.13")
            year <- "2006/7"
            ons_code <- regions1
        }
        if (splits[6] == "2011") {
            year <- "2009/10"
        }
        if (splits[6] == "2009") {
            year <- "2007/8"
        }
        #for first downloading the xls file in temp directory and then reading from it
        file.temp <- paste(tempdir(), "temp.xls", sep = "")
        download.file(file, file.temp)

        for (i in 1:length(sheet.names)) {
            
            data <- read.xlsx(file.temp, sheetName = sheet.names[i], header = FALSE)
            names(data)[2] <- "SHA"
            # Using re expression to find only those rows with ons code and the relevant
            # columns
            data_mod <- subset(data, grepl("^[0-9]+", SHA), select = c(1:4))
            row.names(data_mod) <- NULL
            colnames(data_mod) <- names.new
            from_year <- rep(year, nrow(data_mod))
            from_month <- rep(0, nrow(data_mod))
            to_year <- rep(0, nrow(data_mod))
            to_month <- rep(0, nrow(data_mod))
            indicator <- rep(indicators[i], nrow(data_mod))
            data_mod <- cbind(ons_code, cbind(from_year, from_month, to_year, to_month,indicator), 
                data_mod[, 1:ncol(data_mod)])
            parsed<-rbind(parsed,data_mod)
            
        }
        #print(temporary)
    }
  #print(ncol(temp1))
#temp1<-rbind(temp1,data_mod)
#print(temp1)
#print(head(parsed))
parsed_final<-rbind(parsed_final,parsed)
 #print(names(parsed))
  }


parsed_final<-read.csv("parsed_final.csv")
parsed_final<-parsed_final[,2:ncol(parsed_final)]
parsed_final$from_year<-as.character(parsed_final$from_year)
for (i in 1:nrow(data))
{
x<-unlist(strsplit(parsed_final[i,2],"/"))
if(length(x)==2)
{
  if(x[1]=="2008"||x[1]=="2006"||x[1]=="2007")
    {
    parsed_final[i,4]<-paste("200",x[2],sep="")
  }
  else
    {
  parsed_final[i,4]<-paste("20",x[2],sep="")
  }
  
  parsed_final[i,2]<-x[1]
  }
}
parsed_final$from_year<-as.numeric(as.character(parsed_final$from_year))
parsed_final$to_year<-as.numeric(as.character(parsed_final$to_year))
parsed_final$from_month<-as.numeric(as.character(parsed_final$from_month))
parsed_final$to_month<-as.numeric(as.character(parsed_final$to_month))
parsed_final$total_admissions<-as.numeric(as.character(parsed_final$total_admissions))
parsed_final$male_adm<-as.numeric(as.character(parsed_final$male_adm))
parsed_final$female_adm<-as.numeric(as.character(parsed_final$female_adm))
parsed_final$name<-as.character(parsed_final$name)
parsed_final$indicator<-as.character(parsed_final$indicator)

print(nrow(parsed_final))
print(str(parsed_final))
print(head(parsed_final,1))
#write.csv(parsed_final,"parsed_final.csv") 