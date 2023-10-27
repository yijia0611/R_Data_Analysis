library(tidyverse)
library(rvest)


# hasznaltauto.hu



url <- "https://www.hasznaltauto.hu/talalatilista/PCOG2VGRR3RDADH4S56ACFGY3UFY652CQ52EEK45WSXZLINGQQTDCSWSNIA7D37HWSSTIHCPWUTHMPA6J4FHF5RBV5PE4MZJFIEFRESTWDAUMLAZTNFOCL3IKCP2CDWUIJZSSQFHRGNR4PZIK5QEBX2AAH7CFFS3HHUS62IY23QHFI3MT47ANYN3A4LVEYHN6L73HHYPQ2FNRFL3LHICCP4CGN24WUMKNH5X3F5CAQDXNJ2AFP7SEIML6OASN67CRR5W5UNBHZQKAF6HYO6LJATDQAR7JDBGKLWJSJBLWAXOQMLMR6SQUDHAPUPCN3GQ245PRWED3FSZHNWZY6Z3QP553M73IYIJP2J4DP7IDK2UHH3I6N4TQO5SL6VMBHSNY2GXPNIPMT7CQHZ6DUMQVCXMXLUI5X62VKX7LJU7WYR7AQYK4VL25K2RWZOCSHOQOUKUZTUAVMQFP7IVTRWKQQFNA6CFHAR22X7IQOS6Y4NN33MFBVFQWIOVMUAEPOSUVJISDFTP7LC63WMREFKGMVE2UD3VUZSTWOD4DHVBDRXAHLPBIRASYQJO4RKXKRTNK6V6JNELWMMGOXJ4RPY2ZC7HGGPHP4KCFRXRZTCYGO2CLOW2RUTUTYOBWNOMPDVBFJT6WGLYKCXTK6SMKCTM5CTJGQL2UISVZ25LMYWG25UCBQDYI6OEZXS7FL3GYNOG37ROU2FPX2JYKMUKDKVRBV7POPMGUSEKMIHV3MZ7VV5IEA42KPWJ3XGWX2R6RPSLIX3NZDGPLACPJJZHUIGVSOUQUNZIZP3XX4CH5L3DSTWV22LFSJI6HQ7GNZUM72HTHW4RFNAZ6G6HKIVTRV75DLRWNBDWI377ADB4ND5H"



page <- read_html(url)






my_node <- page %>% 
  html_nodes(".cim-kontener a")



name_of_the_car <- my_node %>% 
  html_text()



url_to_car <- my_node %>% 
  html_attr("href")




table_form_page<-url_to_car %>% 
  na.omit() %>% 
  first() %>% 
  read_html() %>% 
  html_table(fill = TRUE) %>% 
  keep(~ ncol(.) == 2)##所有有两列的数据集筛选出�?
#apply

cars
apply(cars,2,mean)
 x<-1:10
 x
 lapply(x, function(x) x^2)
 sapply(x, function(x) x^2)
 map(x,~ .^2)#same with tidy method
 map_dbl(x,~.^2)
 
 #map
 
 table_form_page %>% 
   map(~set_names(.,"x","y"))
 
 table_form_page %>% 
   map(~ set_names(.,"x","y")) %>% 
   bind_rows()#merge the tables
 
 get_data <- function(url_to_car) {
   url_to_car %>%
     read_html() %>%
     html_table(fill = TRUE) %>%
     keep(~ ncol(.) == 2) %>%
     map(~ set_names(., "x", "y")) %>%
     bind_rows()
 }
 
 get_data("https://www.hasznaltauto.hu/szemelyauto/suzuki/swift/suzuki_swift_1.2_dualjet_hybrid_gl_plusz-16320785")
 
 url_to_car %>% 
   na.omit() %>% 
   head() %>% 
   map(get_data)
 #另一个车的例�?
 "https://www.hasznaltauto.hu/talalatilista/PCOG2VG3R3RDADH5S56ADV5ZQTDVTIKBLKQZCVU227FFAU2CSMMCK2JVQD4PO4O2KINJNJ22Y53HY4T4KKING53Z6FZDZENCQSAAKOIFNOWMLAVRWEKP5DAG6UE2UQANGSJQE5C2XBTPYTMKBXKSUYAIFUIHW2YMVDAWKRWZE4QPY5HALQFKYXHWP72WHIXPMAJHX4DHBM5GIB3QUZVKQSJRN27K6UQU4DAG4FLI4WPRITHCWVAJG7MS4OMRW5FI64MOQSJ2JZLC4Z2GX2DJYOMIYVDI5EQMFXQRBYAADXLZCFB3U3H3LK445YZHIWFKJUH543WKK5Y6QKY5PSOM5PGHUODMNWZDRUH665TX2VMJYTZS7AHV3LNW5AJ5LPXWE5D7MW7F3CYZT4HANXSQHGN7ZKDS6R4GQIRLXSW36RF5H5N5LJ3TVNAE32SVA7XJ3IPGLC5F2IAV33YFAOXIITDVI5YLDII4WXXBCLQJ5JMPSA7KPAOGW5HTDPLCYOUEKSLR4HHIKKVGQN65CWYTIO5WNAEKKGNUE3UK3F6ZKRWGVMBHFD5NGQDLHAJHWF7ELRPFJO6SM3UUVDW3PAJOGSI3J5RTY3PDLGGGO3J4C6FSWB2T2YGG3ANOOLBZXJDQ55TO4Y6HKCM3CKIN3SUVOGR5EYUFGZ6FGSNDHFMRFJLV2GRR4PZTIEDAXARS4JXPZ4S3XOI44IXXC5NUKM74SQQZIWGWLCHL64MWYNBEJVYYNLWYT5LD3QABZUY44TSONNPFQ5G7XESHXKUHHELAD7PRQFUE3FXW4SUBRYVP5XHYEX7SCBSXXF22THSKV4OJ6M2LIJ7U3GHFYAO2BR4F4W4R3HDH62C3DG2AHMWXL5SKNFJ7/page2"
 
 
 url
 
many_urls <- c("", str_c("/page", 2:10 ))
 many_urls
 
#合并url �? html两个表格
 cars_df <- tibble(url = str_c(url, many_urls)) %>%
   sample_n(2) %>%
   mutate(
     page = map(url, read_html),
     url_to_car = map(page, html_nodes, ".cim-kontener a"),
     car = map(url_to_car, html_text),
     url_to_car = map(url_to_car, html_attr, "href")
) %>% 
   select(car, url_to_car) %>% 
   unnest(cols = c(car, url_to_car)) %>% #把p那些没有数据的演示出�?
   filter(!is.na(url_to_car))  #把没有数据的全部去掉
 
 cars_df %>% 
   sample_n(2) %>% #列出其中2�?
   mutate(
     data = map(url_to_car, get_data)
   ) %>% 
   pull(data) %>% 
   first() %>% 

cars_df %>% 
   sample_n(2) %>% #列出其中2�?
   mutate(
     data = map(url_to_car, get_data)
   ) %>% 
   unnest(data) %>%  #展开数据
   select(-url_to_car)%>%
   pivot_wider(names_from = x, values_from = y) %>% 
   janitor::clean_names()#去除匈牙利语的音标以及一些别的多余标�?
 
 safely_read_html <- possibly(read_html, NA, quiet = FALSE)
 
 
 sleepy_read_html <- function(url)
   page <- safely_read_html(url)
 
 for(i in 1:3){
   if (is.na(page)){
     Sys.sleep(2)
     meesage("i have to be patient")
     page <-safely_read_html(url)
   }
 }   
 
 safely_read_html <- possibly(read_html, NA, quiet = FALSE)
 
 
 
 sleepy_read_html <- function(url) {
   page <- safely_read_html(url)
   
   for (i in 1:3) {
     if (is.na(page)) {
       Sys.sleep(2)
       message("I have to be patient! :)")
       page <- safely_read_html(url)
     }
   }
   
   page
 }
 
 
 
 get_data <- function(url_to_car) {
   page <- url_to_car %>% 
     sleepy_read_html()
   
   if (!is.na(page)) {
     
     page %>% 
       html_table(fill = TRUE) %>% 
       keep(~ ncol(.) == 2) %>% 
       map(~ set_names(., "x", "y")) %>% 
       bind_rows()
   } else {
     tibble(x = as.character(NA), y = as.character(NA))
   }
 }
 
 
 
 cars_data_df <- cars_df %>% 
   mutate(
     data = map(url_to_car, get_data)
   ) %>% 
   unnest(data) %>% 
   pivot_wider(names_from ="x", values_from = "y") %>% 
   janitor::clean_names()
 
 
   