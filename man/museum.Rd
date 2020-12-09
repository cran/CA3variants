\name{museum}
\alias{museum}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Raw data: Three variables from a  survey
}
\description{
This raw data table represents a possible data set selected from a large survey on customer satisfacyion during museum visiting.
The rows are individuals. The first column concerns the number of visits, the second column concerns if they like it, and the third column their satisfaction.}
\usage{data(museum)}
\format{
  The format is:
 num [1:223, 1:3] "often" "much" "excellent" ...\cr
%rows [1:223] "1", "2", "3", "4", "5", "6"
%cols [1:3] "nvisit", "like", "satisfaction"
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Beh EJ and Lombardo R 2014 Correspondence Analysis: Theory, Practice and New Strategies. John Wiley & Sons. \cr
}
\examples{
museum<-structure(list(nvis = structure(c(2L, 2L, 4L, 4L, 1L, 3L, 3L, 
2L, 4L, 1L, 3L, 3L, 4L, 2L, 4L, 3L, 4L, 2L, 2L, 3L, 4L, 4L, 2L, 
4L, 3L, 4L, 2L, 2L, 4L, 1L, 2L, 2L, 4L, 1L, 4L, 2L, 2L, 2L, 4L, 
1L, 1L, 1L, 1L, 2L, 2L, 3L, 2L, 3L, 4L, 4L, 1L, 3L, 2L, 2L, 3L, 
3L, 3L, 2L, 4L, 3L, 2L, 4L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 
3L, 3L, 3L, 2L, 2L, 4L, 4L, 4L, 4L, 3L, 2L, 3L, 3L, 3L, 4L, 2L, 
2L, 2L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 2L, 3L, 4L, 4L, 1L, 3L, 
3L, 2L, 4L, 3L, 2L, 4L, 3L, 2L, 4L, 2L, 3L, 3L, 2L, 2L, 2L, 3L, 
2L, 3L, 2L, 2L, 3L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 3L, 2L, 3L, 3L, 
3L, 4L, 4L, 1L, 3L, 3L, 2L, 1L, 1L, 1L, 1L, 3L, 4L, 2L, 4L, 3L, 
4L, 2L, 2L, 3L, 4L, 2L, 3L, 3L, 3L, 4L, 2L, 2L, 2L, 4L, 1L, 3L, 
1L, 1L, 2L, 2L, 3L, 2L, 3L, 3L, 3L, 1L, 3L, 2L, 2L, 2L, 1L, 1L, 
2L, 2L, 2L, 1L, 3L, 2L, 3L, 4L, 4L, 1L, 3L, 2L, 2L, 2L, 3L, 2L, 
3L, 4L, 4L, 1L, 3L, 3L, 3L, 2L, 1L, 4L, 1L, 3L, 4L, 3L, 4L, 2L, 
4L, 3L, 4L, 2L, 2L, 3L, 3L, 4L), .Label = c("no", "often", "some", 
"voften"), class = "factor"), like = structure(c(2L, 2L, 2L, 
2L, 2L, 3L, 3L, 2L, 2L, 2L, 3L, 3L, 2L, 3L, 1L, 3L, 2L, 3L, 3L, 
1L, 3L, 2L, 3L, 2L, 3L, 2L, 2L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 
2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 2L, 3L, 3L, 2L, 3L, 2L, 
3L, 2L, 2L, 3L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 3L, 3L, 3L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 3L, 3L, 2L, 3L, 3L, 2L, 
3L, 2L, 3L, 3L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 1L, 2L, 2L, 3L, 3L, 
2L, 3L, 1L, 2L, 2L, 3L, 3L, 1L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 3L, 2L, 2L, 3L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 1L, 2L, 2L, 3L, 
3L, 3L, 2L, 3L, 1L, 3L, 2L, 3L, 3L, 1L, 3L, 3L), .Label = c("little", 
"much", "some"), class = "factor"), satisfaction = structure(c(1L, 
2L, 2L, 1L, 1L, 2L, 2L, 1L, 3L, 1L, 3L, 1L, 1L, 2L, 2L, 1L, 2L, 
2L, 2L, 2L, 2L, 1L, 4L, 2L, 2L, 3L, 1L, 2L, 1L, 1L, 3L, 3L, 1L, 
1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 4L, 3L, 1L, 1L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 3L, 1L, 2L, 3L, 2L, 
3L, 2L, 3L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 4L, 3L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 
3L, 2L, 3L, 2L, 3L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 3L, 3L, 1L, 
3L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 2L, 2L, 1L, 4L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 
1L, 1L, 1L, 2L, 1L, 1L, 4L, 3L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 
1L, 3L, 4L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 3L, 4L), .Label = c("excellent", 
"good", "suff", "unsuff"), class = "factor")), class = "data.frame", row.names = c("1", 
"2", "3", "5", "6", "8", "9", "10", "12", "13", "14", "16", "17", 
"18", "19", "20", "21", "22", "23", "24", "25", "27", "30", "31", 
"32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
"43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "54", 
"55", "56", "57", "58", "59", "60", "61", "64", "65", "66", "67", 
"68", "69", "70", "71", "72", "73", "74", "75", "78", "80", "81", 
"82", "84", "85", "86", "87", "88", "89", "90", "91", "92", "95", 
"96", "97", "98", "99", "100", "101", "102", "104", "105", "106", 
"107", "108", "109", "110", "111", "112", "113", "115", "116", 
"117", "118", "119", "120", "121", "122", "123", "124", "125", 
"126", "127", "128", "129", "130", "131", "132", "133", "136", 
"138", "139", "140", "142", "143", "144", "145", "146", "147", 
"148", "149", "150", "151", "153", "154", "155", "156", "157", 
"158", "159", "160", "162", "163", "165", "166", "167", "168", 
"169", "170", "171", "173", "174", "175", "176", "177", "178", 
"179", "180", "181", "182", "183", "184", "185", "186", "187", 
"189", "190", "191", "192", "193", "194", "195", "196", "197", 
"198", "200", "201", "202", "203", "204", "205", "206", "207", 
"208", "209", "210", "211", "212", "213", "214", "215", "217", 
"218", "219", "220", "221", "222", "223", "224", "225", "227", 
"228", "229", "230", "231", "232", "233", "234", "235", "236", 
"237", "238", "239", "240", "241", "242", "243", "244", "245", 
"246", "247", "248", "249", "250", "251", "252", "253"))
dim(museum)
data(museum)
}
\keyword{datasets}
