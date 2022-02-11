\name{ratrank}
\alias{ratrank}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Rating-ranking data a three-way contingency table
}
\description{
This three-way contingency table represents a known data set described in van Herk and  van de Velden (2007). The three-way contingency table consists of nine rating
values against nine ranking values given by the same
participants across five European countries (France, Italy, Germany, UK and Spain).}
\usage{data(ratrank)}
\format{
  The format is:
% num [1:13, 1:9] 81 66 150 667 67 24 47 430 743 132 ...\cr
row names [1:9] "1", "2", "3", "4", "5", "6","7","8","9" 
col names [1:9] "rank1", "rank2", "rank3", "rank4", "rank5", "rank6",
                       "rank7", "rank8", "rank9"
tube names [1:5] "F", "I","G","U","S"
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
van Herk H and van de Velden M (2007) Insight into the relative merits of rating and ranking
in a cross-national context using three-way correspondence analysis. Food Quality and Preference, 18, 1096--1105.\cr
}
\examples{
ratrank<-structure(c(766L, 128L, 38L, 10L, 12L, 3L, 2L, 5L, 9L, 619L, 
234L, 67L, 16L, 15L, 5L, 2L, 8L, 7L, 512L, 277L, 109L, 22L, 23L, 
5L, 11L, 7L, 7L, 385L, 291L, 152L, 64L, 41L, 9L, 12L, 7L, 12L, 
297L, 251L, 192L, 82L, 96L, 17L, 12L, 6L, 20L, 187L, 203L, 259L, 
105L, 119L, 44L, 19L, 8L, 29L, 143L, 144L, 209L, 140L, 170L, 
54L, 51L, 22L, 40L, 77L, 100L, 152L, 148L, 215L, 73L, 62L, 56L, 
90L, 47L, 45L, 84L, 119L, 200L, 82L, 98L, 67L, 231L, 859L, 101L, 
53L, 18L, 18L, 9L, 7L, 2L, 16L, 733L, 205L, 53L, 23L, 21L, 13L, 
11L, 6L, 18L, 622L, 224L, 124L, 41L, 27L, 8L, 12L, 6L, 19L, 547L, 
248L, 102L, 78L, 45L, 19L, 11L, 11L, 22L, 466L, 243L, 139L, 76L, 
76L, 25L, 21L, 9L, 28L, 357L, 239L, 168L, 105L, 95L, 61L, 20L, 
14L, 24L, 293L, 192L, 165L, 128L, 133L, 42L, 58L, 28L, 44L, 215L, 
162L, 161L, 127L, 148L, 60L, 65L, 54L, 91L, 140L, 121L, 142L, 
128L, 157L, 69L, 76L, 75L, 175L, 1219L, 193L, 29L, 13L, 3L, 4L, 
2L, 6L, 3L, 651L, 504L, 111L, 30L, 19L, 8L, 5L, 8L, 6L, 476L, 
335L, 230L, 35L, 13L, 8L, 5L, 4L, 6L, 346L, 324L, 201L, 136L, 
30L, 5L, 5L, 6L, 5L, 239L, 299L, 234L, 101L, 170L, 22L, 14L, 
4L, 6L, 166L, 246L, 265L, 116L, 96L, 71L, 27L, 11L, 16L, 124L, 
179L, 215L, 163L, 139L, 52L, 80L, 20L, 32L, 80L, 114L, 172L, 
148L, 168L, 80L, 84L, 96L, 70L, 63L, 48L, 101L, 115L, 183L, 92L, 
123L, 131L, 292L, 916L, 99L, 40L, 12L, 7L, 3L, 3L, 13L, 42L, 
578L, 224L, 65L, 11L, 15L, 6L, 3L, 18L, 30L, 486L, 207L, 140L, 
34L, 14L, 10L, 7L, 16L, 29L, 405L, 207L, 149L, 64L, 30L, 7L, 
14L, 21L, 19L, 304L, 256L, 157L, 60L, 83L, 9L, 17L, 20L, 21L, 
239L, 222L, 195L, 95L, 55L, 34L, 20L, 18L, 18L, 204L, 169L, 213L, 
113L, 89L, 23L, 45L, 16L, 15L, 165L, 148L, 184L, 128L, 121L, 
46L, 38L, 51L, 23L, 89L, 94L, 147L, 141L, 181L, 70L, 57L, 32L, 
82L, 1086L, 89L, 37L, 10L, 12L, 6L, 9L, 6L, 24L, 501L, 251L, 
55L, 11L, 14L, 7L, 7L, 7L, 11L, 415L, 139L, 188L, 22L, 14L, 8L, 
4L, 4L, 12L, 359L, 148L, 111L, 101L, 21L, 7L, 7L, 3L, 15L, 278L, 
158L, 128L, 49L, 127L, 9L, 12L, 6L, 13L, 240L, 162L, 130L, 48L, 
58L, 49L, 11L, 5L, 12L, 185L, 113L, 148L, 78L, 84L, 26L, 52L, 
7L, 16L, 128L, 91L, 119L, 110L, 118L, 37L, 38L, 35L, 28L, 83L, 
50L, 67L, 89L, 165L, 47L, 66L, 46L, 120L), .Dim = c(9L, 9L, 5L
), .Dimnames = list(c("1", "2", "3", "4", "5","6", "7",
                      "8", "9"), c("rank1", "rank2", "rank3", 
                        "rank4", "rank5", "rank6",
                       "rank7", "rank8", "rank9"),
                      c("F", "I", "G", "U", "S")))
dim(ratrank)
data(ratrank)
}
\keyword{datasets}
