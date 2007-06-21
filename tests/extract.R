
library(arules)
data(Adult)

all.equal(Adult, c(Adult[1:100], Adult[-(1:100)]))

ec <- eclat(Adult)
all.equal(ec, c(ec[1:100], ec[-(1:100)]))


i <- items(ec)
all.equal(i, c(i[1:100,], i[-(1:100),]))

###

