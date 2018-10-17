# Setup
library(devtools)
library(logging)

basicConfig(level = 'FINEST')
addHandler(writeToFile, file="~/generation.log", level='DEBUG')


path="C:\\Users\\allek\\Desktop\\BA\\github\\streamgenerator\\R-streamgenerator"

loginfo(paste(c("Trying to install the streamgenerator package from path: ", path)))
install_local(path, force=TRUE)
loginfo("Successfully installed the streamgenerator package.")
