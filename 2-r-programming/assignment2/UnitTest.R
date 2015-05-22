##
## Use package.install("RUnit") before running this file
## Notice the "/" used although on Windows OS
##

rUnitLibrary <- "C:/Users/laiyw/AppData/Local/Temp/RtmpWyu0sa/downloaded_packages"
sourceFile <- "D:/development/ProgrammingAssignment2/cachematrix.R"

library("RUnit", lib.loc = rUnitLibrary)
source(sourceFile)

test.suite <- defineTestSuite(
  "UnitTestImpl", 
  dir = file.path("D:/development/datasciencecoursera/2-r-programming/assignment2/test"),
  testFileRegexp = '^\\w+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
