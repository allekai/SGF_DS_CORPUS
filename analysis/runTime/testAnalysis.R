# Test run of the stream generation and time logging
test.verbose <- F
# make a first test run
tic("Test static stream configuration generation")
test.static.conf <- streamgenerator::generate.stream.config(dim = 10,
                                                            mindim = 2,
                                                            maxdim = 4,
                                                            dependency = "Linear",
                                                            nstep = 1)
test.static.conf$subspaces <- list(1:3)
test.static.conf$ margins <- 0.9
toc(log = T)

tic("Test static stream generation")
test.static.stream <- streamgenerator::generate.static.stream(n = 10000,
                                                              prop = 0.05,
                                                              method = "Construction",
                                                              stream.config = test.static.conf,
                                                              verbose = test.verbose)
toc(log = T)


tic("Test dynamic stream configuration generation")
test.dynamic.conf <- streamgenerator::generate.stream.config(dim = 10,
                                                             mindim = 2,
                                                             maxdim = 4,
                                                             dependency = "Linear",
                                                             nstep = 3)
test.dynamic.conf$subspaceslist <- list(list(1:3, 8:9),
                                        list(1:3, 8:9),
                                        list(1:3, 8:9))
test.dynamic.conf$marginslist <- list(list(0.9, 0.9),
                                      list(0.5, 0.5),
                                      list(0.1, 0.1))
toc(log = T)

tic("Test dynamic stream generation")
test.dynamic.stream <- streamgenerator::generate.dynamic.stream(n = 5000,
                                                                prop = 0.05,
                                                                method = "Construction",
                                                                stream.config = test.dynamic.conf,
                                                                verbose = test.verbose)
toc(log = T)
test.log <- tic.log()
test.log.raw <- tic.log(format =FALSE)
tic.clearlog()