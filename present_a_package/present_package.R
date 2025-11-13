library(ParallelStructure)
library(Rmpi)

data <- data("structure_data")


system('mkdir structure_results')

data(structure_data)

data(structure_jobs)

write(t(structure_jobs),ncol=length(structure_jobs[1,]),file='joblist1.txt')
write(t(structure_data),ncol=length(structure_data[1,]),file='example_data.txt')


parallel_structure(structure_path=NULL,joblist='joblist1.txt',n_cpu=4,
              infile='example_data.txt',outpath='structure_results/',
              numinds=987,numloci=9,printqhat=1)
