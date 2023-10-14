# SAXPY using Fortran Standard Parallelism

See https://developer.nvidia.com/blog/accelerating-fortran-do-concurrent-with-gpus-and-the-nvidia-hpc-sdk

## Compile for GPUs

	FC=nvfortran cmake -B build/stdpar-gpu -DENABLE_STDPAR_GPU=ON

## Compile for multi-core CPUs

	FC=nvfortran cmake -B build/stdpar-gpu -DENABLE_STDPAR_MULTICORE=ON
