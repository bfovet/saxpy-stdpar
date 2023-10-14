cmake_minimum_required(VERSION 3.22)

project(saxpy LANGUAGES Fortran)

option(ENABLE_STDPAR_MULTICORE "Enable Nvidia stdpar multicore" ON)
option(ENABLE_STDPAR_GPU "Enable Nvidia stdpar GPU" OFF)

add_executable(saxpy)
target_sources(saxpy PRIVATE saxpy.f90)

set(STDPAR_FLAGS "-stdpar -Minfo=accel -Minfo=stdpar -fast -Mllvm-fast -Ktrap=none")

if (ENABLE_STDPAR_GPU)
  # Enable offloading support (via the non-standard `-stdpar`) for the new NVHPC SDK.
  # The values are Nvidia architectures in ccXY format will be passed in via `-gpu=` (e.g `cc70`)
  # Possible values are:
  #   cc35  - Compile for compute capability 3.5
  #   cc50  - Compile for compute capability 5.0
  #   cc60  - Compile for compute capability 6.0
  #   cc62  - Compile for compute capability 6.2
  #   cc70  - Compile for compute capability 7.0
  #   cc72  - Compile for compute capability 7.2
  #   cc75  - Compile for compute capability 7.5
  #   cc80  - Compile for compute capability 8.0
  #   ccall - Compile for all supported compute capabilities"
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${STDPAR_FLAGS} -Wall -gpu=cc80")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native ${STDPAR_FLAGS} -Wall -gpu=cc80")
elseif (ENABLE_STDPAR_MULTICORE)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${STDPAR_FLAGS} -Wall -stdpar=multicore")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native ${STDPAR_FLAGS} -Wall -stdpar=multicore")
else()
  if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g3 -fbacktrace")
    set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native")
  endif()  
endif()

target_compile_options(saxpy PRIVATE -stdpar)
target_link_options(saxpy PRIVATE -stdpar)
