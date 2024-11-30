# Conway's Game of Life With Fortran
A project for my CCS 4102 - Programming Languages Class

# DEPENDENCIES
## Msys64
```
pacman -S mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-fpm \
 mingw-w64-ucrt-x86_64-raylib mingw-w64-ucrt-x86_64-gcc \
 mingw-w64-ucrt-x86_64-gcc-libgfortran mingw-w64-ucrt-x86_64-glfw
```
Modify ```fpm.toml``` of fortran-raylib
```
[build]
link = [ "raylib", "glfw3", "glu32", "pthread", "m" ]
```
Change only glfw and gl
# BUILD
```
fpm build --flag "-fno-range-check"
```
# RUN
```
fpm run --flag "-fno-range-check"
```