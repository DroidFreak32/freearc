@echo off
C:\Base\Compiler\Dev-Cpp\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O3 -funroll-loops -march=x86-64 -mtune=generic -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr mmdet.cpp -ommdet.exe


