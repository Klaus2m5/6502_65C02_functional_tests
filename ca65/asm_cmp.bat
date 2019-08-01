ca65 -l %1.lst %1.ca65
ld65 %1.o -o %1.bin -m %1.map -C example.cfg
fc /B %1.bin ..\bin_files\%1.bin

