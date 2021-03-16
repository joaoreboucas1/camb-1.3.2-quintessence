These are test files for my CAMB modification for quintessence. The modified files are in /camb/fortran/ModifiedFiles.

To use, do the following:
1. Rename quintessencejoao3.f90 to DarkEnergyQuintessence.f90
2. Replace camb/fortran/DarkEnergyQuintessence.f90 with the modified file above. You might want to keep the original file, so rename the
original to DarkEnergyQuintessence_original.f90
3. Rename camb_modified.f90 to camb.f90
4. Replace camb/fortran/camb.f90 with the modified file. Again, you might want to keep the original file.
5. make clean + make to compile everything
6. Run camb with params_quintessence.ini (you might want to place this file in the inifiles folder and
 run ./camb ../inifiles/params_quintessence.ini from the fortran folder)


######################

To run with quintessence, you set "dark_energy_model = earlyquintessence" in the inifile.
To set the potential functional form, use "field_potential_type = <number>", where <number> will correspond to some type
To set the potential parameters, use "potentialparam1 = <value>", "<potentialparam2 = <value>", etc. Careful with the definitions!