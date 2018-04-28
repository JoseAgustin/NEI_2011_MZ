#
#   Generador de programa convert.exe
#
FCLIB = -L$(NETCDF)/lib -I$(NETCDF)/include -lnetcdff -axAVX
FC = ifort
objetcs = module_var_nei.o lee_NEI.o lee_wrfinput.o guarda.o convierte.o
DEPS = var_nei.mod
# Makefile

convert.exe: $(objetcs)
	$(FC) -o $@   $(FCLIB) $^

%.mod:%.90 %.o
	$(FC) -c -o $@ $(FCLIB) $^

%.o:%.f90 $(DEPS)
	$(FC) -c -o $@ $(FCLIB) $^

clean:
	rm convert.exe  $(objetcs) $(DEPS)

