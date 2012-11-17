#
# All new fltplt Makefile needing gmake
#
NEED_VERSION := 3.80 3.81 3.82
$(if $(filter $(MAKE_VERSION),$(NEED_VERSION)),,        \
 $(error This makefile requires one of GNU make version $(NEED_VERSION).))

SRCS = get_grid.f95 grid_coords.f95 inp_par.f95 interpolate.f95 main3d.f95 \
    mod_floats.f95 mod_grid.f95 mod_kinds.f95 mod_netcdf.f95 mod_param.f95 \
    mod_scalars.f95 nf_fread2d.f95 read_fltpar.f95 set_depth.f95 \
    set_scoord.f95 strings.f95

OBJS = $(subst .f95,.o,$(SRCS))

FC = gfortan
#FC = pgf95
FFLAGS = -O0 -g -C -I $(shell $(NC_CONFIG) --prefix)/include
NC_CONFIG = nc-config
#NC_CONFIG = /usr/local/pkg/netcdf/netcdf-4.1.pgi/bin/nc-config
LIBS = $(shell $(NC_CONFIG) --flibs)
MDEPFLAGS = --cpp --fext=f95 --file=-

%.o: %.f95
	$(FC) -c $(FFLAGS) $<

ll_to_ij: $(OBJS)
	$(FC) -o ll_to_ij $(LDFLAGS) $(FFLAGS) $(OBJS) $(LIBS)

.PHONY: clean
.PHONY: depend

clean:
	rm -rf *.o *.mod

depend:
	sfmakedepend $(MDEPFLAGS) $(SRCS) > MakeDepend

ifneq "$(MAKECMDGOALS)" "clean"
  -include MakeDepend
endif
