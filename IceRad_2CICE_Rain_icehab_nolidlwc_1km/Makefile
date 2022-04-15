HDFEOSHOME = /data/keeling/a/yulanh
HDFHOME=/sw/hdf4-4.2.7-gnu-6.1.0
MPIHOME=/sw/openmpi-3.0.0-gnu-6.1.0

FFLAGS = -g -I$(HDFEOSHOME)/hdfeos/include -I$(HDFHOME)/include -I/$(MPIHOME)/include
LDFLAGS = -L$(HDFEOSHOME)/hdfeos/lib -L$(HDFHOME)/lib -L$(MPIHOME)/lib -lmpi -lhdfeos -lGctp -lmfhdf -ldf -ljpeg -lz 

FC = mpif90
 
SOURCES=global.f90 Julday2month.f90 read_2cice.f90 read_rain.f90 read_aerosol.f90 read_ecmwf.f90 read_cloudsat.f90 core_simulate.f90 ice_cf.f90 sun_info.f90 read_output.f90 read_albedo_spectral.f90 write_hdf.f90 inquire_info.f90
OBJECTS=$(SOURCES:.f90=.f90.o)

all: ice_cf

clean:
	rm -f *.o ice_cf

ice_cf: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $@ $(LDFLAGS)

%.f90.o:
	$(FC) $(FFLAGS) -c $(@:.o=) -o $@
