.PHONY: all clean ensemble representer
.ONESHELL: compile runmodel

PYTHON=python3

# Parameters
MODEL='TA15'
COLUMN='B0z' 'PDYN'
MU=5 #7
SIGMA=2
ENSEMBLE_SIZE=30
SIGN=True
IMAGEID='Bz_PDyn_pos_standard'


# Coords if looked from x-y plane with range x \in [0, 1], y \in [0,1]
X1=.95 # horizontal position
Y1=.8 # vertical position

X2=.95
Y2=.45

X3=.9
Y3=.09 

X4=.44
Y4=.5

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py --sign $(SIGN) $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) 

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/$(MODEL)/TA_input_old
	mv $< $@

output_folder:
	mkdir model/$(MODEL)/output

ensemble: model/TA15/TA15_input output_folder compile runmodel
	
compile:
	$(MAKE) -C 'model/$(MODEL)'

run:
	$(MAKE) -C 'model/$(MODEL)'

representer: model/TA15/output
	$(PYTHON) DA/domain.py $< $(COLUMN)  $(X1) $(Y1) --extra $(X2) $(Y2) --extra $(X3) $(Y3) --extra $(X4) $(Y4) --identifier $(IMAGEID)

clean:
	rm DA/input/TA15_input
	
all: ensemble representer
