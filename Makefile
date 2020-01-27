.PHONY: all clean ensemble representer
.ONESHELL: compile runmodel

PYTHON=python3

# Parameters
MODEL='T89'
COLUMN='SWX'
MU=5 #7
SIGMA=2
ENSEMBLE_SIZE=30
SIGN=True
IMAGEID='2004_04_09'
FOLDER='report_januari/$(MODEL)/'

# Coords if looked from x-y plane with range x \in [0, 1], z \in [0,1]
X1=0.5 #.95 # horizontal position
Z1=0.583 #.8 # vertical position

X2=.917
Z2=.999

X3=.333
Z3=.417

X4=.5
Z4=.833

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py --sign $(SIGN) $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) 

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/$(MODEL)/TA_input_old
	mv $< $@

output_folder:
	mkdir model/$(MODEL)/output

new_ensemble: model/TA15/TA15_input output_folder compile run

ensemble: output_folder compile run

compile:
	(cd 'model/$(MODEL)'; make compile)

run:
	(cd 'model/$(MODEL)'; make run)

representer: 
	$(PYTHON) DA/domain.py model/$(MODEL)/output $(COLUMN)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --identifier $(IMAGEID) --folder $(FOLDER)

clean:
	rm DA/input/TA15_input
	
all: ensemble representer
