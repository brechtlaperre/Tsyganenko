.PHONY: all clean ensemble representer
.ONESHELL: compile runmodel

PYTHON=python3

# Parameters
COLUMN='VGSEX'
MU=-400
SIGMA=10
ENSEMBLE_SIZE=30
SIGN=False
IMAGEID=extr_pos_B

#X1=98
#Y1=126

X1=.8
Y1=.8 

X2=.7
Y2=.5
#X2=100
#Y2=80

X3=.4
Y3=.9 

#X3=74
#Y3=185

X4=.5
Y4=.5

#X4=130
#Y4=43

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py --sign $(SIGN) $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) 

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/TA15/TA_input_old
	mv $< $@

model/TA15/output:
	mkdir model/TA15/output

compile:
	cd model/TA15
	gfortran Geopack-2008.for TA_2015_B.for MAIN_TA15.for -o TA15.e

ensemble: model/TA15/TA15_input model/TA15/output compile runmodel
	
runmodel:
	cd model/TA15
	./TA15.e

representer: model/TA15/output_n
	$(PYTHON) DA/domain.py $< $(COLUMN)  $(X1) $(Y1) --extra $(X2) $(Y2) --extra $(X3) $(Y3) --extra $(X4) $(Y4) --identifier $(IMAGEID)

clean:
	rm DA/input/TA15_input
	
all:ensemble representer
