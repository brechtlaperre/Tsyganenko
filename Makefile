.PHONY: all clean ensemble representer

PYTHON=python3


# Parameters
COLUMN='B0y'
MU=0
SIGMA=3
ENSEMBLE_SIZE=30

X1=96
Y1=126

X2=100
Y2=180

X3=63
Y3=185

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py -- $(COLUMN) $(MU) $(SIGMA) $(ENSEMBLE_SIZE)

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/TA15/TA_input_old
	mv $< $@

model/TA15/output:
	mkdir model/TA15/output


ensemble: model/TA15/TA15_input model/TA15/output
	cd model/TA15; ./run.sh


representer: model/TA15/output
	$(PYTHON) DA/domain.py $< $(X1) $(Y1)
	$(PYTHON) DA/domain.py $< $(X2) $(Y2)
	$(PYTHON) DA/domain.py $< $(X3) $(Y3)


clean:
	rm DA/input/TA15_input
	
all: clean ensemble representer
