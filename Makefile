.PHONY: all clean ensemble representer

PYTHON=python3

# Parameters
COLUMN='B0z' 'PDYN'
MU=0
SIGMA=3
ENSEMBLE_SIZE=30

X1=98
Y1=126

X2=100
Y2=80

X3=74
Y3=185

X4=130
Y4=43

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE)

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/TA15/TA_input_old
	mv $< $@

model/TA15/output:
	mkdir model/TA15/output

ensemble: model/TA15/TA15_input model/TA15/output
	cd model/TA15; ./run.sh

representer: model/TA15/output
	$(PYTHON) DA/domain.py $< $(COLUMN)  $(X1) $(Y1) --extra $(X2) $(Y2) --extra $(X3) $(Y3) --extra $(X4) $(Y4)

clean:
	rm DA/input/TA15_input
	
all:ensemble representer
