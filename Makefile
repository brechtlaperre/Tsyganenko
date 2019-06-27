.PHONY: all clean run_ensemble test

PYTHON=python3


# Parameters
COLUMN='B0z'
MU=0
SIGMA=0.05
ENSEMBLE_SIZE=100

DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py $(COLUMN) $(MU) $(SIGMA) $(ENSEMBLE_SIZE)

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/TA15/TA_input_old
	mv $< $@

model/TA15/output:
	mkdir model/TA15/output


run_ensemble: model/TA15/TA15_input model/TA15/output
	cd model/TA15; ./run.sh


test:
	echo 'Test' $(this)

clean:
	rm DA/input/TA15_input
	

