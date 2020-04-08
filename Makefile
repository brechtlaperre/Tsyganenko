.PHONY: all clean representer
.ONESHELL: compile runmodel ensemble

PYTHON=python3

# Parameters
MODEL='TA15'
INPUT='input_Vx'
IMAGEID='2004_04_09'
FOLDER='report_januari/$(MODEL)/'
COLUMN='Vx'
MU=1 #7
SIGMA=0.1
ENSEMBLE_SIZE=30
SIGN=False

# Coords. Either less then zero for percentages, or values in x/R_e
X1= 15 #0.5 #.95 # horizontal position
Z1= 20 #0.583 #.8 # vertical position

X2= -10 #.917
Z2= 20 #.999

X3= -10 #.333
Z3= 5 #.417

X4= -20 #.5
Z4= -3 #.833

timestamp:
	$(PYTHON) src/data/generate_timestamp_input.py --sign $(SIGN) $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) 

timeseries:
	$(PYTHON) src/data/generate_timeseries_input.py 

ensemble: compile run

compile:
	(cd 'model/$(MODEL)'; make compile)

run:
	(cd 'model/$(MODEL)'; ./$(MODEL).e $(INPUT) )

representer:
	$(PYTHON) src/DA/domain.py model/$(MODEL)/output $(COLUMN)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --identifier $(IMAGEID) --folder $(FOLDER)

clean:
	rm DA/input/TA15_input

movie:
	$(PYTHON) src/visualize/generate_images.py $(INPUT) $(MODEL)
	ffmpeg -r 1 -i movies/$(MODEL)/$(INPUT)/frame%02d.png -vcodec mpeg4 -y movies/$(MODEL)/$(INPUT)/movie.mp4
	
all: ensemble representer
