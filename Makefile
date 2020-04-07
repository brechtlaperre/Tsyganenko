.PHONY: all clean representer
.ONESHELL: compile runmodel ensemble

PYTHON=python3

# Parameters
MODEL='T96'
INPUT='input1'
IMAGEID='2004_04_09'
FOLDER='report_januari/$(MODEL)/'
COLUMN='SWX'
MU=5 #7
SIGMA=2
ENSEMBLE_SIZE=30
SIGN=True

# Coords. Either less then zero for percentages, or values in x/R_e
X1= 15 #0.5 #.95 # horizontal position
Z1= 20 #0.583 #.8 # vertical position

X2= -10 #.917
Z2= 20 #.999

X3= -10 #.333
Z3= 5 #.417

X4= -20 #.5
Z4= -3 #.833

src/DA/input/TA15_input:
	$(PYTHON) DA/generate_input.py --sign $(SIGN) $(COLUMN) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) 

model/TA15/TA15_input: DA/input/TA15_input
	mv $@ model/$(MODEL)/TA_input_old
	mv $< $@

output_folder:
	mkdir model/$(MODEL)/output

new_ensemble: model/TA15/TA15_input output_folder compile run

ensemble: compile run

compile:
	(cd 'model/$(MODEL)'; make compile)

run:
	(cd 'model/$(MODEL)'; ./$(MODEL).e $(INPUT) )

representer:
	$(PYTHON) src/DA/domain.py model/$(MODEL)/output $(COLUMN)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --identifier $(IMAGEID) --folder $(FOLDER)

clean:
	rm DA/input/TA15_input

movies/$(MODEL)/$(INPUT):
	mkdir movies/$(MODEL)/$(INPUT)

movie:
	$(PYTHON) src/visualize/generate_images.py $(INPUT) $(MODEL)
	ffmpeg -r 1 -i movies/$(MODEL)/$(INPUT)/frame%02d.png -vcodec mpeg4 -y movies/$(MODEL)/$(INPUT)/movie.mp4
	
	
all: ensemble representer
