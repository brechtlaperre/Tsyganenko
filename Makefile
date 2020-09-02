.PHONY: all clean representer
.ONESHELL: compile runmodel ensemble

PYTHON := python3

# Parameters
MODEL=T96
FEATURE='Vx'
NAME:='Vx4'
INPUT='input'$(NAME)
OUTPUT='output'$(NAME)
IMAGEID='2004_04_09'
FOLDER='report_januari/$(MODEL)/'
MU=1 #7
SIGMA=0.1
ENSEMBLE_SIZE=50
SIGN=False

# Coords. Either less then zero for percentages, or values in x/R_e
X1:=15#0.5 #.95 # horizontal position
Z1:=20#0.583 #.8 # vertical position

X2:=-10#.917
Z2:=19#.999

X3:=-10#.333
Z3:=5#.417

X4:=-20#.5
Z4:=-3#.833

X5:=8
Z5:=-4

timestamp:
	$(PYTHON) src/data/generate_timestamp_input.py --sign $(SIGN) $(FEATURE) -- $(MU) $(SIGMA) $(ENSEMBLE_SIZE) $(FEATURE)

timeseries:
	$(PYTHON) src/data/generate_timeseries_input.py 

ensemble: compile folder run

compile:
	(cd 'model/$(MODEL)'; make compile)

run: 
	(cd 'model/$(MODEL)'; ./$(MODEL).e $(INPUT) $(OUTPUT) )


representer: folder
	$(PYTHON) src/DA/domain.py model/$(MODEL)/$(OUTPUT) $(FEATURE)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --identifier $(IMAGEID) --folder results/$(MODEL)/$(OUTPUT)/ --model $(MODEL) --datafile model/input/$(INPUT).csv

model/$(MODEL)/$(OUTPUT): SHELL:=/bin/bash
model/$(MODEL)/$(OUTPUT):
	@if [ ! -d $@ ]; then mkdir $@; fi; 

folder: SHELL:=/bin/bash
folder: model/$(MODEL)/$(OUTPUT) results/$(MODEL)/$(OUTPUT)

results/$(MODEL)/$(OUTPUT):: SHELL :=/bin/bash
results/$(MODEL)/$(OUTPUT):
	@if [ ! -d $@ ]; then mkdir $@; fi;

clean:
	rm DA/input/TA15_input

movie:
	$(PYTHON) src/visualize/generate_images.py $(INPUT) $(MODEL) $(OUTPUT)
	ffmpeg -r 1 -i movies/$(MODEL)/$(INPUT)/frame%02d.png -vcodec mpeg4 -y movies/$(MODEL)/$(INPUT)/movie.mp4
	
FIRST := 5
LAST := 6
NUMBERS := $(shell seq $(FIRST) $(LAST))
JOBS := $(addprefix Vx,${NUMBERS})
IMAGES := $(addprefix Vx,${NUMBERS})
.PHONY: all ${JOBS}
RESFOLDER = results2
jobs: compile ${JOBS} ; echo "$@ success"
${JOBS}:
	$(eval OUTPUT = output2'$@')
	$(eval INPUT = input'$@')
	$(shell @if [ ! -d model/$(MODEL)/$(OUTPUT) ]; then mkdir model/$(MODEL)/$(OUTPUT); fi;)
	$(shell @if [ ! -d $(RESFOLDER)/$(MODEL)/$(OUTPUT) ]; then mkdir $(RESFOLDER)/$(MODEL)/$(OUTPUT); fi;)
	#(cd 'model/$(MODEL)'; ./$(MODEL).e $(INPUT) $(OUTPUT) )
	$(PYTHON) src/DA/domain.py model/$(MODEL)/$(OUTPUT) $(FEATURE)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --extra $(X5) $(Z5) --identifier $(IMAGEID) --folder $(RESFOLDER)/$(MODEL)/$(OUTPUT)/ --model $(MODEL) --datafile model/input/$(INPUT).csv
	
vxmovie:
	(cd results/$(MODEL); ls outputVx*/$(MODEL)_Vx_'$(X1)'_$(Z1)_t+*.png | sort -V | xargs -I {} echo "file '{}'" > vxfiles1.txt)
	(cd results/$(MODEL); ls outputVx*/$(MODEL)_Vx_$(X2)_$(Z2)_t+*.png | sort -V | xargs -I {} echo "file '{}'" > vxfiles2.txt)
	(cd results/$(MODEL); ls outputVx*/$(MODEL)_Vx_$(X3)_$(Z3)_t+*.png | sort -V | xargs -I {} echo "file '{}'" > vxfiles3.txt)
	(cd results/$(MODEL); ls outputVx*/$(MODEL)_Vx_$(X4)_$(Z4)_t+*.png | sort -V | xargs -I {} echo "file '{}'" > vxfiles4.txt)
	ffmpeg -safe 0 -r 1 -f concat -i results/$(MODEL)/vxfiles1.txt -vcodec mpeg4 -y results/$(MODEL)/$(MODEL)_vxmovie1.mp4
	ffmpeg -safe 0 -r 1 -f concat -i results/$(MODEL)/vxfiles2.txt -vcodec mpeg4 -y results/$(MODEL)/$(MODEL)_vxmovie2.mp4
	ffmpeg -safe 0 -r 1 -f concat -i results/$(MODEL)/vxfiles3.txt -vcodec mpeg4 -y results/$(MODEL)/$(MODEL)_vxmovie3.mp4
	ffmpeg -safe 0 -r 1 -f concat -i results/$(MODEL)/vxfiles4.txt -vcodec mpeg4 -y results/$(MODEL)/$(MODEL)_vxmovie4.mp4

test:
	$(PYTHON) src/DA/domain.py model/$(MODEL)/$(OUTPUT) $(FEATURE)  $(X1) $(Z1) --extra $(X2) $(Z2) --extra $(X3) $(Z3) --extra $(X4) $(Z4) --identifier $(IMAGEID) --folder results/$(MODEL)/$(OUTPUT)/ --model $(MODEL) --datafile model/input/$(INPUT).csv

reference: compile
	$(eval OUTPUT = outputref2)
	$(eval INPUT = input_ref)
	$(shell @if [ ! -d model/$(MODEL)/$(OUTPUT) ]; then mkdir model/$(MODEL)/$(OUTPUT); fi;)
	$(shell @if [ ! -d results2/$(MODEL)/$(OUTPUT) ]; then mkdir results2/$(MODEL)/$(OUTPUT); fi;)
	(cd 'model/$(MODEL)'; ./$(MODEL).e $(INPUT) $(OUTPUT) )
	$(PYTHON) src/visualize/generate_images.py $(INPUT) $(MODEL) $(OUTPUT) results2
	#ffmpeg -r 1 -i results/$(MODEL)/$(OUTPUT)/frame%02d.png -vcodec mpeg4 -y results/$(MODEL)/$(OUTPUT)/movie.mp4