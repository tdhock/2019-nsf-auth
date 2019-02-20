figure-histogram-features-labels.png: figure-histogram-features-labels.R
	R --vanilla < $<
figure-histogram-features/index.html: figure-histogram-features.R
	R --vanilla < $<
