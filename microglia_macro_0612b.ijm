run("Set Scale...", "distance=1.0800 known=100 unit=μm");

setAutoThreshold("Default no-reset");
//run("Threshold...");
setThreshold(0, 254, "raw");
run("Create Selection");


run("ROI Manager...");
roiManager("Add");
roiManager("Show All");

setThreshold(0, 90, "raw");
setOption("BlackBackground", false);
run("Convert to Mask");

roiManager("Select", 0);

run("Analyze Particles...", "clear summarize");

path = getDirectory("image");
title = getTitle();
p = "_processed";
saveAs("Jpeg", path+title+p);