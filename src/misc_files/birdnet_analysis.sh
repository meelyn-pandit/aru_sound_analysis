for f in heatburst_audio/*; do
#python analyze.py --i $f; --lat 35.075 --lon -99.835
python analyze.py --i $f --lat 35.075 --lon -99.835
done
