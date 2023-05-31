
# Update live_experiments on local machine
sudo cp -r "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/memorygrid" "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/live_experiments/"
sudo find "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/live_experiments/memorygrid" -type f -iname \*.js -delete
sudo javascript-obfuscator "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/memorygrid" --output "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/live_experiments/memorygrid" --options-preset "low-obfuscation"


# Run memorygrid on local machine
sudo cp -r "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/memorygrid" /var/www/live_experiments/


# Run live_experiments on local machine
sudo cp -r "/media/beel/DATA/___UNI___/Semester 4/Laborpraktikum/code/experiments/live_experiments/memorygrid" /var/www/live_experiments/

# just adding a test line for testing push