rm checkouts/*
rmdir checkouts

mkdir checkouts

ln -s ../undertaker checkouts/undertaker
ln -s ../undertaker-junit checkouts/undertaker-junit

rm undertaker-junit/checkouts/*
rmdir undertaker-junit/checkouts

mkdir undertaker-junit/checkouts

ln -s ../../undertaker undertaker-junit/checkouts/undertaker

cd undertaker
lein install

cd ../undertaker-junit
lein install