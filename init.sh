rm checkouts/*
rmdir checkouts

mkdir checkouts

ln -s ../undertaker checkouts/undertaker
ln -s ../undertaker-junit checkouts/undertaker-junit

cd undertaker
lein install

cd ../undertaker-junit
lein install