for file in $(find -name project.clj)
do
    sed -i 's/(def version ".*")/(def version "'$1'")/g' $file
done