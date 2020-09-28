# get changed files
git diff --name-status  master:man/figures..v2:man/figures > man/figures/DAILY.CHANGES

# remove files
for i in `cat DAILY.CHANGES | awk '{print $2}'`; do echo $i; rm -v $i; done

#for i in `cat DAILY.CHANGES | awk '{print $2}'`; do echo $i; git merge master $i; done

# recover daily commits
for i in `cat DAILY.CHANGES | awk '{print $2}'`; do echo $i; git checkout  master $i; done
