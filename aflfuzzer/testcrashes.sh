for f in `ls findings/crashes/id* | xargs `
do
  cat $f | ./a.out
  echo $f $?
done
