dir1=dns
dir2=NEW/$dir1
for i in `ls $dir1/*.*`; do
# echo $i
 j=`echo $i | sed "s/$dir1//g"`
# echo $dir2/$j
 diff $i $dir2/$j > $i.diff
done 
for i in `ls $dir1/*.diff` ; do
 size=`stat -c %s $i`
 if [ $size -lt 1 ]; then
  rm -f $i
 fi
done
