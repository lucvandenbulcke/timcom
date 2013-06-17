ICODEDIR="$ROOTDIR/src/I"

icode_generate() {
  local i
  MYDIR=$PWD
  if [ -d "$ICODEDIR" ]
  then
    cd $ICODEDIR
    for i in `ls *.f90`
    do
      cp $i $MYDIR/ 
      echo "$i generate done"
    done
    cd $MYDIR
  else
    echo "$ICODEDIR directory does NOT exist!"
  fi
}
